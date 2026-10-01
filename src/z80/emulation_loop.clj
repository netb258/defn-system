(ns z80.emulation-loop
  (:require [z80.memory :as memory]
            [z80.display :as display]
            [quil.core :as q]))

;; --------------------------------------------------------------------------------------------------
;; --------------------------------------- Z80 Instruction Loop -------------------------------------
;; --------------------------------------------------------------------------------------------------

;; This will serve as our frame canvas. Every frame will be drawn here.
;; We will initialize this in the quil 'setup' function like so:
;; (reset! global-frame-buffer (q/create-image 256 224 :rgb))
(def ^:private global-frame-buffer (atom nil))

(defn- vblank-irq-enabled?
  "Checks the VDP's internal Register 1 state to see if the Sega Master System 
  hardware has enabled V-Blank Frame Interrupt requests."
  [^z80.vdp.VdpState vdp]
  (let [vdp-regs ^ints (:regs vdp)
        reg1 (aget vdp-regs 1)]
    ;; Bit 5 of VDP Register 1 enables the Frame Interrupt (V-Blank IRQ)
    ;; Remember the bits are counted from right to left starting at bit 0.
    (not= 0 (bit-and reg1 2r00100000))))

(defn- hblank-irq-enabled?
  "Checks Bit 4 of VDP Register 0 to see if Line Interrupts (H-Blank IRQs) are enabled.
  The bits are counted from right to left starting at bit 0."
  [^z80.vdp.VdpState vdp]
  (let [vdp-regs ^ints (:regs vdp)
        reg0 (aget vdp-regs 0)]
    (not= 0 (bit-and reg0 2r00010000))))

(defn- get-vdp-reg10
  "Retrieves the value of VDP Register 10 (Scanline target)."
  [^z80.vdp.VdpState vdp]
  (let [vdp-regs ^ints (:regs vdp)]
    (aget vdp-regs 10)))

;; NOTE: The method executeOneInstruction has a line like this: instruction = memory.readByte(reg_PC).
;; Since the program counter starts out at 0x0000, we should be starting out by reading a single byte form the ROM.
(defn- do-instruction-loop!
  "Executes a single PAL frame scanline-by-scanline (313 lines total).
  Runs the CPU instructions and draws the graphics."
  [^com.codingrodent.microprocessor.Z80.Z80Core cpu vdp-atom]
  (let [;; PAL Sega Master System metrics: 313 total scanlines per frame (0 to 312).
        lines-per-frame 313
        ;; Every scanline lasts exactly 228 CPU T-states (cycles). 
        ;; Total frame footprint = 313 lines * 228 cycles = 71,364 cycles per frame (~50Hz).
        cycles-per-line 228
        ;; The VDP's register 10 holds a counter that is crucial the the timing of the H-BLANK.
        ;; The Master System triggers an H-BLANK interrupt only if this counter rolls over below zero.
        line-interrupt-counter (atom (get-vdp-reg10 @vdp-atom))
        ;; Extract the raw PImage canvas object out of the atom container once per frame
        frame-canvas ^processing.core.PImage @global-frame-buffer]

    ;; Open the direct 1D primitive pixel array for unchecked mutations
    (.loadPixels frame-canvas)

    (dotimes [scanline lines-per-frame]
      (let [start-tstates (.getTStates cpu)
            target-tstates (+ start-tstates cycles-per-line)]

        ;; Keep VDP state synchronized with the current horizontal scan-line.
        (swap! vdp-atom assoc :current-scan-line scanline)

        ;; Contrary to intuition, the interrupts need to be fired before executing the CPU instructions.
        (let [vdp-state @vdp-atom
              vblank-asserted? (and (:vblank-active? vdp-state) (vblank-irq-enabled? vdp-state))
              hblank-asserted? (and (:hblank-active? vdp-state) (hblank-irq-enabled? vdp-state))]
          (if (or vblank-asserted? hblank-asserted?)
            (.setInterrupt cpu true)
            (.setInterrupt cpu false)))

        ;; 1. PROCESS Z80 CPU INSTRUCTIONS FOR THIS SCANLINE
        ;; Step the Z80 processor repeatedly until it consumes exactly 228 cycle T-states.
        (loop []
          (when (< (.getTStates cpu) target-tstates)
            (.executeOneInstruction cpu)
            (recur)))

        ;; 2. RUN LINE RENDERING FUNCTIONS
        ;; Only render within the standard 224-line limit.
        (when (< scanline 224)
          (let [vdp-regs    ^ints (:regs @vdp-atom)
                ;; Bit 3 of VDP Register 1 controls standard 192-line mode vs extended 224-line mode
                reg1        (int (aget vdp-regs 1))
                mode-224?   (not= 0 (bit-and reg1 2r00001000))
                active-limit (if mode-224? 224 192)]
            ;; ALWAYS draw the background line. This ensures that when the system is in 192-line mode,
            ;; lines 192 to 223 automatically drop into the overscan loop to draw a clean uniform border.
            (display/draw-background-line! @vdp-atom frame-canvas scanline)
            ;; ONLY compute foreground sprites and run collision grid checks during active video display lines
            (when (< scanline active-limit)
              (display/draw-all-sprites-line-for-scanline! frame-canvas vdp-atom scanline mode-224?))))

        ;; 3. HANDLE H-BLANK
        ;; The VDP line counter decrements on every active scanline.
        (if (<= scanline 192)
          (let [current-count @line-interrupt-counter
                new-count (dec current-count)]
            ;; When the counter underflows below 0, reset it from VDP Register 10 and request a CPU interrupt.
            (if (< new-count 0)
              (do
                ;; Counter underflowed! Reload from VDP Register 10
                (reset! line-interrupt-counter (get-vdp-reg10 @vdp-atom))
                ;; Trigger CPU Interrupt if the game requested H-Blank IRQs
                ;; We have just processed a whole single scan-line with the loop above.
                ;; So, we can trigger an interrupt to let the game know there is a short time
                ;; before we snap back and process another scan-line.
                (swap! vdp-atom assoc :hblank-active? true))
              ;; Decrement counter normally
              (reset! line-interrupt-counter new-count)))
          ;; Outside the active window, the counter continually reloads from Register 10
          (reset! line-interrupt-counter (get-vdp-reg10 @vdp-atom)))

        ;; 4. HANDLE V-BLANK
        ;; Trigger a VBlank on the last visible scanline, so that games have time to update
        ;; before we go back to scanline 1 and start processing a new frame. This will be a significant pause.
        ;; This is the longest most crucial synchronization event.
        ;; During V-BLANK the VRAM is fully accessible without disrupting the display.
        ;; Games use this window to: Update sprite positions (moving characters, enemies, projectiles),
        ;; Load new tile graphics into VDP memory and more.
        (when (= scanline 193)
          (swap! vdp-atom assoc :vblank-active? true))

        ;; 5. END OF FRAME
        ;; This is the very last scanline of the PAL cycle loop.
        (when (= scanline 312)
          (swap! vdp-atom assoc :vblank-active? false))))
    ;; This is where the finalized frame is rendered.
    (.updatePixels frame-canvas)))

;; --------------------------------------------------------------------------------------------------
;; ------------------------------------------- Quil Setup -------------------------------------------
;; --------------------------------------------------------------------------------------------------

;; This function will prepare everything needed for the instruction loop to run.
(defn make-setup-function []
  (fn []
    ;; Technically we are emulating a PAL Master System and the FPS should be 50.
    ;; However, all Game Gear consoles run at 60 FPS no matter the region.
    ;; Setting the FPS to 60 covers both consoles.
    (q/frame-rate 60)
    (reset! global-frame-buffer (q/create-image 256 224 :rgb))))

(defn set-nearest-neighbor!
  "This function forces Nearest Neighbor sampling on all images.
  That should disable any antialiasing / filtering or texture smoothing.
  We are doing this, because Quil automatically applies Bilinear filtering on all upscaed images."
  []
  (.textureSampling (q/current-graphics) 2))

;; This function will call the instruction loop 50 times a second:
(defn make-draw-function [cpu-atom vdp-atom]
  (fn []
    ;; 1. Execute Z80 code line-by-line while filling 'global-frame-buffer'
    (do-instruction-loop! @cpu-atom vdp-atom)
    ;; 2. Force Nearest Neighbor sampling. I want the image blocky.
    (set-nearest-neighbor!)
    ;; 3. If we are dealing with a SMS rom, we should paint the frame directly from the frame-buffer.
    ;; If we are dealing with a GG rom, we should crop the screen to 160x144 before doing the drawing.
    (let [screen-dimensions (display/get-screen-width-and-hieght)
          screen-width  (first screen-dimensions)
          screen-height (second screen-dimensions)
          ;; Extract the centered Game Gear 160x144 window (X: 48 to 207, Y: 24 to 167)
          ;; We will draw this limited area only if a Game Gear rom is selected.
          gg-viewport (.get ^processing.core.PImage @global-frame-buffer 48 24 160 144)]
      (if @memory/gg-rom-selected?
        (q/image gg-viewport 0 0 screen-width screen-height)
        (q/image @global-frame-buffer 0 0 screen-width screen-height)))))
