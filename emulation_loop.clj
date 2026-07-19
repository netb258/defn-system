(ns z80.emulation-loop
  (:require [z80.memory :as memory]
            [z80.display :as display]
            [quil.core :as q]))

;; --------------------------------------------------------------------------------------------------
;; --------------------------------------- Z80 Instruction Loop -------------------------------------
;; --------------------------------------------------------------------------------------------------

(defn- vblank-irq-enabled?
  "Checks the VDP's internal Register 1 state to see if the Sega Master System 
   hardware has enabled V-Blank Frame Interrupt requests."
  [^z80.vdp.VdpState vdp]
  (let [vdp-regs ^ints (:regs vdp)
        reg1 (if (> (alength vdp-regs) 1) (aget vdp-regs 1) 0)]
    ;; Bit 5 of VDP Register 1 enables the Frame Interrupt (V-Blank IRQ)
    (not= 0 (bit-and reg1 0x20))))

(defn- hblank-irq-enabled?
  "Checks Bit 4 of VDP Register 0 to see if Line Interrupts (H-Blank IRQs) are enabled."
  [^z80.vdp.VdpState vdp]
  (let [vdp-regs ^ints (:regs vdp)
        reg0 (if (> (alength vdp-regs) 0) (aget vdp-regs 0) 0)]
    (not= 0 (bit-and reg0 0x10))))

(defn- get-vdp-reg10
  "Retrieves the value of VDP Register 10 (Scanline target)."
  [^z80.vdp.VdpState vdp]
  (let [vdp-regs ^ints (:regs vdp)]
    (if (> (alength vdp-regs) 10) (aget vdp-regs 10) 0)))

;; Persistent frame canvas initialized matching standard PAL dimensions 
;; Every frame will be drawn here.
;; We will initialize this in the quil 'setup' function like so:
;; (reset! global-frame-buffer (q/create-image 256 224 :rgb))
(def ^:private global-frame-buffer (atom nil))

(defn- do-instruction-loop!
  "Executes a single PAL frame scanline-by-scanline (313 lines total).
  Runs the CPU instructions and draws the graphics."
  [^com.codingrodent.microprocessor.Z80.Z80Core cpu ^z80.vdp.VdpState vdp]
  (let [;; PAL Sega Master System metrics: 313 total scanlines per frame (0 to 312).
        lines-per-frame 313
        ;; Every scanline lasts exactly 228 CPU T-states (cycles). 
        ;; Total frame footprint = 313 lines * 228 cycles = 71,364 cycles per frame (~50Hz).
        cycles-per-line 228
        ;; The VDP's register 10 holds a counter that is crucial the the timing of the H-BLANK.
        ;; The Master System triggers an H-BLANK interrupt only if this counter rolls over below zero.
        line-interrupt-counter (atom (get-vdp-reg10 @vdp))
        ;; Extract the raw PImage canvas object out of the atom container once per frame
        frame-canvas ^processing.core.PImage @global-frame-buffer]

    ;; Open the direct 1D primitive pixel array for unchecked mutations
    (.loadPixels frame-canvas)

    (dotimes [scanline lines-per-frame]
      (let [start-tstates (.getTStates cpu)
            target-tstates (+ start-tstates cycles-per-line)]

        ;; Keep VDP state synchronized with the current hardware horizontal raster layer
        (swap! vdp assoc :current-scan-line scanline)

        ;; 1. PROCESS Z80 CPU INSTRUCTIONS FOR THIS SCANLINE
        ;; Step the Z80 processor repeatedly until it consumes exactly 228 cycle T-states.
        (loop []
          (when (< (.getTStates cpu) target-tstates)
            (.executeOneInstruction cpu)
            (recur)))

        ;; 2. RUN LINE RENDERING FUNCTIONS
        ;; Only render within the standard 224-line limit.
        (when (< scanline 224)
          (let [current-vdp @vdp
                vdp-regs    ^ints (:regs current-vdp)
                ;; Bit 3 of VDP Register 1 controls standard 192-line mode vs extended 224-line mode
                reg1        (int (if (and vdp-regs (>= (alength vdp-regs) 2)) (aget vdp-regs 1) 0))
                mode-224?   (not= 0 (bit-and reg1 0x08))
                active-limit (if mode-224? 224 192)]
            ;; ALWAYS draw the background line. This ensures that when the system is in 192-line mode,
            ;; lines 192 to 223 automatically drop into the overscan loop to draw a clean uniform border.
            (display/draw-background-line! current-vdp frame-canvas scanline)
            ;; ONLY compute foreground sprites and run collision grid checks during active video display lines
            (when (< scanline active-limit)
              (display/draw-all-sprites-line-for-scanline! frame-canvas current-vdp scanline mode-224?))))

        ;; 3. HANDLE H-BLANK
        ;; The VDP line counter decrements on every active scanline.
        (if (<= scanline 192)
          (let [current-count @line-interrupt-counter
                new-count (dec current-count)]
            ;; When the counter underflows below 0, reset it from VDP Register 10 and request a CPU interrupt.
            (if (< new-count 0)
              (do
                ;; Counter underflowed! Reload from VDP Register 10
                (reset! line-interrupt-counter (get-vdp-reg10 @vdp))
                ;; Trigger CPU Interrupt if the game requested H-Blank IRQs
                ;; We have just processed a whole single scan-line with the loop above.
                ;; So, we can trigger an interrupt to let the game know there is a short time
                ;; before we snap back and process another scan-line.
                (when (hblank-irq-enabled? @vdp)
                  (.setInterrupt cpu true)))
              ;; Decrement counter normally
              (reset! line-interrupt-counter new-count)))
          ;; Outside the active window, the counter continually reloads from Register 10
          (reset! line-interrupt-counter (get-vdp-reg10 @vdp)))

        ;; 4. HANDLE V-BLANK
        ;; Trigger a VBlank on the last visible scanline, so that games have time to update
        ;; before we go back to scanline 1 and start processing a new frame. This will be a significant pause.
        ;; This is the longest most crucial synchronization event.
        ;; During V-BLANK the VRAM is fully accessible without disrupting the display.
        ;; Games use this window to: Update sprite positions (moving characters, enemies, projectiles),
        ;; Load new tile graphics into VDP memory and more.
        (when (= scanline 193)
          (swap! vdp assoc :vblank-active? true)
          (when (vblank-irq-enabled? @vdp)
            (.setInterrupt cpu true)))

        ;; 5. END OF FRAME CLEANUP
        ;; On the very last scanline of the PAL cycle loop, sanitize the image buffer pixels.
        (when (= scanline 312)
          (swap! vdp assoc :vblank-active? false)
          (let [pixels-arr ^ints (.pixels frame-canvas)]
            (dotimes [i (alength pixels-arr)]
              ;; - (bit-and ... 0x00FFFFFF) completely strips out our internal layer sorting metadata mask.
              ;; - (bit-or 0xFF000000 ...) sets Alpha back to 0xFF (100% opaque) so Quil doesn't render it as black.
              ;; - Wrapped in unchecked-int to suppress Clojure's signed integer overflow arithmetic exceptions.
              (aset pixels-arr i (unchecked-int (bit-or 0xFF000000 (bit-and (aget pixels-arr i) 0x00FFFFFF)))))))))
    ;; Finalize mutations and push the primitive pixel array modifications back into the Quil canvas
    (.updatePixels frame-canvas)))

;; --------------------------------------------------------------------------------------------------
;; ------------------------------------------- Quil Setup -------------------------------------------
;; --------------------------------------------------------------------------------------------------

;; This function will prepare everything needed for the instruction loop to run.
(defn make-setup-function [^com.codingrodent.microprocessor.Z80.Z80Core cpu]
  (fn []
    ;; This should be the accurate FPS for a PAL console.
    (q/frame-rate 50)
    (reset! global-frame-buffer (q/create-image 256 224 :rgb))
    (memory/reset-emulator cpu)
    (println "Sega Master System initialized with Sega Mapper support. Running real-time cycle loop...")))

(defn set-nearest-neighbor!
  "This function forces Nearest Neighbor sampling on all images.
  That should disable any antialiasing / filtering or texture smoothing.
  We are doing this, because Quil automatically applies Bilinear filtering on all upscaed images."
  []
  (.textureSampling (q/current-graphics) 2))

;; This function will call the instruction loop 50 times a second:
(defn make-draw-function [^com.codingrodent.microprocessor.Z80.Z80Core cpu ^z80.vdp.VdpState vdp]
  (fn []
    ;; 1. Execute Z80 code line-by-line while filling 'global-frame-buffer'
    (do-instruction-loop! cpu vdp)
    ;; 2. Force Nearest Neighbor sampling to preserve retro pixel art sharpness
    (set-nearest-neighbor!)
    ;; 3. Paint the fully constructed frame directly from the buffer
    (q/image @global-frame-buffer 0 0 display/screen-width display/screen-height)))
