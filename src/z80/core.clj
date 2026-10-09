(ns z80.core
  (:require [z80.vdp :as vdp]
            [z80.memory :as memory]
            [z80.io-bus :as io-bus]
            [z80.joypads :as joypads]
            [z80.display :as display]
            [z80.emulation-loop :as emu-loop]
            [quil.core :as q])
  ;; NOTE: We are using this Java library to provide a Z80 CPU implementation.
  (:import [com.codingrodent.microprocessor.Z80 Z80Core])
  (:gen-class))

;; NOTE: A wonderful overview of the Sega Master System and all of it's components can be found here:
;; https://www.smspower.org/uploads/Development/JavaGear-Report.pdf

(def ^:private cpu (atom nil))
(def ^:private memory-bus (atom nil))
(def ^:private io-bus (atom nil))

(defn construct-cpu!
  "This function will tie all components together.
  It will construct a valid Z80Core CPU and pass it a Memory Bus that knows how to communicate between CPU/RAM/ROM.
  It will also pass it a valid IO-BUS that know how to communicate between CPU/VDP/JoyPads."
  [^z80.vdp.VdpState vdp]
  (let [memory-bus-instance (memory/make-memory-bus)
        io-bus-instance     (io-bus/make-io-bus cpu vdp)
        cpu-instance        (Z80Core. memory-bus-instance io-bus-instance)]
    (reset! memory-bus memory-bus-instance)
    (reset! io-bus io-bus-instance)
    ;; NOTE: We are initializing the SP with address 0xDFF0, because Shadow Dancer requires this on startup.
    (.setSP cpu-instance 0xDFF0)
    (reset! cpu cpu-instance)))

;; Once the above function is called, the Z80Core object should be hooked up to all other components.

;; After that we can do (memory/load-rom-into-memory! ROM-AS-BYTES).
;; Then the Z80Core object can access the loaded ROM at the first memory address 0x00.
;; After that, we can repeatedly call executeOneInstruction on the 'cpu'. 
;; The emulation_loop.clj module contains functions that call executeOneInstruction in a loop.

;; The method executeOneInstruction has lines like this:
;; instruction = memory.readByte(reg_PC)
;; decodeOneByteInstruction(instruction)
;; incPC();

;; So, when we first call executeOneInstruction, it will decode the Z80 opcode at address 0x00.
;; Then, since we are running it in a loop, it will continue to execute machine code from there.

;; --------------------------------------------------------------------------------------------------
;; ------------------------------------------ Main function -----------------------------------------
;; --------------------------------------------------------------------------------------------------

(defn -main [rom-path]
  (let [active-vdp (atom (vdp/create-vdp))]
    (when (clojure.string/ends-with? rom-path ".gg")
      (reset! memory/gg-rom-selected? true))
    (construct-cpu! active-vdp)
    (memory/load-rom-into-memory! (java.nio.file.Files/readAllBytes (java.nio.file.Paths/get rom-path (into-array String []))))
    (q/defsketch sms-screen
      :title "DeFn System"
      ;; NOTE: These two functions really kick off the emulation.
      ;; The setup/draw functions will start the Z80 instruction loop and draw the result to the screen.
      :setup (emu-loop/make-setup-function)
      :draw  (emu-loop/make-draw-function cpu active-vdp)
      ;; NOTE: The key-press-handler receives all components as arguments.
      ;; It needs to pass them to the save-states module, when certain keys are pressed.
      :key-pressed  (joypads/make-key-press-handler cpu active-vdp memory-bus io-bus)
      :key-released (joypads/make-key-release-handler)
      ;; This executes exactly once right as the Quil window closes
      ;; This is the proper way to save SRAM, especially for the Game Gear as some games use it to supplement
      ;; the system's 8KB of work RAM. For example Shinig Force Gaiden actively does this.
      ;; The game makes thousands of writes per second to SRAM, so constantly saving to disk is wasteful.
      :on-close (fn [] (memory/save-sram-to-disk!))
      :features [:exit-on-close]
      :renderer :opengl
      :size (display/get-screen-width-and-hieght))))
