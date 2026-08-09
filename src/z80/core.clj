(ns z80.core
  (:require [z80.vdp :as vdp]
            [z80.memory :as memory]
            [z80.io-bus :as io-bus]
            [z80.joypads :as joypads]
            [z80.display :as display]
            [z80.emulation-loop :as emu-loop]
            [quil.core :as q])
  (:import [com.codingrodent.microprocessor.Z80 Z80Core])
  (:gen-class))

;; NOTE: A wonderful overview of the Sega Master System and all of it's components can be found here:
;; https://www.smspower.org/uploads/Development/JavaGear-Report.pdf

;; The io-bus will need to communicate with the CPU, even though we have not composed it yet.
;; We can't compose the CPU yet, because we must pass it both a memory-bus and an io-bus.
(defonce cpu (atom nil))

(defn construct-cpu!
  "This function will finally tie all components together.
  It will construct a Z80Core CPU and pass it a Memory Bus that knows how to communicate between CPU/RAM/ROM.
  It will also pass it a proper IO-BUS that know how to communicate between CPU/VDP/JoyPads."
  [^z80.vdp.VdpState vdp]
  (let [cpu-instance (Z80Core. (memory/make-memory-bus) (io-bus/make-io-bus cpu vdp))]
    (reset! cpu cpu-instance)))

;; --------------------------------------------------------------------------------------------------
;; ------------------------------------------ Main function -----------------------------------------
;; --------------------------------------------------------------------------------------------------

(defn -main [& args]
  ;; The ROM path must be provided as a command line argument.
  (if (empty? args) (println "Please supply the path to a ROM as a command line argument.")
    ;; Create all components, starting with the VDP and load a ROM into memory.
    (let [active-vdp (atom (vdp/create-vdp))]
      (construct-cpu! active-vdp)
      (memory/load-rom-into-memory! (java.nio.file.Files/readAllBytes (java.nio.file.Paths/get (first args) (into-array String []))))
      (q/defsketch sms-screen
        :title "DeFn System"
        ;; NOTE: These two functions really kick off the emulation.
        ;; The setup/draw functions will start the Z80 instruction loop and draw the result to the screen.
        :setup (emu-loop/make-setup-function @cpu)
        :draw (emu-loop/make-draw-function @cpu active-vdp)
        :key-pressed (joypads/make-key-press-handler @cpu)
        :key-released (joypads/make-key-release-handler)
        :renderer :opengl
        :features [:exit-on-close]
        :size [display/screen-width display/screen-height]))))
