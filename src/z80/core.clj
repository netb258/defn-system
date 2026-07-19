(ns z80.core
  (:require [z80.vdp :as vdp]
            [z80.memory :as memory]
            [z80.joypads :as joypads]
            [z80.display :as display]
            [z80.emulation-loop :as emu-loop]
            [quil.core :as q])
  (:import [com.codingrodent.microprocessor IMemory IBaseDevice]
           [com.codingrodent.microprocessor.Z80 Z80Core]
           [com.codingrodent.microprocessor.Z80 CPUConstants$RegisterNames])
  (:gen-class))

;; The program will perform a lot worse with reflection, so we should add type hints where possible.
;; (set! *warn-on-reflection* true)

;; Initialize a reference to the VDP
(def active-vdp (atom (vdp/create-vdp)))

;; The io-bus will need to communicate with the CPU, even though we have not composed it yet.
(declare cpu)

;; --- SEGA MASTER SYSTEM I/O BUS ---
;; SMS components like Video (VDP) and Joypads are hooked up to the ports here.

;; NOTE: The IO-BUS turned out to be just one function. Instead of needlessly making it a module, I'm just dropping it here:
(defn make-io-bus []
  (reify IBaseDevice
    (IORead [this address]
      (let [port (memory/signed->unsigned address)]
        (cond
          ;; --- VDP V-COUNTER PORT (PAL 50Hz MODE) ---
          ;; Returns the current vertical scanline coordinate.
          ;; Total PAL frame = 71040 cycles across 313 scanlines (~227.13 cycles per line).
          (= port 0x7E) (vdp/get-v-counter @active-vdp)
          ;; --- VDP H-COUNTER PORT (PAL 50Hz MODE) ---
          ;; Returns the current horizontal scanline coordinate.
          (= port 0x7F) (vdp/calculate-h-counter cpu)
          ;; --- VDP DATA PORT ---
          (= port 0xBE)
          (let [result (atom 0x00)]
            (swap! active-vdp (fn [vdp]
                                (let [[val updated-vdp] (vdp/data-read! vdp)]
                                  (reset! result val)
                                  updated-vdp)))
            @result)

          ;; --- VDP STATUS PORT ---
          ;; Clears the Vblank flag inside the VDP.
          (= port 0xBF)
          (let [result (atom 0x00)]
            (swap! active-vdp (fn [vdp]
                                (let [[val updated-vdp] (vdp/read-status-port! vdp cpu)]
                                  (reset! result val)
                                  updated-vdp)))
            @result)

          ;; Controller Ports
          (= port 0xDC) (joypads/read-joypad1)
          (= port 0xDD) (joypads/read-joypad2)
          :else 0xFF)))

    (IOWrite [this address data]
      (let [port (memory/signed->unsigned address)]
        (cond
          (= port 0xBE) (swap! active-vdp vdp/data-write! (unchecked-byte data))
          ;; NOTE: port 0xBF pulls double duty depending on whether the Z80 CPU is writing to it or reading from it.
          ;; When reading, it serves as the status port. When writing it is the control port.
          (= port 0xBF) (swap! active-vdp vdp/control-write! data))
        nil))))

;; Instantiate the CPU with both Memory and IO Bus
(def ^com.codingrodent.microprocessor.Z80.Z80Core cpu (Z80Core. (memory/make-memory-bus) (make-io-bus)))

;; --------------------------------------------------------------------------------------------------
;; ------------------------------------------ Main function -----------------------------------------
;; --------------------------------------------------------------------------------------------------

(defn -main [& args]
  ;; The ROM path must be provided as a command line argument.
  (if (empty? args) (println "Please supply the path to a ROM as a command line argument.")
    (do (memory/load-rom-into-memory! (java.nio.file.Files/readAllBytes (java.nio.file.Paths/get (first args) (into-array String []))))
        (q/defsketch sms-screen
          :title "DeFn System"
          :renderer :opengl
          :features [:exit-on-close]
          :key-pressed (joypads/make-key-press-handler cpu)
          :key-released (joypads/make-key-release-handler)
          :size [display/screen-width display/screen-height]
          :setup (emu-loop/make-setup-function cpu)
          :draw (emu-loop/make-draw-function cpu active-vdp)))))
