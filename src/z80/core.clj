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

;; The io-bus will need to communicate with the CPU, even though we have not composed it yet.
;; We can't compose the CPU yet, because we must pass it both a memory-bus and an io-bus.
(declare cpu)

;; Construct the VDP record.
;; The VDP does not need to be passed when we construct the CPU.
;; The CPU will read data from and write data to the VDP through the io-bus.
(def active-vdp (atom (vdp/create-vdp)))

(defn- do-vdp-io-read!
  "Calls either vdp/read-status-port! or vdp/data-read! (the two VDP io reading functions).
   Puts the VDP in a new state and returns the result byte that these read functions return."
  [^z80.vdp.VdpState atom-vdp read-fn & args]
  (let [result (atom nil)]
    (swap! atom-vdp (fn [vdp]
                      (let [[val updated-vdp] (apply read-fn vdp args)]
                        (reset! result val)
                        updated-vdp)))
    @result))

;; --- SEGA MASTER SYSTEM I/O BUS ---
;; SMS components like Video (VDP) and Joypads are hooked up to the ports here.

(defn make-io-bus []
  (reify IBaseDevice
    (IORead [this address]
      (let [port (memory/signed->unsigned address)
            ;; Decode ports by their upper 2 bits (4 main blocks)
            port-group (bit-and port 0xC0)] 
        (cond
          ;; --- Group 0x00 to 0x3F ---
          (= port-group 0x00)
          (if (even? port)
            0xFF ;; Port $00 is generally unmapped/read-only export status or open bus
            (vdp/get-v-counter @active-vdp)) ;; Odd ports ($01-$3F) return V-Counter!

          ;; --- Group 0x40 to 0x7F ---
          (= port-group 0x40)
          (if (even? port)
            ;; NOTE: The v-counter is basically the current scan-line
            ;; The h-counter is the current pixel within that scan-line.
            (vdp/get-v-counter @active-vdp) ;; Even ports ($40-$7E) = V-Counter
            (vdp/calculate-h-counter cpu))  ;; Odd ports ($41-$7F)  = H-Counter

          ;; --- Group 0x80 to 0xBF ---
          (= port-group 0x80)
          (if (even? port)
            ;; VDP DATA PORT ($BE)
            (do-vdp-io-read! active-vdp vdp/data-read!)
            ;; VDP STATUS PORT ($BF)
            (do-vdp-io-read! active-vdp vdp/read-status-port! cpu))

          ;; --- Group 0xC0 to 0xFF ---
          (= port-group 0xC0)
          (if (even? port)
            (joypads/read-joypad1) ;; Even ports ($DC) = P1 Input
            (bit-or (joypads/read-joypad2) 0x80)) ;; Odd ports ($DD)  = P2 Input + Export (non Japan) Bit (Bit 7 = 1)

          :else 0xFF)))

    (IOWrite [this address data]
      (let [port (memory/signed->unsigned address)
            port-group (bit-and port 0xC0)]
        (cond
          ;; VDP Writes ($80-$BF)
          ;; NOTE: port 0xBF pulls double duty depending on whether the Z80 CPU is writing to it or reading from it.
          ;; When reading, it serves as the status port. When writing it is the control port.
          (= port-group 0x80)
          (if (even? port)
            (swap! active-vdp vdp/data-write! (unchecked-byte data))
            (swap! active-vdp vdp/control-write! data)))
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
