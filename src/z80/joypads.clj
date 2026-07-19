(ns z80.joypads
  (:require [quil.core :as q])
  (:import java.awt.event.KeyEvent))

;; Default state is 0xFF (all bits 1 = all buttons unpressed)
(def ^:private joypad-p1 (atom 0xFF))
(def ^:private joypad-p2 (atom 0xFF))

(defn read-joypad1 [] @joypad-p1)
(defn read-joypad2 [] @joypad-p2)

;; On a standard SMS joypad, 0 means pressed and 1 means unpressed (active low). When no buttons are held, ports 0xDC and 0xDD must return 0xFF.

;; The Z80 CPU will use ports 0xDC and 0xDD to interact with the joypads (basically it just reads data from them).
;; The two ports are responsible for the following: 
;; Port 0xDC (Data Port A): Covers Player 1 controls and part of Player 2.
;; Port 0xDD (Data Port B): Covers the rest of Player 2 and the Reset button.
;; When the Z80 executes an IN A, ($DC) instruction, it expects a byte where 0 means pressed and 1 means unpressed (active low).

(defn- get-key []
  (let [raw-key (q/raw-key)
        the-key-code (q/key-code)
        the-key-pressed (if (= processing.core.PConstants/CODED (int raw-key)) the-key-code raw-key)]
    the-key-pressed))

;; Map keyboard characters/keywords to their respective hardware bits
(def ^:private p1-key-map
  {KeyEvent/VK_UP    0x01
   KeyEvent/VK_DOWN  0x02
   KeyEvent/VK_LEFT  0x04
   KeyEvent/VK_RIGHT 0x08
   \z                0x10   ;; Button 1 (Also considered Start)
   \x                0x20}) ;; Button 2

(def ^:private p2-key-map
  {\i 0x40   ;; P2 Up (shares port 0xDC bit 6)
   \k 0x80   ;; P2 Down (shares port 0xDC bit 7)
   \j 0x01   ;; P2 Left (on port 0xDD bit 0)
   \l 0x02   ;; P2 Right (on port 0xDD bit 1)
   \n 0x04   ;; P2 Button 1 (on port 0xDD bit 2)
   \m 0x08}) ;; P2 Button 2 (on port 0xDD bit 3)
 ;; P2 Button 2 (on port 0xDD bit 3)

(defn make-key-press-handler [^com.codingrodent.microprocessor.Z80.Z80Core cpu]
  (fn []
    (let [user-input (get-key)]
      (if (= user-input \newline)
        ;; Intercept the pause key and fire an NMI directly into the Java Z80 core
        ;; We need this because the Master System did not feature a pouse button on the JoyPad.
        ;; Instead the pause button was placed on the console itself.
        (.setNMI cpu)
        ;; Otherwise, run the existing controller port code
        (do
          ;; Player 1
          (when-let [bit-p1 (get p1-key-map user-input)]
            (swap! joypad-p1 #(bit-and % (bit-not bit-p1))))
          ;; Player 2
          (when-let [bit-p2 (get p2-key-map user-input)]
            (if (or (= user-input :up) (= user-input :down))
              (swap! joypad-p1 #(bit-and % (bit-not bit-p2)))
              (swap! joypad-p2 #(bit-and % (bit-not bit-p2))))))))))

(defn make-key-release-handler []
  (fn []
    (let [user-input (get-key)]
      ;; Player 1
      (when-let [bit-p1 (get p1-key-map user-input)]
        (swap! joypad-p1 #(bit-or % bit-p1)))
      ;; Player 2 (special case for up/down)
      (when-let [bit-p2 (get p2-key-map user-input)]
        (if (or (= user-input :up) (= user-input :down))
          (swap! joypad-p1 #(bit-or % bit-p2))
          (swap! joypad-p2 #(bit-or % bit-p2)))))))
