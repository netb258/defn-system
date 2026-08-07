(ns z80.joypads
  (:require [quil.core :as q])
  (:import java.awt.event.KeyEvent))

;; Default state is 0xFF (all bits 1 = all buttons unpressed)
(def ^:private joypad-p1 (atom 2r11111111))
(def ^:private joypad-p2 (atom 2r11111111))

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
  {KeyEvent/VK_UP    2r00000001
   KeyEvent/VK_DOWN  2r00000010
   KeyEvent/VK_LEFT  2r00000100
   KeyEvent/VK_RIGHT 2r00001000
   \z                2r00010000   ;; Button 1 (Also considered Start)
   \x                2r00100000}) ;; Button 2

(def ^:private p2-key-map
  {\i 2r01000000   ;; P2 Up (shares port 0xDC bit 6)
   \k 2r10000000   ;; P2 Down (shares port 0xDC bit 7)
   \j 2r00000001   ;; P2 Left (on port 0xDD bit 0)
   \l 2r00000010   ;; P2 Right (on port 0xDD bit 1)
   \n 2r00000100   ;; P2 Button 1 (on port 0xDD bit 2)
   \m 2r00001000}) ;; P2 Button 2 (on port 0xDD bit 3)
 ;; P2 Button 2 (on port 0xDD bit 3)

(defn- set-pressed
  "Takes the byte representing all buttons (it always starts out as 2r11111111) and one of the key press bytes defined above.
  button-byte will only have one bit set to 1, the others will be zero. That same 1 bit, will be set to 0 in all-buttons-byte.
  Remember that the Master System will count 0 as pressed and 1 as unpressed.
  For example: (set-pressed 2r11111111 2r00000001) => 2r11111110"
  [all-buttons-byte button-byte]
  (bit-and all-buttons-byte (bit-not button-byte)))

(defn- set-released
  "Takes the byte representing all buttons (it always starts out as 2r11111111) and one of the key press bytes defined above.
  button-byte will only have one bit set to 1, the others will be zero. That same 1 bit, will be set to 1 in all-buttons-byte.
  Remember that the Master System will count 0 as pressed and 1 as unpressed.
  For example: (set-released 2r11000000 2r00000001) => 2r11000001"
  [all-buttons-byte button-byte]
  (bit-or all-buttons-byte button-byte))

(defn make-key-press-handler [^com.codingrodent.microprocessor.Z80.Z80Core cpu]
  (fn []
    (let [user-input (get-key)]
      (if (= user-input \newline)
        ;; Intercept the pause key and fire an NMI directly into the Java Z80 core (to pause the CPU)
        ;; The Master System did not feature a pouse button on the JoyPad. The pause button was placed on the console itself.
        ;; When this button was pressed, it issued a non maskable interrupt that the CPU cannot ignore.
        (.setNMI cpu)
        ;; Otherwise, run the existing controller port code
        (do
          ;; Player 1
          (when-let [bit-p1 (get p1-key-map user-input)]
            (swap! joypad-p1 set-pressed bit-p1))
          ;; Player 2
          (when-let [bit-p2 (get p2-key-map user-input)]
            ;; Remember joypad-p1 handles port 0xDC and needs to report if \i or \k are pressed.
            (if (or (= user-input \i) (= user-input \k))
              (swap! joypad-p1 set-pressed bit-p2)
              (swap! joypad-p2 set-pressed bit-p2))))))))

(defn make-key-release-handler []
  (fn []
    (let [user-input (get-key)]
      ;; Player 1
      (when-let [bit-p1 (get p1-key-map user-input)]
        (swap! joypad-p1 set-released bit-p1))
      ;; Player 2 (special case for up/down)
      (when-let [bit-p2 (get p2-key-map user-input)]
        ;; Remember joypad-p1 handles port 0xDC and needs to report if \i or \k are released.
        (if (or (= user-input \i) (= user-input \k))
          (swap! joypad-p1 set-released bit-p2)
          (swap! joypad-p2 set-released bit-p2))))))
