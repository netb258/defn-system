(ns z80.vdp
  (:import [com.codingrodent.microprocessor IMemory IBaseDevice]
           [com.codingrodent.microprocessor.Z80 Z80Core]
           [com.codingrodent.microprocessor.Z80 CPUConstants$RegisterNames]))

(defrecord VdpState [
  vram
  cram
  regs
  first-byte?
  command-byte
  vram-pointer
  operation
  read-buffer
  current-scan-line
  vblank-active?
  sprite-collision?])

(defn create-vdp []
  (map->VdpState {
    :vram (byte-array 16384) ;; 16KB of VRAM. Used for pretty much all graphics in an SMS game.
    :cram (int-array 32)     ;; 32 bytes of cram. Used for color.
    :regs (int-array 16)     ;; 16 registers. They store valuable info on H/V scrolling locks, Sprite Attribute Tables and more.
    :first-byte? true        ;; Every command the Z80 sends to the VDP control port is 2 bytes long. This is tracked with a 1-bit flip-flop (true/false).
    :command-byte 0          ;; A temporary holding buffer for the first byte of a 2-byte control command.
    :vram-pointer 0          ;; This will be The VDP Video Memory 14 bit Address Pointer.
    :operation 0             ;; Remembers the current mode (0, 1, 2, or 3) that the VDP is operating in.
    :read-buffer 0           ;; Small but fast 8bit VRAM cache.
    :current-scan-line 0     ;; This is basically the V-COUNTER.
    :vblank-active? false    ;; Is the CPU executing a V-BLANK interrupt currently?
    :sprite-collision? false})) ;; Are any sprites colliding currently?

;; As mentioned above, the VDP can operate in 4 modes:
;; Mode 0 (00): VRAM Read Mode - Used when the Z80 CPU wants to read graphics data out of the VDP's 16KB Video RAM.
;; Mode 1 (01): VRAM Write Mode - Sets up the VDP so the Z80 can upload background tiles, sprite graphics, and the name table into VRAM.
;; Mode 2 (10): VDP Register Write Mode - Write to one of the VDPs 16 registers.
;; Mode 3 (11): CRAM (Color RAM) Write Mode - This mode is dedicated to changing the colors displayed on the screen.

(defn get-v-counter [^VdpState vdp]
  ;; PAL Hardware V-Counter mapping rules:
  ;; Lines 0-242 count up linearly (0x00 to 0xF2).
  ;; Lines 243-312 jump to 0xBA and increment to 0xFF.
  (let [line (int (.current-scan-line vdp))]
    (cond
      (<= line 242) line
      (<= line 312) (+ 0xBA (- line 243))
      :else 0xFF))) ;; Safety boundary fallback

(defn calculate-h-counter [^Z80Core cpu]
  ;; The H-counter is purely dependent on the current Z80 line progress.
  ;; 1 Z80 cycle = 1.5 H-Counter increments.
  ;; NOTE: Tried keeping h-counter inside the VDP record, but this calculation is faster.
  (let [current-cycles (.getTStates cpu)
        line-cycles (mod current-cycles 227) ;; 227 cycles per line in PAL
        h-val (quot (* line-cycles 3) 2)]
    (if (<= h-val 225)
      h-val
      (bit-and (+ 202 (- h-val 226)) 0xFF))))

;; NOTE: IN the following code, you will see this alot (bit-and some-vram-address 0x3FFF).
;; The reason for this is that the vram pointer is 14 bits long and doesn't fit neatly into standard bytes and words (8 and 16 bits).

(defn data-write! [^VdpState vdp ^long value]
  (let [op (int (.operation vdp))
        loc (int (.vram-pointer vdp))]
    (cond
      ;; VRAM Write
      ;; Even though the docs say that only Mode 1 is VRAM write, the actual hardware behaves like this.
      (or (= op 0) (= op 1) (= op 2))
      (let [address (bit-and loc 0x3FFF)
            ^bytes vram (.vram vdp)]
        (aset vram address (unchecked-byte value)))

      ;; CRAM (Palette) Write
      (= op 3)
      (let [cram-idx (bit-and loc 0x1F)
            ^ints cram (.cram vdp)]
        (aset cram cram-idx (int value))))

    (-> vdp
        ;; Address pointer must wrap around at 14 bits (0x3FFF)
        (assoc :vram-pointer (bit-and (inc loc) 0x3FFF))
        ;; FIX for FluBBa VDP test 9: Hardware writes to the data port explicitly overwrite the read buffer!
        (assoc :read-buffer value)
        (assoc :first-byte? true))))

(defn data-read! [^VdpState vdp]
  (let [loc (int (.vram-pointer vdp))
        address (bit-and loc 0x3FFF)
        ^bytes vram-arr (.vram vdp)
        ;; 1. The CPU receives what was ALREADY sitting in the hardware buffer
        return-val (bit-and (int (.read-buffer vdp)) 0xFF)
        ;; 2. Prefetch the NEXT byte from VRAM into the buffer for the next read
        next-buffered-val (bit-and (aget vram-arr address) 0xFF)
        ;; 3. Increment and wrap the VRAM address pointer
        next-loc (bit-and (inc loc) 0x3FFF)]
    [return-val (assoc vdp 
                       :vram-pointer next-loc 
                       :read-buffer next-buffered-val
                       :first-byte? true)]))

;; Before the Z80 can instruct the VDP to perform one of it's 4 modes (VRAM Read, VRAM Write, VDP Register Write, CRAM Write)
;; it must first write two bytes to the VDP control port (status port). Those two bytes will set the VDP in the proper state
;; to perform an upcoming VRAM Read, VRAM Write, VDP Register Write or CRAM Write.
;; This is exactly what this function does.
;; It parses the two byte command and sets the VDP in the proper state to execute one of it's modes.

(defn control-write! [^VdpState vdp ^long value]
  (if (:first-byte? vdp)
    ;; First byte: Save and wait for the second byte
    (let [clean-val (bit-and value 0xFF)
          old-loc (int (.vram-pointer vdp))
          new-loc (bit-or (bit-and old-loc 0x3F00) clean-val)
          
          ;; Hardware Prefetch: If currently in Read Mode (op 0), update read-buffer instantly!
          op (int (.operation vdp))
          ^bytes vram-arr (.vram vdp)
          updated-buffer (if (= op 0) 
                           (bit-and (aget vram-arr (bit-and new-loc 0x3FFF)) 0xFF) 
                           (int (.read-buffer vdp)))]
      (assoc vdp 
             :command-byte clean-val 
             :vram-pointer new-loc
             :read-buffer updated-buffer
             :first-byte? false))
    
    ;; Second byte received: Combine both to process command
    (let [low-byte (bit-and (:command-byte vdp) 0xFF)
          high-byte (bit-and value 0xFF)
          ;; Extract Operation Code (Top 2 bits of the second byte)
          code-type (bit-shift-right (bit-and high-byte 0xC0) 6)
          ;; Extract Address (Lower 6 bits of high byte + full low byte)
          new-loc (bit-or low-byte (bit-shift-left (bit-and high-byte 0x3F) 8))]
      (cond
        ;; Mode 0: VRAM Read
        (= code-type 0)
        (let [^bytes vram-arr (.vram vdp)
              buffered-val (if (< new-loc (count vram-arr)) (bit-and (aget vram-arr new-loc) 0xFF) 0)]
          (assoc vdp :vram-pointer (inc new-loc) :operation code-type :read-buffer buffered-val :first-byte? true))

        ;; Mode 1: VRAM Write
        (= code-type 1) (assoc vdp :vram-pointer new-loc :operation code-type :first-byte? true)

        ;; Mode 2: VDP Register Write (Top bits are 10xx xxxx)
        (= code-type 2)
        (let [reg-num (bit-and high-byte 0x0F) 
              ^ints regs-arr (.regs vdp)]
          (when (< reg-num (alength regs-arr))
            (aset regs-arr reg-num (int low-byte)))
          (assoc vdp :first-byte? true))

        ;; Mode 3: CRAM Pointer Setup (Top bits are 11xx xxxx)
        ;; The operation must be set to 3 so data-write! knows to route incoming bytes to CRAM.
        (= code-type 3) (assoc vdp :vram-pointer new-loc :operation 3 :first-byte? true)
        :else (assoc vdp :first-byte? true)))))

;; The CPU needs to read from the status port, because it is important for timing and synchronization between the Z80 CPU and the VDP.

(defn read-status-port! [^VdpState vdp ^Z80Core cpu]
  ;; Check if V-Blank is actively triggered and also check for sprite collisions.
  (let [vblank-bit (if (:vblank-active? vdp) 0x80 0x00)
        collision-bit (if (:sprite-collision? vdp) 0x20 0x00)
        current-status (bit-or vblank-bit collision-bit)]
    ;; Reading this port clears the CPU interrupt line.
    (.setInterrupt cpu false)
    ;; Return the accumulated status byte and clear both flags on read
    [current-status (assoc vdp 
                           :first-byte? true 
                           :vblank-active? false
                           :sprite-collision? false)]))
