(ns z80.vdp
  (:require [z80.memory :as memory])
  (:import [com.codingrodent.microprocessor.Z80 Z80Core]))

;; Note, that this VDP implementation will NOT do any actual rendering. That task is handled by another module "display.clj".
;; We just need the data defined in VdpState to be filled out properly. Particularly :vram, :cram and :regs.
;; After that "display.clj" can parse this VDP data and do the rendering.

(defrecord VdpState [
  vram
  cram
  gg-cram-buffer
  regs
  first-byte?
  command-byte
  vram-pointer
  mode
  read-buffer
  current-scan-line
  sprite-overflow?
  sprite-collision?
  hblank-active?
  vblank-active?
  report-vblank-active?])

;; NOTE on VDP Registers - Notice that our VDP record below contains an array of 16 registers (:regs (int-array 16)).
;; Other modules will frequently look up these registers. This is a full description of the data they hold:
;; https://www.smspower.org/Development/VDPRegisters

(defn create-vdp []
  (map->VdpState {
    :vram (byte-array 16384) ;; 16KB of VRAM. Used for pretty much all graphics in an SMS game.
    :cram (int-array 64)     ;; The SMS has 32 bytes of CRAM. Holds 32 colors. However the GG needs 64 bytes (32 colors * 2 bytes each). Using 64 bytes covers the needs of both.
    :gg-cram-buffer 0        ;; Game Gear specific: The GG uses 12-bit colors, so they require 2 bytes. Holds the low byte of the 12-bit color.
    :regs (int-array 16)     ;; 16 registers. They store valuable info on H/V scrolling locks, Sprite Attribute Tables and more.
    :first-byte? true        ;; Every command the Z80 sends to the VDP control port is 2 bytes long. This is tracked with a 1-bit flip-flop (true/false).
    :command-byte 0          ;; A temporary holding buffer for the first byte of a 2-byte control command.
    :vram-pointer 0          ;; This will be The VDP Video Memory 14 bit Address Pointer.
    :mode 0                  ;; Remembers the current mode (0, 1, 2, or 3) that the VDP is operating in.
    :read-buffer 0           ;; Small but fast 8bit VRAM cache.
    :current-scan-line 0     ;; This is basically the V-COUNTER.
    :sprite-overflow? false  ;; The Master System can only have 8 sprites on a single scan-line. The VDP should report if this limit is exceeded.
    :sprite-collision? false ;; Are any sprites colliding currently?
    :hblank-active? false    ;; Is the CPU executing an H-BLANK interrupt currently (this gets set during the Z80 instruction loop)?
    :vblank-active? false    ;; Is the CPU executing a V-BLANK interrupt currently  (this gets set during the Z80 instruction loop)?
    :report-vblank-active? false ;; One of the VDP's duties is to report if a V-BLANK interrupt is happening during a VDP Status Port read.
    }))

;; NOTE: There are 2 different flags for V-BLANK (:vblank-active? and :report-vblank-active?).
;; The difference between them is that :report-vblank-active? only serves for VDP status port reads,
;; while vblank-active? is actually used for interrupt management in the CPU instruction loop.

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
      (memory/signed->unsigned (+ 202 (- h-val 226))))))

;; NOTE: The code needed a lot of this: (bit-and some-vram-address 0x3FFF).
;; The reason for this is that the vram pointer is 14 bits long and doesn't fit neatly into standard bytes and words (8 and 16 bits).
;; In the end I just rolled it into a small helper function:

(defn- take-14-bits
  "Takes a Clojure number (which is stored as a long).
  Returns a new number with only the first 14 bits kept as they are.
  The rest of the bits are set to 0."
  [number]
  (bit-and number 2r0011111111111111))

(defn- make-word 
  "Takes two bytes and glues them together into a word."
  [byte1 byte2]
  (bit-or byte2 (bit-shift-left byte1 8)))

(defn- move-to-next-vram-addr
  "Returns a VDP state that is ready to read the next value in VRAM."
  [^VdpState vdp ^long current-vram-address ^long current-vram-value]
  (-> vdp
      (assoc :vram-pointer (take-14-bits (inc current-vram-address)))
      (assoc :read-buffer current-vram-value)
      (assoc :first-byte? true)))

(defn- do-gg-cram-write!
  "WRITING to the Game Gear's CRAM is more work than with the Master System.
   Since all colors require 2 bytes, we need to cache the first byte and wait for the second.
   Keep in mind, READING from CRAM is the same in both systems. It simply happens one byte at a time."
  [^VdpState vdp ^long loc ^long value]
  (let [cram-addr (bit-and loc 2r00111111) ;; GG has 64 bytes/indexes in CRAM, so we need 6 bits.
        is-even? (zero? (bit-and cram-addr 2r00000001))
        ^ints cram (.cram vdp)]
    (if is-even?
      ;; Even byte: Just cache it in the CRAM buffer, don't write to CRAM yet
      (-> (move-to-next-vram-addr vdp loc value)
          (assoc :gg-cram-buffer (memory/signed->unsigned value)))

      ;; Odd byte: Combine cached even byte with current odd byte and write to CRAM
      (let [even-byte (:gg-cram-buffer vdp)
            odd-byte (memory/signed->unsigned value)
            ;; Storing them safely into sequential indices in our int-array
            even-cram-idx (dec cram-addr)
            odd-cram-idx cram-addr]
        (aset cram even-cram-idx (int even-byte))
        (aset cram odd-cram-idx (int odd-byte))
        (move-to-next-vram-addr vdp loc value)))))

(defn data-write! [^VdpState vdp ^long value]
  (let [op (int (.mode vdp))
        loc (int (.vram-pointer vdp))]
    (cond
      ;; --- VRAM Write (Modes 0, 1, 2) ---
      (or (= op 0) (= op 1) (= op 2))
      (let [address (take-14-bits loc)
            ^bytes vram (.vram vdp)]
        (aset vram address (unchecked-byte value))
        (move-to-next-vram-addr vdp loc value))

      ;; --- CRAM (Palette) Write (Mode 3) ---
      (= op 3)
      (if @memory/gg-rom-selected? (do-gg-cram-write! vdp loc value)
        (let [cram-idx (bit-and loc 2r00011111) ;; We only take 5 bits here, they can represent 32 numbers.
              ^ints cram (.cram vdp)]
          (aset cram cram-idx (int value))
          (move-to-next-vram-addr vdp loc value)))

      :else (move-to-next-vram-addr vdp loc value))))

(defn data-read!
  "Reads a single byte from the VDP's VRAM.
  The function returns a vector with two values inside it.
  The first one is the byte that was read.
  The second one is the new state that the VDP should transition to."
  [^VdpState vdp]
  (let [loc (int (.vram-pointer vdp))
        address (take-14-bits loc)
        ^bytes vram-arr (.vram vdp)
        ;; 1. The CPU receives what was ALREADY sitting in the hardware buffer
        return-val (memory/signed->unsigned (.read-buffer vdp))
        ;; 2. Prefetch the NEXT byte from VRAM into the buffer for the next read
        next-buffered-val (memory/signed->unsigned (aget vram-arr address))]
    [return-val (move-to-next-vram-addr vdp loc next-buffered-val)]))

;; Before the Z80 can instruct the VDP to perform one of it's 4 modes (VRAM Read, VRAM Write, VDP Register Write, CRAM Write)
;; it must first write two bytes to the VDP control port. Those two bytes will set the VDP in the proper state
;; to perform an upcoming VRAM Read, VRAM Write, VDP Register Write or CRAM Write.
;; This is exactly what this function does.
;; It parses the two byte command and sets the VDP in the proper state to execute one of it's modes.

(defn control-write!
  "Takes a byte 'value' and depending on what is inside the byte, the VDP will transition to a new state.
  The function returns the new state that the VDP shuld transition to.

  NOTE, that Mode 2: VDP Register Write is a special case. In Mode 2, both the register write will be performed
  and also a new state for the VDP will be returned. The other modes do not actually perform their specified tasks.
  Think of them as set-up phases."
  [^VdpState vdp ^long value]
  (if (:first-byte? vdp)
    ;; First byte: Save and wait for the second byte
    ;; When a first-byte arrives, we need to construct a new vram-pointer
    ;; by keeping 6 bits from the old one and adding the 8 bits from the new first-byte.
    (let [clean-val (memory/signed->unsigned value)
          old-loc (int (.vram-pointer vdp))
          ;; This value tells you everyting: 2r11111100000000.
          ;; We keep the 1 bits as they currently are and set the rest to 0. Then we fill the 0 bits with clean-val.
          new-loc (bit-or (bit-and old-loc 2r11111100000000) clean-val)
          
          ;; Hardware Prefetch: If currently in Read Mode (op 0), update read-buffer instantly!
          op (int (.mode vdp))
          ^bytes vram-arr (.vram vdp)
          updated-buffer (if (= op 0) 
                           (memory/signed->unsigned (aget vram-arr (take-14-bits new-loc))) 
                           (int (.read-buffer vdp)))]
      (assoc vdp 
             :command-byte clean-val 
             :vram-pointer new-loc
             :read-buffer updated-buffer
             :first-byte? false))

    ;; Second byte received: Combine both to process command
    (let [low-byte (memory/signed->unsigned (:command-byte vdp))
          high-byte (memory/signed->unsigned value)
          ;; Extract Operation Code (Top 2 bits of the second byte)
          ;; We need to push these 2 bits all the way to the right to get a small number (0-3)
          ;; Otherwise it works out to a large number (ex. 2r11000000 is 192)
          code-type (bit-shift-right (bit-and high-byte 2r11000000) 6)
          ;; Extract Address (Lower 6 bits of high byte + full low byte)
          new-loc (make-word (bit-and high-byte 2r00111111) low-byte)]
      (cond
        ;; Mode 0: VRAM Read
        (= code-type 0)
        (let [^bytes vram-arr (.vram vdp)
              vram-val (memory/signed->unsigned (aget vram-arr new-loc))]
          (assoc vdp
                 :vram-pointer (inc new-loc)
                 :mode code-type
                 :read-buffer vram-val
                 :first-byte? true))

        ;; Mode 1: VRAM Write
        (= code-type 1) (assoc vdp :vram-pointer new-loc :mode code-type :first-byte? true)

        ;; Mode 2: VDP Register Write (Top bits are 10xx xxxx)
        (= code-type 2)
        (let [reg-num (bit-and high-byte 2r00001111) ;; Figure out exactly which register to write.
              ^ints regs-arr (.regs vdp)]
          (aset regs-arr reg-num (int low-byte))
          (assoc vdp 
                 :mode code-type
                 :vram-pointer new-loc
                 :first-byte? true))

        ;; Mode 3: CRAM Pointer Setup (Top bits are 11xx xxxx)
        ;; The operation must be set to 3 so data-write! knows to route incoming bytes to CRAM.
        (= code-type 3) (assoc vdp :vram-pointer new-loc :mode 3 :first-byte? true)
        :else (assoc vdp :first-byte? true)))))

;; NOTE: The VDP status port should report a few statuses. First and foremost:
;; When the Z80 CPU fires an interrupt, the game code can't easily tell the reason why the interrupt happened.
;; It's part of the VDP's job to keep track of when interrupts are fired because of a graphical VBLank event.
;; This way the game code can read the status port $BF and receive a byte flag
;; that tells it if the current interrupt is a VBlank interrupt.
;; Note that there is no corresponding status code for HBlank. Instead a counter in VDP register 10 is used.

;; The other two statuses (Sprite Collision and Sprite Overflow) are rarely used by comparison, but they are included.
;; The Sprite collision flag does sound important, but in practice most games just use their own collision logic.

(defn read-status-port!
  "Returns a VDP status byte and a new state for the VDP.
  The function returns a vector with two values inside it.
  The first one is the status byte.
  The second one is the new state that the VDP should transition to."
  [^VdpState vdp ^Z80Core cpu]
  ;; Check if V-Blank is actively triggered and also check for sprite collisions and overflows.
  (let [vblank-bit    (if (:report-vblank-active? vdp) 2r10000000 0x00)
        overflow-bit  (if (:sprite-overflow? vdp)      2r01000000 0x00)
        collision-bit (if (:sprite-collision? vdp)     2r00100000 0x00)
        ;; When the CPU reads the VDP status port it must receive this combined status byte.
        current-status (bit-or vblank-bit overflow-bit collision-bit)]
    ;; Reading this port clears the CPU interrupt line.
    (.setInterrupt cpu false)
    ;; Return the accumulated status byte and reset VDP status flags.
    ;; NOTE: Reading the VDP status port is the only thing that clears :report-vblank-active?
    [current-status (assoc vdp 
                           :first-byte? true 
                           :vblank-active? false
                           :report-vblank-active? false
                           :hblank-active? false
                           :sprite-overflow? false
                           :sprite-collision? false)]))
