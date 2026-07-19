(ns z80.memory
  (:import [com.codingrodent.microprocessor IMemory]))

;; --- SEGA MASTER SYSTEM MEMORY LAYOUT ---
;; The SMS has 64KB of total address space:
;; 0x0000 - 0xBFFF : ROM Cartridge space (48KB)
;; 0xC000 - 0xDFFF : System RAM (8KB)
;; 0xE000 - 0xFFFF : System RAM Mirror (Points to the same 8KB RAM)
;; They needed the mirror RAM addresses for convenience and also it apparantly saves on hardware.
(def ^:private rom-cart-start 0x0000)
(def ^:private rom-cart-end   0xBFFF)
(def ^:private ram-start      0xC000)
(def ^:private ram-end        0xDFFF)
(def ^:private mram-start     0xE000)
(def ^:private mram-end       0xFFFF)

;; (def ^{:tag 'bytes} rom (byte-array 49152))    ;; 48KB max for a basic ROM with no mapper.
;; Since we are now implementing the standard Sega Mapper our old static 48KB array needs to go.
(def ^:private rom (atom (byte-array 0)))
;; The stardard Sega Mapper splits the ROM space into 16kb pieces/slots
;; and dynamically loads parts of large games into the ROM space.
;; Track the current active bank index for each of the three 16KB slots
(def ^:private mapper-banks (atom {:slot0 0
                                   :slot1 1
                                   :slot2 2}))
(def ^{:tag 'bytes :private true} sms-ram (byte-array 8192)) ;; 8KB of actual Work RAM

(defn signed->unsigned
  "Takes a signed byte (range -128 to 127) 
  and converts it to an unsigned byte (range 0 to 255)."
  ^long [^long signed-byte]
  (bit-and signed-byte 0xFF))

(defn- read-byte-from-mapper-slot
  "Returns an unsigned byte from a provided mapper slot and address."
  ^long [slot ^long address ^bytes read-only-memory]
  (let [bank-data (slot @mapper-banks)
        total-banks (quot (alength read-only-memory) 16384)
        ;; Cleanly wraps around using mod calculation if the game is too small to have a third bank.
        ;; They smallest Master System game shuld be 32KB (or two banks each 16KB).
        safe-bank (mod bank-data total-banks)
        real-offset (+ (* safe-bank 16384) address)]
    (signed->unsigned (aget read-only-memory real-offset))))

;;NOTE: We need this function, because some ROM dumpers add a header.
(defn- detect-rom-header-offset
  "Scans raw ROM bytes for the magic 'TMR SEGA' string at standard hardware 
   offsets to determine if an extra 512-byte copier header is present."
  [^bytes rom-bytes]
  (let [;; Standard SMS header locations
        standard-offsets [0x7FF0 0x3FF0 0x1FF0]
        ;; Helper to check if a specific offset contains "TMR SEGA"
        has-magic-string? (fn [^long base-addr]
                            (and (<= (+ base-addr 7) (count rom-bytes))
                                 (= "TMR SEGA" 
                                    (String. rom-bytes base-addr 8 "US-ASCII"))))]
    (cond
      ;; If found at standard locations, this is a clean ROM (0 byte offset)
      (some has-magic-string? standard-offsets) 0
      ;; If found shifted forward by 512 bytes, this is a copier-headered ROM
      (some #(has-magic-string? (+ % 512)) standard-offsets) 512
      ;; Fallback default if the string is entirely missing (common in tiny test ROMs)
      :else 0)))

(defn load-rom-into-memory!
  "Calculates the correct data offset, allocates a perfectly sized
   byte array for the cartridge, and loads the raw machine code."
  [^bytes source-bytes]
  (let [header-offset (detect-rom-header-offset source-bytes)
        actual-code-len (- (count source-bytes) header-offset)
        new-target-array (byte-array actual-code-len)]
    
    (if (> header-offset 0)
      (println "Detected a 512-byte rom dumper header. Stripping offset...")
      (println "Detected raw/clean ROM structure."))
    
    ;; Copy clean binary code into properly sized array
    (System/arraycopy source-bytes header-offset new-target-array 0 actual-code-len)
    
    ;; Overwrite the global rom atom with this newly allocated array
    (reset! rom new-target-array)
    (println (format "Successfully loaded ROM into cartridge memory (%d KB)." (quot actual-code-len 1024)))))

(defn reset-emulator [^com.codingrodent.microprocessor.Z80.Z80Core cpu]
  ;; Reset the Sega Mapper to its standard power-on baseline state
  (reset! mapper-banks {:slot0 0
                        :slot1 1
                        :slot2 2})
  ;; Flush the Master System Work RAM completely
  (System/arraycopy (byte-array 8192) 0 sms-ram 0 8192)
  ;; Hard reset the CPU hardware states for a clean boot
  (.reset cpu)
  (.resetTStates cpu)
  (.setProgramCounter cpu rom-cart-start)) 

;; NOTE: It is worth noting: games that don't use the standard Sega mapper
;; will never write to the mapper registers (0xFFFD, 0xFFFE, 0xFFFF).
;; This means that for those games, the mapper banks will allways stay as:
;; :slot0 0
;; :slot1 1
;; :slot2 2
;; Having these three slots as 16KB chunks is really convenient,
;; because it maps cleanly to the SMS ROM Cartridge space (48KB):
;; Slot 0 = (0 * 16384 = 0x0000) - Ends at 0x4000
;; Slot 1 = (1 * 16384 = 0x4000) - Ends at 0x8000
;; Slot 2 = (2 * 16384 = 0x8000) - Ends at 0xBFFF
;; So basically they go from 0x0000 to 0xBFFF.
;; This means that our setup works with those games
;; that require the standard Sega mapper and those that do not.

(defn make-memory-bus []
  (reify IMemory
    (^int readByte [this ^int address]
      (let [^bytes active-rom @rom]
        (cond
          ;; --- SLOT 0 (0x0000 - 0x3FFF) ---
          (< address 0x4000)
          (if (< address 0x0400)
            ;; First 1KB is strictly reserved by the Z80 for interrupt handling routines.
            ;; This small space never gets swapped out.
            (signed->unsigned (aget active-rom address))
            ;; Remainder of Slot 0 uses the mapper bank
            (read-byte-from-mapper-slot :slot0 address active-rom))
          ;; --- SLOT 1 (0x4000 - 0x7FFF) ---
          (< address 0x8000) (read-byte-from-mapper-slot :slot1 (- address 0x4000) active-rom)
          ;; --- SLOT 2 (0x8000 - 0xBFFF) ---
          (< address ram-start) (read-byte-from-mapper-slot :slot2 (- address 0x8000) active-rom)
          ;; --- WORK RAM (0xC000 - 0xDFFF) ---
          (< address mram-start) (signed->unsigned (aget sms-ram (- address ram-start)))
          ;; --- RAM MIRROR (0xE000 - 0xFFFF) ---
          :else (signed->unsigned (aget sms-ram (- address mram-start))))))

    (^void writeByte [this ^int address ^int value]
      (cond
        ;; ROM Space is read-only
        (< address ram-start) nil 
        ;; Write to main Work RAM
        (< address mram-start) (aset-byte sms-ram (- address ram-start) (unchecked-byte value))
        ;; Write to Mirror RAM area & Mapper Registers
        :else
        (do
          ;; Mirror the write down into the actual 8KB Work RAM
          (aset-byte sms-ram (- address mram-start) (unchecked-byte value))
          ;; Intercept writes targeting the Mapper Registers (0xFFFD - 0xFFFF)
          ;; and fill our Clojure atom with the data.
          ;; The Sega Master System, uses Memory-Mapped I/O for its cartridge banking,
          ;; so it's the job of the Memory Bus to do this, not the IO Bus.
          (cond
            (= address 0xFFFD) (swap! mapper-banks assoc :slot0 value)
            (= address 0xFFFE) (swap! mapper-banks assoc :slot1 value)
            (= address 0xFFFF) (swap! mapper-banks assoc :slot2 value))))
      nil)

    (^int readWord [this ^int address]
      (let [low (.readByte this address)
            high (.readByte this (inc address))]
        (bit-or low (bit-shift-left high 8))))

    (^void writeWord [this ^int address ^int value]
      (.writeByte this address (signed->unsigned value))
      (.writeByte this (inc address) (signed->unsigned (bit-shift-right value 8)))
      nil)))
