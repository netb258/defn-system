(ns z80.memory
  (:require [clojure.java.io :as io])
  (:import [com.codingrodent.microprocessor IMemory]))

;; The emulator can set this atom to true if the user is running a Game Gear ROM.
;; I didn't know where to put this one. Ultimately chose the memory module.
(def gg-rom-selected? (atom false))

;; NOTE: A complete Memory Map can be found here: https://www.smspower.org/Development/MemoryMap

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

;; (def ^{:tag 'bytes} rom (byte-array 49152)) ;; 48KB max for a basic ROM with no mapper.
;; Since we are now implementing the standard Sega Mapper our old static 48KB array needs to go.
;; On real hardware, once the use plugs in a cart, the cpu can freely access the cart's ROM memory.
(def ^:private rom (atom (byte-array 0)))

;; The stardard Sega Mapper splits the ROM space into 16kb pieces/slots
;; and dynamically indexes parts of large games into the ROM space.
;; Track the current active bank index for each of the three 16KB slots.
;; On real hardware, this small memory would be on the mapper chip.
;; Games that come with the standard Sega Mapper will include instruction that write to this memory.
(def ^:private mapper-banks (atom {:slot0 0
                                   :slot1 1
                                   :slot2 2}))

(def ^:private sms-ram (atom (byte-array 8192))) ;; 8KB of actual Work RAM

;; NOTE: We're going to be using these two functions a lot. Notice that the above ROM and RAM is defined as (byte-array).
;; This creates a problem. The original hardware works with unsigned bytes (0 to 255).
;; However, Java (and Clojure) work with signed bytes (-128 to 127) by default.

;; Also, this memory (especially the RAM) will be read-from/written-to many times a second. This creates another hurdle.
;; Even though all the memory is efficient byte arrays like this (byte-array 8192), we cannot simply write with (aset).
;; The function aset is a generic function that uses object boxing and will be slow. We need to use (aset-byte) for speed.
;; However, aset-byte only works with signed bytes. We are going to have to pass incoming bytes to (unsigned->signed).
;; This way any input byte to (aset-byte) will be transformed into a signed byte.

;; Basically our access to system memory will look like this:
;; When the hardware tries to write an unsigned byte to our (byte-array), we convert it to signed with (unsigned->signed).
;; When the hardware tries to read from our (byte-array), we convert the read byte to unsigned with (signed->unsigned)

(defn signed->unsigned
  "Takes a signed byte (range -128 to 127) 
  and converts it to an unsigned byte (range 0 to 255)."
  [^long signed-byte]
  (bit-and signed-byte 0xFF))

(defn unsigned->signed
  "Takes an unsigned byte (range 0 to 255) 
  and converts it to a signed byte (range -128 to 127)."
  [^long unsigned-byte]
  (unchecked-byte unsigned-byte))

;; When the user inserts a cartridge into the console, the Z80 CPU can freely read from the cart's memory (ROM).
;; However, the Z80 can only access 64K addresses.
;; If the cart contains more memory than 64KB, then the Z80 cannot access any data beyond those 64K addresses.
;; To overcome this limitation, SMS cartridges came with swappable memory banking.

;; This flexible memory scheme was implemented by the standard Sega mapper chip, which was used in 99% of carts.
;; The below function and the @mapper-banks atom, implement this simple and elegant memory scheme.

(defn- read-byte-from-mapper-slot
  "Returns an unsigned byte from a provided mapper slot and address.
  The 'slot' parameter should be one of these keywords:
  :slot0, :slot1, :slot2."
  ^long [slot ^long address ^bytes read-only-memory]
  (let [bank-idx (slot @mapper-banks)
        total-banks (quot (alength read-only-memory) 16384)
        ;; Cleanly wraps around using mod calculation if the game is too small to have a third bank.
        ;; The smallest Master System game shuld be 32KB (or two banks each 16KB).
        ;; These games should never read from slot 2. If bank-idx is 2 and total-banks is 2, the mod will return 0.
        safe-bank (mod bank-idx total-banks)
        real-offset (+ (* safe-bank 16384) address)]
    (signed->unsigned (aget read-only-memory real-offset))))

;; --------------------------------------------------------------------------------------------------
;; -------------------------------------- Battery Save Functions ------------------------------------
;; --------------------------------------------------------------------------------------------------

;; Port 0xFFFC - Control SRAM state (default 0). Tracks if SRAM is enabled.
(def ^:private sram-control (atom 0))

(defn- md5-hash
  "Takes a ROM as a byte-array and returns it's MD5 hash as a string."
  [^bytes rom-bytes]
  (let [md (java.security.MessageDigest/getInstance "MD5")]
    (.update md rom-bytes)
    (format "%032x" (java.math.BigInteger. 1 (.digest md)))))

(defn get-rom-md5-hash []
  (md5-hash @rom))

;; NOTE: We are using delay, because we want to wait for the @rom to be loaded by load-rom-into-memory!
(def ^:private sram-file-path (delay (str (get-rom-md5-hash) ".sav")))

;; Standard SMS Cartridge RAM is usually 8KB or 16KB.
;; However, some Game Gear games come with 32KB of SRAM (for example Shining Force).
;; Allocating 32KB covers the needs of both consoles.
(def ^:private cart-sram 
  (delay
    (let [file (io/file @sram-file-path)]
      (if (.exists file)
        (with-open [xin (io/input-stream file)]
          (let [buf (byte-array 32768)]
            (.read xin buf)
            buf))
        (byte-array 32768)))))

(defn save-sram-to-disk!
  "Flushes the current in-memory Cartridge SRAM to a local file.
   The whole operation is skipped if SRAM is empty."
  []
  (when (not (every? zero? @cart-sram))
    (with-open [xout (io/output-stream @sram-file-path)]
      (.write xout ^bytes @cart-sram))))

(defn- sram-enabled? 
  "Returns true if the SRAM enable bit (Bit 3) was set in port 0xFFFC."
  []
  (not= 0 (bit-and @sram-control 2r00001000)))

(defn- get-sram-offset
  "Calculates the offset in SRAM based on Address and Bank Select bit (Bit 2).
   This is necessary, because the Sega Mapper is active for SRAM as well and will break it up into banks.
   The mapper chip can only break up SRAM into two banks."
  ^long [^long address]
  (let [slot-size   16384
        slot2-start 0x8000
        bank (if (not= 0 (bit-and @sram-control 2r00000100)) slot-size 0)
        sram-relative-addr (- address slot2-start)]
    (+ bank sram-relative-addr)))

;; --------------------------------------------------------------------------------------------------
;; -------------------------------------- ROM Loading Functions -------------------------------------
;; --------------------------------------------------------------------------------------------------

;;NOTE: We need this function, because some ROM dumpers add a header.
(defn- detect-rom-header-offset
  "Scans raw ROM bytes for the magic 'TMR SEGA' string to determine
   if an extra 512-byte copier header is present.
   This function takes a ROM as a byte-array and returns either 0 or 512.
   Returns 0 if the ROM is clean.
   Returns 512 if the ROM has a dumper header that needs to be removed."
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
  "Takes a byte-array and loads it into the private @rom atom.
   Any header from a ROM dumper is omitted."
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

;; --------------------------------------------------------------------------------------------------
;; ------------------------------------------ Serialization  ----------------------------------------
;; --------------------------------------------------------------------------------------------------

;; This module knows how to serialize and deserialize the system memory.

(defn- save-bytes-to-file
  "Saves a byte-array (byte-arr) to a file on disk (file-path)."
  [file-path byte-arr]
  (with-open [out (io/output-stream file-path)]
    (.write out byte-arr)))

(defn- load-bytes-from-file
  "Takes the path to a file as a string.
   Returns the contents of the file as a byte-array."
  [file-path]
  (with-open [in (io/input-stream file-path)]
    (let [buf (byte-array (.length (io/file file-path)))]
      (.read in buf)
      buf)))

(defn- write-ds-to-file
  "Saves a Clojure data structure (ds) to a file (file-path).
   The data structure is saved in EDN fromat."
  [file-path ds]
  (spit file-path (with-out-str (pr ds))))

(defn- read-ds-from-file
  "Returns a Clojure data structure that is read from an END file (file-path)."
  [file-path]
  (read-string (slurp file-path)))

(defn serialize-ram! [file-path]
  (save-bytes-to-file file-path @sms-ram))

(defn deserialize-ram! [file-path]
  (let [ram-on-disk (load-bytes-from-file file-path)]
    (reset! sms-ram ram-on-disk)))

(defn serialize-sram! [file-path]
  (when (sram-enabled?)
    (save-bytes-to-file file-path @cart-sram)))

(defn deserialize-sram! [file-path]
  (when (sram-enabled?)
    (let [sram-on-disk (load-bytes-from-file file-path)]
      (System/arraycopy sram-on-disk 0 @cart-sram 0 32768))))

(defn serialize-mapper! [file-path]
  (write-ds-to-file file-path @mapper-banks))

(defn deserialize-mapper! [file-path]
  (reset! mapper-banks (read-ds-from-file file-path)))

;; --------------------------------------------------------------------------------------------------
;; -------------------------------------- Memory Bus constructor  -----------------------------------
;; --------------------------------------------------------------------------------------------------

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

(defn make-memory-bus
  "Returns a complete Memory object that can be used by Z80Core to compose a CPU object.
  The CPU object shuld be able to read-from/write-to the memory addresses defined here."
  []
  (reify IMemory
    (^int readByte [this ^int address]
      (let [^bytes active-rom @rom
            ^bytes active-ram @sms-ram
            ^bytes active-sram @cart-sram]
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
          ;; --- SLOT 2 / SRAM SPACE (0x8000 - 0xBFFF) ---
          (< address ram-start)
          (if (sram-enabled?)
            (signed->unsigned (aget active-sram (get-sram-offset address)))
            (read-byte-from-mapper-slot :slot2 (- address 0x8000) active-rom))
          ;; --- WORK RAM (0xC000 - 0xDFFF) ---
          (< address mram-start) (signed->unsigned (aget active-ram (- address ram-start)))
          ;; --- RAM MIRROR (0xE000 - 0xFFFF) ---
          :else (signed->unsigned (aget active-ram (- address mram-start))))))

    (^void writeByte [this ^int address ^int value]
      (let [^bytes active-ram @sms-ram
            ^bytes active-sram @cart-sram]
        (cond
          ;; Write to Slot 2 SRAM (if enabled by the game)
          (and (>= address 0x8000) (< address ram-start) (sram-enabled?))
          (do
            (aset-byte active-sram (get-sram-offset address) (unsigned->signed value)))
          ;; ROM Space is otherwise read-only
          (< address ram-start) nil 
          ;; Write to main Work RAM
          (< address mram-start) (aset-byte active-ram (- address ram-start) (unsigned->signed value))
          ;; Write to Mirror RAM area & Mapper Registers
          :else
          (do
            ;; Mirror the write down into the actual 8KB Work RAM
            (aset-byte active-ram (- address mram-start) (unsigned->signed value))
            ;; Intercept writes targeting the Mapper Registers (0xFFFD - 0xFFFF)
            ;; and fill our Clojure atom with the data.
            ;; The Sega Master System, uses Memory-Mapped I/O for its cartridge banking,
            ;; so it's the job of the Memory Bus to do this, not the IO Bus.
            (cond
              (= address 0xFFFC) (reset! sram-control value)
              (= address 0xFFFD) (swap! mapper-banks assoc :slot0 value)
              (= address 0xFFFE) (swap! mapper-banks assoc :slot1 value)
              (= address 0xFFFF) (swap! mapper-banks assoc :slot2 value)))))
      nil)

    (^int readWord [this ^int address]
      (let [low (.readByte this address)
            high (.readByte this (inc address))]
        (bit-or low (bit-shift-left high 8))))

    (^void writeWord [this ^int address ^int value]
      (.writeByte this address (signed->unsigned value))
      (.writeByte this (inc address) (signed->unsigned (bit-shift-right value 8)))
      nil)))
