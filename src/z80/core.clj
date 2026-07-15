(ns z80.core
  (:require [z80.vdp :as vdp]
            [quil.core :as q])
  (:import [com.codingrodent.microprocessor IMemory IBaseDevice]
           [com.codingrodent.microprocessor.Z80 Z80Core]
           [com.codingrodent.microprocessor.Z80 CPUConstants$RegisterNames])
  (:import java.awt.event.KeyEvent)
  (:gen-class))

;; --------------------------------------------------------------------------------------------------
;; ---------------------------------- Set Up MEMORY / IO BUS / CPU ----------------------------------
;; --------------------------------------------------------------------------------------------------

;; --------------------------------------------------------------------------------------------------
;; --------------------------------------------  MEMORY ---------------------------------------------
;; --------------------------------------------------------------------------------------------------

;; The program will perform a lot worse with reflection, so we should add type hints where possible.
;; (set! *warn-on-reflection* true)

;; --- SEGA MASTER SYSTEM MEMORY LAYOUT ---
;; The SMS has 64KB of total address space:
;; 0x0000 - 0xBFFF : ROM Cartridge space (48KB)
;; 0xC000 - 0xDFFF : System RAM (8KB)
;; 0xE000 - 0xFFFF : System RAM Mirror (Points to the same 8KB RAM)
;; They needed the mirror RAM addresses for convenience and also it apparantly saves on hardware.
(def rom-cart-start 0x0000)
(def rom-cart-end   0xBFFF)
(def ram-start      0xC000)
(def ram-end        0xDFFF)
(def mram-start     0xE000)
(def mram-end       0xFFFF)

;; (def ^{:tag 'bytes} rom (byte-array 49152))    ;; 48KB max for a basic ROM with no mapper.
;; Since we are now implementing the standard Sega Mapper our old static 48KB array needs to go.
(def rom (atom (byte-array 0)))
;; The stardard Sega Mapper splits the ROM space into 16kb pieces/slots
;; and dynamically loads parts of large games into the ROM space.
;; Track the current active bank index for each of the three 16KB slots
(def mapper-banks (atom {:slot0 0
                         :slot1 1
                         :slot2 2}))
(def ^{:tag 'bytes} sms-ram (byte-array 8192)) ;; 8KB of actual Work RAM

(defn signed->unsigned
  "Takes a signed byte (range -128 to 127) 
  and converts it to an unsigned byte (range 0 to 255)."
  ^long [^long signed-byte]
  (bit-and signed-byte 0xFF))

(defn read-byte-from-mapper-slot
  "Returns an unsigned byte from a provided mapper slot and address."
  ^long [slot ^long address ^bytes read-only-memory]
  (let [bank-data (slot @mapper-banks)
        total-banks (quot (alength read-only-memory) 16384)
        ;; Cleanly wraps around using mod calculation if the game is too small to have a third bank.
        ;; They smallest Master System game shuld be 32KB (or two banks each 16KB).
        safe-bank (mod bank-data total-banks)
        real-offset (+ (* safe-bank 16384) address)]
    (signed->unsigned (aget read-only-memory real-offset))))

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

(def memory-bus
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

;; --------------------------------------------------------------------------------------------------
;; --------------------------------------------- IO BUS ---------------------------------------------
;; --------------------------------------------------------------------------------------------------

;; --- SEGA MASTER SYSTEM I/O BUS ---
;; SMS components like Video (VDP) and Joypads are hooked up to the ports here.

;; Initialize a mutable reference to the VDP
(def active-vdp (atom (vdp/create-vdp)))

;; --- JOYPAD STATE MANAGEMENT ---
;; NOTE: This is a bit early, but the I/O Bus needs to see the joypads.
;; Default state is 0xFF (all bits 1 = all buttons unpressed)
(def joypad-p1 (atom 0xFF))
(def joypad-p2 (atom 0xFF))

(defn check-sprite-collision! [^z80.vdp.VdpState vdp]
  ;; In FluBBa's VDP test 47, Sprite 0 and Sprite 1 are defined at the very top of the SAT.
  ;; Let's inspect the first two entries in VRAM's Sprite Attribute Table.
  ;; Assuming default SAT location in VRAM (typically 0x3F00 for SMS, or check Reg 5/6)
  (let [^bytes vram-arr (.vram vdp)
        sat-base 0x3F00 ;; Standard SMS SAT base address
        ;; Read Y coordinates for Sprites 0 and 1
        y0 (signed->unsigned (aget vram-arr sat-base))
        y1 (signed->unsigned (aget vram-arr (inc sat-base)))
        ;; Read X coordinates (X-table starts 128 bytes after Y-table)
        x0 (signed->unsigned (aget vram-arr (+ sat-base 0x80)))
        x1 (signed->unsigned (aget vram-arr (+ sat-base 0x82)))]
    ;; Check if both sprites are active (Y != 0xD0 on SMS terminates sprite rendering)
    ;; and check if their coordinates overlap.
    (if (and (not= y0 0xD0) 
             (not= y1 0xD0)
             (= y0 y1) 
             (= x0 x1))
      (assoc vdp :sprite-collision? true)
      vdp)))

;; The io-bus will need to communicate with the CPU, even though we have not composed it yet.
(declare cpu)

(def io-bus
  (reify IBaseDevice
    (IORead [this address]
      (let [port (signed->unsigned address)]
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
            ;; Clear the interrupt line.
            (.setInterrupt ^com.codingrodent.microprocessor.Z80.Z80Core cpu false)
            @result)

          ;; Controller Ports
          (= port 0xDC) @joypad-p1
          (= port 0xDD) @joypad-p2
          :else 0xFF)))

    (IOWrite [this address data]
      (let [port (signed->unsigned address)]
        (cond
          (= port 0xBE) (swap! active-vdp vdp/data-write! (unchecked-byte data))
          ;; NOTE: port 0xBF pulls double duty depending on whether the Z80 CPU is writing to it or reading from it.
          ;; When reading, it serves as the status port. When writing it is the control port.
          (= port 0xBF) (swap! active-vdp vdp/control-write! data))
        nil))))

;; --------------------------------------------------------------------------------------------------
;; ------------------------------------------------ CPU ---------------------------------------------
;; --------------------------------------------------------------------------------------------------

;; Instantiate the CPU with both Memory and IO Bus
(def ^com.codingrodent.microprocessor.Z80.Z80Core cpu (Z80Core. memory-bus io-bus))


;; --------------------------------------------------------------------------------------------------
;; ------------------------------------------- ROM Loading ------------------------------------------
;; --------------------------------------------------------------------------------------------------

;;NOTE: We need this function, because some ROM dumpers add a header.
(defn detect-rom-header-offset
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

;; --------------------------------------------------------------------------------------------------
;; ------------------------------------- Background Display Code ------------------------------------
;; --------------------------------------------------------------------------------------------------

;; These dimensions should be accurate for a PAL console.
(def scale 4)
(def screen-width (* 256 scale))
(def screen-height (* 224 scale))

(defn get-sms-pixel-color-idx
  "Extracts the exact 4-bit color palette index (0-15) for a specific 
   horizontal pixel (0-7) in a 4bpp planar Sega Master System tile row."
  ^long [^bytes vram tile-index row-y pixel-x]
  (let [;; Each 8x8 tile is stored as 4bpp (4 bits per pixel). 
        ;; 8 pixels * 4 bits = 32 bits (4 bytes) per vertical row.
        ;; Therefore, a single 8x8 tile takes up exactly 32 bytes of VRAM.
        tile-base-addr (* tile-index 32)
        ;; Each vertical row within the tile spans 4 planar bytes.
        row-offset (* row-y 4)
        addr (+ tile-base-addr row-offset)
        ;; --- 4BPP PLANAR UNPACKING ---
        ;; The SMS uses a chunky-planar format where a single pixel's color index is 
        ;; sliced across 4 separate bitplanes (bytes). 
        ;; Byte 0 holds Bit 0 of all 8 pixels in the row.
        ;; Byte 1 holds Bit 1 of all 8 pixels in the row, etc.
        ;; Force absolute unsigned parsing to ensure color bits don't spill or break bounds.
        b0 (if (< addr (count vram)) (signed->unsigned (aget vram addr)) 0)
        b1 (if (< (inc addr) (count vram)) (signed->unsigned (aget vram (inc addr))) 0)
        b2 (if (< (+ addr 2) (count vram)) (signed->unsigned (aget vram (+ addr 2))) 0)
        b3 (if (< (+ addr 3) (count vram)) (signed->unsigned (aget vram (+ addr 3))) 0)
        ;; Pixels are ordered from Left to Right (MSB to LSB).
        ;; Pixel 0 maps to Bit 7 (shift right by 7), Pixel 7 maps to Bit 0 (shift right by 0).
        shift (- 7 pixel-x)
        ;; Extract the individual bit contribution for this target pixel from each plane
        bit0 (bit-and (bit-shift-right b0 shift) 0x01)
        bit1 (bit-and (bit-shift-right b1 shift) 0x01)
        bit2 (bit-and (bit-shift-right b2 shift) 0x01)
        bit3 (bit-and (bit-shift-right b3 shift) 0x01)]
    ;; Reconstruct the final 4-bit color index by combining the individual bits
    (bit-or bit0 
            (bit-shift-left bit1 1)
            (bit-shift-left bit2 2)
            (bit-shift-left bit3 3))))

(defn sms-color->rgb
  "Converts a 6-bit SMS color byte (00BBGGRR) to a standard 24-bit RGB vector."
  [sms-color-byte]
  ;; Guarantee that we mask out any signed integer junk before splitting bits
  (let [clean-byte (signed->unsigned (int sms-color-byte))
        r (bit-and clean-byte 0x03)                     ;; Lower 2 bits are Red
        g (bit-shift-right (bit-and clean-byte 0x0C) 2) ;; Middle 2 bits are Green
        b (bit-shift-right (bit-and clean-byte 0x30) 4) ;; Upper 2 bits are Blue
        ;; Each channel scales from a 0-3 range up to 0-255 for Quil:
        scale (fn [v] (int (* v 85)))]
    [(scale r) (scale g) (scale b)]))

(defn get-vdp-color-palette
  "Checks the entire 32-byte CRAM memory of the VDP and returns all 32 colors.
   Indices 0-15 are for backgrounds, indices 16-31 are for sprites."
  ^ints [^ints vdp-cram-as-int-array]
  (let [color-palette-cache (int-array 32)]
    (dotimes [i 32]
      (let [[r g b] (sms-color->rgb (aget vdp-cram-as-int-array i))]
        (aset color-palette-cache i (int (q/color r g b)))))
    color-palette-cache))

;; Ok time to do the actual drawing. Internally the Master System divides the TV screen into a matrix of 32 columns and 28 rows.
;; Each cell of this matrix is populated by an 8x8 pixel tile.
;; This matrix structure (also known as the Naming Table) is kept as flat bytes in the VDP's VRAM.
;; Also, this is why the base resolution is 256x224.
;; Width: 32 columns × 8 pixels = 256 pixels
;; Height: 28 rows × 8 pixels = 224 pixels
;; This is how the static background is handled. The actual moving sprites are a different story.

(defrecord BackgroundData
  [^int naming-table-start  ;; VRAM starting address for the Tile Map (Name Table)
   ^int base-scroll-x       ;; Hardware horizontal scroll offset (Register 8)
   ^int base-scroll-y       ;; Hardware vertical scroll offset (Register 9)
   ^int max-rows            ;; Max rows in the tile map (28 for 192-line mode, 32 for 224-line mode)
   ^int visible-height      ;; Vertical resolution in pixels (192 or 224)
   ^int overscan-color      ;; Border/Overscan color value fetched from the palette cache
   ^boolean h-scroll-lock?  ;; Disables horizontal scrolling for rows 0-15 (Register 0, Bit 6 - for game HUDs)
   ^boolean v-scroll-lock?  ;; Disables vertical scrolling for columns 24-31 (Register 0, Bit 7 - for game HUDs)
   ^boolean hide-left-8?])  ;; Blanks out the leftmost 8 pixels using the overscan color (Register 0, Bit 5)

(defn parse-background-data
  "Parses VDP registers and packs them into a single BackgroundData record."
  [^ints vdp-regs ^ints color-palette-cache]
  ;; --- Register 1: Video Mode Control ---
  (let [reg1 (int (if (and vdp-regs (>= (alength vdp-regs) 2)) (aget vdp-regs 1) 0))
        ;; Bit 3 of Register 1 determines if the display is in the extended 224-line mode
        mode-224? (not= 0 (bit-and reg1 0x08))
        visible-height (int (if mode-224? 224 192))
        max-rows (int (if mode-224? 32 28))
        ;; --- Register 2: Name Table Base Address ---
        reg2 (int (if (and vdp-regs (>= (alength vdp-regs) 3)) (aget vdp-regs 2) 0x0E))
        ;; Bits 1-3 determine the base VRAM address, shifted left by 10 (multiplied by 0x400 bytes)
        naming-table-start (int (bit-shift-left (bit-and reg2 0x0E) 10))
        ;; --- Register 0: Mode Control 1 ---
        reg0 (int (if (and vdp-regs (>= (alength vdp-regs) 1)) (aget vdp-regs 0) 0))
        h-scroll-lock? (not= 0 (bit-and reg0 0x40)) ; Bit 6: Locks horizontal scrolling for top 2 rows
        v-scroll-lock? (not= 0 (bit-and reg0 0x80)) ; Bit 7: Locks vertical scrolling for rightmost 8 columns
        hide-left-8?   (not= 0 (bit-and reg0 0x20)) ; Bit 5: Masks column 0 to hide scrolling artifacts
        ;; --- Registers 8 & 9: Scrolling Offsets ---
        base-scroll-x (int (if (and vdp-regs (>= (alength vdp-regs) 9)) (signed->unsigned (aget vdp-regs 8)) 0))
        raw-scroll-y (int (if (and vdp-regs (>= (alength vdp-regs) 10)) (signed->unsigned (aget vdp-regs 9)) 0))
        ;; In 192-line mode, if Y scroll >= 224, it wraps around through 32 pixels instead of 256
        base-scroll-y (int (if mode-224? raw-scroll-y (if (>= raw-scroll-y 224) (mod raw-scroll-y 32) raw-scroll-y)))
        ;; --- Register 7: Border / Overscan Color ---
        reg7 (int (if (and vdp-regs (>= (alength vdp-regs) 8)) (aget vdp-regs 7) 0))
        border-palette-idx (bit-and reg7 0x0F) ; Lower 4 bits index the border color
        ;; SMS background palette entries always reside in the second 16-color slot (offset 16+)
        overscan-color (int (aget color-palette-cache (+ border-palette-idx 16)))]
    (->BackgroundData naming-table-start base-scroll-x base-scroll-y max-rows visible-height 
                    overscan-color h-scroll-lock? v-scroll-lock? hide-left-8?)))

(defn draw-single-background-line-pixel!
  "Renders a single pixel for a scanline, factoring in horizontal/vertical locks, flips.
   Returns a high-performance closure meant to be run repeatedly across individual horizontal loops."
  [cfg ^bytes vram-bytes ^ints color-palette-cache ^ints img-pixels]
  ;; Extract layout configurations once to avoid map property lookups inside the hot inner pixel loop
  (let [naming-table-start (int (:naming-table-start cfg))
        base-scroll-x      (int (:base-scroll-x cfg))
        base-scroll-y      (int (:base-scroll-y cfg))
        max-rows           (int (:max-rows cfg))
        overscan-color     (int (:overscan-color cfg))
        hide-left-8?       (boolean (:hide-left-8? cfg))
        vram-len           (int (alength vram-bytes))]
    (fn [pixel-x pixel-y col-v-locked? row-h-locked?]
      ;; 1. VERTICAL AXIS LOOKUPS
      ;; If vertical scroll locking is active (for column entries >= 24), bypass VDP scroll offsets.
      (let [actual-bg-y      (int (if col-v-locked? pixel-y (+ pixel-y base-scroll-y)))
            ;; Find which tile row (0-27 or 0-31 depending on mode-224) contains the targeted pixel y-coordinate
            map-tile-y       (int (mod (quot actual-bg-y 8) max-rows))
            ;; Calculate the exact fine-Y offset (0 to 7) inside that 8x8 graphic matrix cell
            tile-fine-y-act  (int (mod actual-bg-y 8))
            ;; The VDP Name Table is exactly 32 tiles wide (holds data for 32 horizontal tile columns per tile row)
            table-row-offset (int (* map-tile-y 32))
            
            ;; 2. HORIZONTAL AXIS LOOKUPS
            ;; If horizontal scroll locking is active (for tile rows 0 and 1), bypass VDP scroll offsets.
            bg-x             (int (if row-h-locked? pixel-x (signed->unsigned (- pixel-x base-scroll-x))))
            ;; Find which tile column (0-31) contains the targeted pixel x-coordinate
            map-tile-x       (int (quot bg-x 8))
            ;; Calculate the exact fine-X offset (0 to 7) inside that 8x8 graphic matrix cell
            tile-fine-x-act  (int (mod bg-x 8))
            
            ;; 3. READ NAME TABLE ENTRY FROM VRAM
            ;; Every background coordinate is represented by a 2-byte descriptor (16 bits)
            table-idx        (int (+ table-row-offset map-tile-x))
            addr-offset      (int (+ naming-table-start (* table-idx 2)))]
        
        (if (< addr-offset vram-len)
          (let [low-byte        (int (signed->unsigned (aget vram-bytes addr-offset)))
                high-byte       (int (signed->unsigned (aget vram-bytes (unchecked-inc addr-offset))))
                ;; Bit 0 of High-Byte paired with Low-Byte creates the 9-bit pattern tile index (0 to 511)
                tile-index      (int (bit-or low-byte (bit-shift-left (bit-and high-byte 0x01) 8)))
                
                ;; 4. PARSE TILE DESCRIPTOR RENDERING FLAGS
                h-flip?         (not= 0 (bit-and high-byte 0x02))       ;; Bit 1: Flip tile pixels horizontally
                v-flip?         (not= 0 (bit-and high-byte 0x04))       ;; Bit 2: Flip tile pixels vertically
                use-palette-1?  (not= 0 (bit-and high-byte 0x08))       ;; Bit 3: Palette select (0 = Palette 0, 1 = Palette 1)
                palette-offset  (if use-palette-1? 16 0)                ;; System background colors reside in palette entries 16-31
                
                ;; Map fine coordinates depending on active flip vectors
                target-y        (int (if v-flip? (- 7 tile-fine-y-act) tile-fine-y-act))
                target-x        (int (if h-flip? (- 7 tile-fine-x-act) tile-fine-x-act))
                
                ;; Extract the 4-bit pixel color using the SMS planar unpacking routine (SMS patterns are stored in a 4bpp format)
                local-color-idx (int (get-sms-pixel-color-idx vram-bytes tile-index target-y target-x))
                color-idx       (+ local-color-idx palette-offset)
                pixel-color     (int (aget color-palette-cache color-idx))
                
                ;; 5. ENCODE METADATA FOR SPRITE LAYER PRIORITY
                ;; Extract Background Priority Flag (Bit 4 of Name Table High-Byte)
                bg-priority?    (not= 0 (bit-and high-byte 0x10))
                
                ;; - Bit 24: Store Priority State (0 = No priority, 1 = Priority active)
                ;; - Bits 25-28: Store local 4-bit palette color index (0 to 15, to determine background transparency)
                encoded-pixel   (bit-or (bit-and pixel-color 0x00FFFFFF) 
                                        (if bg-priority? 0x01000000 0x00000000)
                                        (bit-shift-left local-color-idx 25))
                
                ;; Compute linear destination index for the 256-wide SMS frame buffer
                dest-idx        (int (+ (* pixel-y 256) pixel-x))]
            
            ;; 6. WRITE PIXEL TO THE FRAME BUFFER
            ;; If Register 0 Bit 5 is checked, Column 0 (leftmost 8 pixels) blanks out to hide scrolling artifacts.
            (if (and hide-left-8? (< pixel-x 8))
              (aset img-pixels dest-idx overscan-color)
              (aset img-pixels dest-idx encoded-pixel)))
          
          ;; VRAM Safeguard: fall back to painting the official overscan color into the 256-wide buffer
          (aset img-pixels (int (+ (* pixel-y 256) pixel-x)) overscan-color))))))

(defn draw-background-line!
  "Renders only the background pixels for the current active scanline into the Quil image buffer."
  [^z80.vdp.VdpState vdp background-image scanline]
  (let [vram-bytes  ^bytes (:vram vdp)
        cram-ints   ^ints (:cram vdp)
        vdp-regs    ^ints (:regs vdp)
        ;; Pull the system palette configuration out of CRAM (Color RAM holding system colors)
        color-palette-cache ^ints (get-vdp-color-palette cram-ints)
        cfg (parse-background-data vdp-regs color-palette-cache)
        visible-height (int (:visible-height cfg))
        overscan-color (int (:overscan-color cfg))
        h-scroll-lock? (boolean (:h-scroll-lock? cfg))
        v-scroll-lock? (boolean (:v-scroll-lock? cfg))
        
        ;; Open the pixel array for direct, fast primitive writes.
        img-pixels ^ints (.pixels ^processing.core.PImage background-image)
        draw-pixel! (draw-single-background-line-pixel! cfg vram-bytes color-palette-cache img-pixels)
        
        pixel-y (int scanline)
        tile-y-int (int (quot pixel-y 8))
        ;; SMS feature: If bit 6 of Reg 0 is set, horizontal scrolling is locked for rows 0 and 1 (game Scoreboard/HUD)
        row-h-locked? (and h-scroll-lock? (< tile-y-int 2))]
    
    ;; Check if the current line falls within the currently active VDP video height (192 vs 224 mode)
    (if (< pixel-y visible-height)
      ;; LOOP 1: Line falls inside active resolution limits. Loop through all 32 hardware tile columns.
      (dotimes [screen-tile-x 32]
        (let [tile-x-int (int screen-tile-x)
              ;; SMS feature: If bit 7 of Reg 0 is set, vertical scrolling is locked for columns 24 to 31 (Side HUD)
              col-v-locked? (and v-scroll-lock? (>= tile-x-int 24))]
          ;; Loop across the 8 horizontal fine pixels belonging to this column block
          (dotimes [fx 8]
            (let [pixel-x (int (+ (* tile-x-int 8) fx))]
              (draw-pixel! pixel-x pixel-y col-v-locked? row-h-locked?)))))
      
      ;; LOOP 2: Handle offscreen color writing (e.g., lines 193-224 when running in 192-line mode)
      ;; Blanks out the remainder of the 224-tall texture canvas with the official overscan/border color
      (let [dest-row-offset (int (* pixel-y 256))]
        (dotimes [pixel-x 256]
          ;; Use standard aset for fast unboxed array modification
          (aset img-pixels (int (+ dest-row-offset pixel-x)) overscan-color))))
    background-image))


;; --------------------------------------------------------------------------------------------------
;; --------------------------------------- Sprite Display Code --------------------------------------
;; --------------------------------------------------------------------------------------------------

;;NOTE on sprites.
;; Unlike the background layer which uses a grid (Name Table), sprites can be placed at any pixel coordinate on the screen.
;; The VDP tracks them using a dedicated region of memory inside VRAM called the Sprite Attribute Table (SAT).
;; The SAT always sits at a specific location in VRAM (determined by VDP Register 5).
;; It can hold up to 64 sprites simultaneously and is divided into two distinct chunks of memory. They are:

;; Y-Coordinate Table (64 bytes): The first 64 bytes of the SAT contain the vertical Y positions for sprites 0 to 63.
;; X & Tile Info Table (128 bytes): This section follows immediately after the Y table. Every sprite gets 2 bytes here:
;; Byte 1: The horizontal X coordinate.
;; Byte 2: The Tile Index number (which 8x8 graphic to pull from VRAM

(defn sprite-size-16? 
  "Checks Bit 1 of VDP Register 1 to see if sprites are 8x16.
   Returns:
   - true  : Sprites are 8x16 pixels.
   - false : Sprites are 8x8 pixels."
  []
  (let [vdp-regs ^ints (:regs @active-vdp)
        reg1 (if (> (alength vdp-regs) 1) (aget vdp-regs 1) 0)]
    ;; Relies on the global atom @active-vdp. Extracts Bit 1 from Register 1.
    (not= 0 (bit-and reg1 0x02))))

(defn get-sprite-tile-base
  "Checks Bit 2 of VDP Register 6 to determine if sprites start at 
   Tile index 0 (VRAM 0x0000) or Tile index 256 (VRAM 0x2000).
   Returns:
   - 256 : Sprites start at Tile index 256 (VRAM 0x2000).
   - 0   : Sprites start at Tile index 0 (VRAM 0x0000)."
  []
  (let [vdp-regs ^ints (:regs @active-vdp)
        reg6 (if (and vdp-regs (>= (alength vdp-regs) 7)) (aget vdp-regs 6) 0)]
    ;; Bit 2 of Register 6 controls the base tile set offset (0 or 256)
    (if (not= 0 (bit-and reg6 0x04))
      256
      0)))

(defrecord SpriteData
  [^int visible-height       ;; Active vertical resolution (192 or 224 pixels)
   ^int sat-base-addr        ;; VRAM starting address for the Sprite Attribute Table (Y-coordinate list)
   ^int sat-info-table       ;; VRAM starting address for the X-coordinate and Tile index pairing table
   ^int sprite-tile-base     ;; Pattern generator base index modifier (0 or 256)
   ^boolean large-sprites?   ;; Sprite size configuration flag (true = 8x16, false = 8x8)
   ^bytes vram-bytes         ;; Direct reference to the raw VRAM byte array
   ^ints color-palette-cache  ;; Cached system palette colors mapped from CRAM
   ^ints img-pixels])        ;; Linear destination pixel array belonging to the Quil image

(defn parse-sprite-data
  "Parses VDP settings and bundles them with memory arrays into a single fast context object."
  [vdp-regs ^bytes vram-bytes ^ints color-palette-cache ^ints img-pixels]
  (let [;; Extract video display mode and height from Register 1, Bit 3
        reg1 (int (if (and vdp-regs (>= (alength ^ints vdp-regs) 2)) (aget ^ints vdp-regs 1) 0))
        mode-224? (not= 0 (bit-and reg1 0x08))
        visible-height (int (if mode-224? 224 192))
        ;; The Sprite Attribute Table (SAT) base is derived from Register 5.
        ;; Shifting (reg5 AND 0x7E) left by 7 bytes points to the Y-coordinate array.
        reg5 (if (and vdp-regs (>= (alength ^ints vdp-regs) 6)) (aget ^ints vdp-regs 5) 0x7E)
        sat-base-addr (int (bit-shift-left (bit-and (int reg5) 0x7E) 7))
        ;; The X-coordinate and Tile data starts exactly 128 bytes (0x80) past the SAT base.
        sat-info-table (int (+ sat-base-addr 0x80))
        large-sprites? (boolean (sprite-size-16?))
        sprite-tile-base (int (get-sprite-tile-base))]
    (SpriteData. visible-height sat-base-addr sat-info-table sprite-tile-base 
                          large-sprites? vram-bytes color-palette-cache img-pixels)))

(defn draw-single-sprite-line!
  "Renders only a SINGLE horizontal row of a SINGLE sprite matching the current scanline.
   This function draws one horizontal row of 8 pixels for one individual sprite on the current scanline.

   Note that the image buffer inside our SpriteData is a primitive array, ensuring high-speed rendering.
   Loops across the 8 horizontal pixels of the matching row to perform transparency,
   clipping, and priority evaluations against pre-rendered background metadata."
  [^SpriteData ctx pixel-x raw-tile-index fine-y current-screen-y]
  ;; Extract context fields directly from the primitive record to avoid reflection or map lookups
  (let [sx                 (int pixel-x)
        sprite-tile-base   (int (.-sprite-tile-base ctx))
        large-sprites?     (boolean (.-large-sprites? ctx))
        vram-bytes         ^bytes (.-vram-bytes ctx)
        color-palette-cache ^ints (.-color-palette-cache ctx)
        img-pixels         ^ints (.-img-pixels ctx)
        
        ;; 1. 8x16 SPRITE TILE INDEX CALCULATIONS
        ;; Real SMS Rule: In 8x16 mode, the tile index specified in the SAT applies to the top half.
        ;; The VDP automatically forces an index increment for the bottom half tile pattern row.
        tile-offset        (int (quot fine-y 8))
        actual-tile-index  (int (if large-sprites? 
                                  (+ (int (signed->unsigned raw-tile-index)) tile-offset) 
                                  raw-tile-index))
        final-sprite-tile  (int (+ actual-tile-index sprite-tile-base))
        tile-fine-y        (int (mod fine-y 8))
        dest-row-offset    (int (* current-screen-y 256))]

    ;; 2. HORIZONTAL PIXEL LOOP
    (dotimes [x 8]
      (let [color-idx (int (get-sms-pixel-color-idx vram-bytes final-sprite-tile tile-fine-y (int x)))]
        ;; Hardware Rule: Sprite color index 0 is always transparent and does not paint.
        (when (> color-idx 0)
          (let [current-screen-x (int (+ sx (int x)))]
            ;; Boundary protection against offscreen horizontal coordinates (viewport clipping)
            (when (and (>= current-screen-x 0) (< current-screen-x 256))
              (let [dest-idx         (int (+ dest-row-offset current-screen-x))
                    
                    ;; 3. DECODE BACKGROUND LAYER METADATA
                    ;; Wrap read values in unchecked-int to bypass safe integer conversion traps.
                    bg-pixel-raw     (unchecked-int (aget img-pixels dest-idx))
                    ;; Extract 4-bit color index stored at bits 25-28 of the background pixel
                    bg-color-idx     (int (bit-and (bit-shift-right bg-pixel-raw 25) 0x0F))
                    ;; Extract 1-bit background priority flag stored at bit 24
                    bg-has-priority? (not= 0 (bit-and bg-pixel-raw 0x01000000))
                    
                    ;; 4. EVALUATE LAYER MIXING PRIORITY
                    ;; The sprite wins if the background tile lacks priority,
                    ;; OR if the background tile has priority but this individual pixel is transparent (index 0).
                    should-draw?     (or (not bg-has-priority?) 
                                         (= bg-color-idx 0))]
                
                (when should-draw?
                  (let [;; SMS sprites map exclusively to the second 16-color block of the system palette
                        sprite-color-idx (int (+ color-idx 16))
                        pixel-color      (int (aget color-palette-cache sprite-color-idx))]
                    ;; Commit pixel to frame buffer array:
                    ;; - Keep bits 24-31 unchanged (retains background metadata for subsequent calculations)
                    ;; - Overwrite bits 0-23 with the new 24-bit sprite RGB values
                    (aset img-pixels dest-idx (bit-or (bit-and bg-pixel-raw 0xFF000000) 
                                                      (bit-and pixel-color 0x00FFFFFF)))))))))))))

(defn draw-all-sprites-line-for-scanline!
  "Takes a Quil image with the current background drawn on it.
   Basically draws a single line of all sprites matching the current scanline.
   Scans all 64 potential hardware sprites and draws their line ONLY if they intersect the active scanline.
   Returns the updated image with matching sprites applied."
  [background-image scanline mode-224?]
  (let [vdp                 @active-vdp
        vram-bytes          ^bytes (:vram vdp)
        cram-ints           ^ints (:cram vdp)
        vdp-regs            ^ints (:regs vdp)
        color-palette-cache ^ints (get-vdp-color-palette cram-ints)
        img-pixels          ^ints (.pixels ^processing.core.PImage background-image)
        vram-len            (int (alength vram-bytes))
        ;; Parse and gather VDP constraints into a single memory block record
        ^SpriteData ctx     (parse-sprite-data vdp-regs vram-bytes color-palette-cache img-pixels)
        sat-base-addr       (int (.-sat-base-addr ctx))
        sat-info-table      (int (.-sat-info-table ctx))
        sprite-height       (int (if (.-large-sprites? ctx) 16 8))]

    ;; Loop through all 64 possible sprite descriptors stored inside the Sprite Attribute Table (SAT)
    (loop [sprite-id (int 0)]
      (when (< sprite-id 64)
        (let [y-addr (int (+ sat-base-addr sprite-id))
              raw-y  (int (if (< y-addr vram-len) (signed->unsigned (aget vram-bytes y-addr)) 0))]
          
          ;; A vertical coordinate entry of 0xD0 signals the VDP to drop subsequent sprite calculations.
          ;; Real SMS Exception: This rule is disabled completely when running in extended 224-line mode.
          (when (or mode-224? (not= raw-y 0xD0))
            (let [;; Internal SMS quirk: Y positions in the SAT are offset by -1.
                  ;; Adding 1 aligns the execution cleanly to target screen spaces.
                  sprite-y (int (inc raw-y))
                  ;; Measure distance from current scanline to evaluate intersection matrix bounds
                  fine-y   (int (- scanline sprite-y))]
              
              ;; VERTICAL SCANLINE INTERSECTION EVALUATION
              (when (and (>= fine-y 0) (< fine-y sprite-height))
                (let [info-idx       (int (* sprite-id 2))
                      x-addr         (int (+ sat-info-table info-idx))
                      ;; In the secondary SAT structure, the pattern/tile index is the byte right after the X coordinate
                      tile-addr      (int (inc x-addr))
                      
                      ;; Fetch properties from secondary SAT tracking offset arrays (X coordinates & Tile indices)
                      sprite-x       (int (if (< x-addr vram-len) (signed->unsigned (aget vram-bytes x-addr)) 0))
                      raw-tile-index (int (if (< tile-addr vram-len) (signed->unsigned (aget vram-bytes tile-addr)) 0))]
                  
                  ;; Fire single-row renderer for matching intersection targets
                  (draw-single-sprite-line! ctx sprite-x raw-tile-index fine-y scanline)))
              
              ;; Advance to parse the next sprite entry block
              (recur (inc sprite-id)))))))))

;; --------------------------------------------------------------------------------------------------
;; --------------------------------------- Z80 Instruction Loop -------------------------------------
;; --------------------------------------------------------------------------------------------------

(defn vblank-irq-enabled?
  "Checks the VDP's internal Register 1 state to see if the Sega Master System 
   hardware has enabled V-Blank Frame Interrupt requests."
  []
  (let [vdp-regs ^ints (:regs @active-vdp)
        reg1 (if (> (alength vdp-regs) 1) (aget vdp-regs 1) 0)]
    ;; Bit 5 of VDP Register 1 enables the Frame Interrupt (V-Blank IRQ)
    (not= 0 (bit-and reg1 0x20))))

(defn hblank-irq-enabled?
  "Checks Bit 4 of VDP Register 0 to see if Line Interrupts (H-Blank IRQs) are enabled."
  []
  (let [vdp-regs ^ints (:regs @active-vdp)
        reg0 (if (> (alength vdp-regs) 0) (aget vdp-regs 0) 0)]
    (not= 0 (bit-and reg0 0x10))))

(defn get-vdp-reg10
  "Retrieves the value of VDP Register 10 (Scanline target)."
  []
  (let [vdp-regs ^ints (:regs @active-vdp)]
    (if (> (alength vdp-regs) 10) (aget vdp-regs 10) 0)))

;; Persistent frame canvas initialized matching standard dimensions 
(def global-frame-buffer (atom nil))

(defn do-instruction-loop!
  "Executes a single PAL frame scanline-by-scanline, updating imagery dynamically.
   
   Drives the entire emulator's timing loop. It synchronizes Z80 CPU execution slices
   with real-time raster scanline generations, allowing games to use mid-frame 
   register writes (raster effects) for split-screen parallax scrolling or palette swapping."
  [^com.codingrodent.microprocessor.Z80.Z80Core cpu
   ^com.codingrodent.microprocessor.IMemory memory-bus]
  (let [;; PAL Sega Master System metrics: 313 total scanlines per frame (0 to 312).
        lines-per-frame 313
        ;; Every scanline lasts exactly 228 CPU T-states (cycles). 
        ;; Total frame footprint = 313 lines * 228 cycles = 71,364 cycles per frame (~50Hz).
        cycles-per-line 228
        ;; Track the internal line counter used to calculate H-Blank (Line) Interrupt timings
        line-interrupt-counter (atom (get-vdp-reg10))
        ;; Extract the raw PImage canvas object out of the thread atom container once per frame
        frame-canvas ^processing.core.PImage @global-frame-buffer]
    
    ;; Open the direct 1D primitive pixel array for unchecked mutations
    (.loadPixels frame-canvas)
    
    (dotimes [scanline lines-per-frame]
      (let [start-tstates (.getTStates cpu)
            target-tstates (+ start-tstates cycles-per-line)]
        
        ;; Keep VDP state synchronized with the current hardware horizontal raster layer
        (swap! active-vdp assoc :current-scan-line scanline)
        
        ;; 1. PROCESS Z80 CPU INSTRUCTIONS FOR THIS SCANLINE
        ;; Step the Z80 processor repeatedly until it consumes exactly 228 cycle T-states.
        (loop []
          (when (< (.getTStates cpu) target-tstates)
            (.executeOneInstruction cpu)
            (recur)))
        
        ;; 2. RUN SYNCHRONOUS LINE RENDERING ENGINE
        ;; Only render within the standard 224-line high PImage texture canvas.
        (when (< scanline 224)
          (let [current-vdp @active-vdp
                vdp-regs    ^ints (:regs current-vdp)
                ;; Bit 3 of VDP Register 1 controls standard 192-line mode vs extended 224-line mode
                reg1        (int (if (and vdp-regs (>= (alength vdp-regs) 2)) (aget vdp-regs 1) 0))
                mode-224?   (not= 0 (bit-and reg1 0x08))
                active-limit (if mode-224? 224 192)]
            
            ;; ALWAYS draw the background line. This ensures that when the system is in 192-line mode,
            ;; lines 192 to 223 automatically drop into the overscan loop to draw a clean uniform border.
            (draw-background-line! current-vdp frame-canvas scanline)
            
            ;; ONLY compute foreground sprites and run collision grid checks during active video display lines
            (when (< scanline active-limit)
              (draw-all-sprites-line-for-scanline! frame-canvas scanline mode-224?)
              (swap! active-vdp check-sprite-collision!))))
        
        ;; 3. HANDLE H-BLANK COUNTER MECHANICS
        ;; The VDP line counter decrements on every active scanline.
        (if (<= scanline 192)
          (let [current-count @line-interrupt-counter
                new-count (dec current-count)]
            ;; When the counter underflows below 0, reset it from VDP Register 10 and request a CPU interrupt.
            (if (< new-count 0)
              (do
                (reset! line-interrupt-counter (get-vdp-reg10))
                (when (hblank-irq-enabled?)
                  (.setInterrupt cpu true)))
              (reset! line-interrupt-counter new-count)))
          ;; Outside the active window, the counter continually reloads from Register 10
          (reset! line-interrupt-counter (get-vdp-reg10)))
        
        ;; 4. TRIGGER FRAME V-BLANK INTERRUPT SIGNALS
        ;; Trigger V-Blank on line 193 (the first line after the standard active video window).
        (when (= scanline 193)
          (swap! active-vdp assoc :vblank-active? true)
          (when (vblank-irq-enabled?)
            (.setInterrupt cpu true)))
        
        ;; 5. END OF FRAME CLEANUP FILTER
        ;; On the very last scanline of the PAL cycle loop, sanitize the image buffer pixels.
        (when (= scanline 312)
          (swap! active-vdp assoc :vblank-active? false)
          (let [pixels-arr ^ints (.pixels frame-canvas)]
            (dotimes [i (alength pixels-arr)]
              ;; - (bit-and ... 0x00FFFFFF) completely strips out our internal layer sorting metadata mask.
              ;; - (bit-or 0xFF000000 ...) sets Alpha back to 0xFF (100% opaque) so Quil doesn't render it as black.
              ;; - Wrapped in unchecked-int to suppress Clojure's signed integer overflow arithmetic exceptions.
              (aset pixels-arr i (unchecked-int (bit-or 0xFF000000 
                                                        (bit-and (aget pixels-arr i) 0x00FFFFFF)))))))))
    
    ;; Finalize mutations and push the primitive pixel array modifications back into the Quil canvas
    (.updatePixels frame-canvas)))

;; --------------------------------------------------------------------------------------------------
;; ------------------------------------------- Quil Setup -------------------------------------------
;; --------------------------------------------------------------------------------------------------

(defn setup []
  ;; This should be the accurate FPS for a PAL console.
  (q/frame-rate 50)
  (reset! global-frame-buffer (q/create-image 256 224 :rgb))
  ;; Reset the Sega Mapper to its standard power-on baseline state
  (reset! mapper-banks {:slot0 0
                        :slot1 1
                        :slot2 2})
  ;; Flush the Master System Work RAM completely
  (System/arraycopy (byte-array 8192) 0 sms-ram 0 8192)
  ;; Hard reset the CPU hardware states for a clean boot
  (.reset cpu)
  (.resetTStates cpu)
  (.setProgramCounter cpu rom-cart-start) 
  (println "Sega Master System initialized with Sega Mapper support. Running real-time cycle loop..."))

(defn set-nearest-neighbor!
  "This function forces Nearest Neighbor sampling on all images.
  That should disable any antialiasing / filtering or texture smoothing.
  We are doing this, because Quil automatically applies Bilinear filtering on all upscaed images."
  []
  (.textureSampling (q/current-graphics) 2))

(defn draw []
  ;; 1. Execute Z80 code line-by-line while filling 'global-frame-buffer'
  (do-instruction-loop! cpu memory-bus)
  
  ;; 2. Force Nearest Neighbor sampling to preserve retro pixel art sharpness
  (set-nearest-neighbor!)
  
  ;; 3. Paint the fully constructed frame directly from the persistent buffer
  (q/image @global-frame-buffer 0 0 screen-width screen-height))

;; --------------------------------------------------------------------------------------------------
;; ----------------------------------------- JoyPad Handling ----------------------------------------
;; --------------------------------------------------------------------------------------------------

;; On a standard SMS joypad, 0 means pressed and 1 means unpressed (active low). When no buttons are held, ports 0xDC and 0xDD must return 0xFF.

;; The Z80 CPU will use ports 0xDC and 0xDD to interact with the joypads (basically it just reads data from them).
;; The two ports are responsible for the following: 
;; Port 0xDC (Data Port A): Covers Player 1 controls and part of Player 2.
;; Port 0xDD (Data Port B): Covers the rest of Player 2 and the Reset button.
;; When the Z80 executes an IN A, ($DC) instruction, it expects a byte where 0 means pressed and 1 means unpressed (active low).

(defn get-key []
  (let [raw-key (q/raw-key)
        the-key-code (q/key-code)
        the-key-pressed (if (= processing.core.PConstants/CODED (int raw-key)) the-key-code raw-key)]
    the-key-pressed))

;; Map keyboard characters/keywords to their respective hardware bits
(def p1-key-map
  {KeyEvent/VK_UP    0x01
   KeyEvent/VK_DOWN  0x02
   KeyEvent/VK_LEFT  0x04
   KeyEvent/VK_RIGHT 0x08
   \z                0x10   ;; Button 1 (Start/TL)
   \x                0x20}) ;; Button 2 (TR)

(def p2-key-map
  {\i 0x40   ;; P2 Up (shares port 0xDC bit 6)
   \k 0x80   ;; P2 Down (shares port 0xDC bit 7)
   \j 0x01   ;; P2 Left (on port 0xDD bit 0)
   \l 0x02   ;; P2 Right (on port 0xDD bit 1)
   \n 0x04   ;; P2 Button 1 (on port 0xDD bit 2)
   \m 0x08}) ;; P2 Button 2 (on port 0xDD bit 3)
 ;; P2 Button 2 (on port 0xDD bit 3)

(defn handle-key-press []
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
            (swap! joypad-p2 #(bit-and % (bit-not bit-p2)))))))))

(defn handle-key-release []
  (let [user-input (get-key)]
    ;; Player 1
    (when-let [bit-p1 (get p1-key-map user-input)]
      (swap! joypad-p1 #(bit-or % bit-p1)))
    ;; Player 2 (special case for up/down)
    (when-let [bit-p2 (get p2-key-map user-input)]
      (if (or (= user-input :up) (= user-input :down))
        (swap! joypad-p1 #(bit-or % bit-p2))
        (swap! joypad-p2 #(bit-or % bit-p2))))))

;; --------------------------------------------------------------------------------------------------
;; ------------------------------------------ Main function -----------------------------------------
;; --------------------------------------------------------------------------------------------------

(defn -main [& args]
  ;; The ROM path must be provided as a command line argument.
  (if (empty? args) (println "Please supply the path to a ROM as a command line argument.")
    (do (load-rom-into-memory! (java.nio.file.Files/readAllBytes (java.nio.file.Paths/get (first args) (into-array String []))))
        (q/defsketch sms-screen
          :title "DeFn System"
          :renderer :opengl
          :features [:exit-on-close]
          :key-pressed handle-key-press
          :key-released handle-key-release
          :size [screen-width screen-height]
          :setup setup
          :draw draw))))
