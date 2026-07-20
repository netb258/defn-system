(ns z80.display
  (:require [z80.memory :as memory]
            [z80.color :as color]))

;; This module basically diggs through the VRAM and VDP registers and displays the information inside them on the screen.
;; Notice that this module does not include Quil. The reason is that all image processing is done on primitive arrays of bytes.
;; This way the performance is superior.

;; These dimensions should be accurate for a PAL console.
;; Notice that they do not do anything inside this module. Just felt like they belong here.
(def scale 4)
(def screen-width (* 256 scale))
(def screen-height (* 224 scale))

;; --------------------------------------------------------------------------------------------------
;; ------------------------------------- Background Display Code ------------------------------------
;; --------------------------------------------------------------------------------------------------

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

(defn- parse-background-data
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
        base-scroll-x (int (if (and vdp-regs (>= (alength vdp-regs) 9)) (memory/signed->unsigned (aget vdp-regs 8)) 0))
        raw-scroll-y (int (if (and vdp-regs (>= (alength vdp-regs) 10)) (memory/signed->unsigned (aget vdp-regs 9)) 0))
        ;; In 192-line mode, if Y scroll >= 224, it wraps around through 32 pixels instead of 256
        base-scroll-y (int (if mode-224? raw-scroll-y (if (>= raw-scroll-y 224) (mod raw-scroll-y 32) raw-scroll-y)))
        ;; --- Register 7: Border / Overscan Color ---
        reg7 (int (if (and vdp-regs (>= (alength vdp-regs) 8)) (aget vdp-regs 7) 0))
        border-palette-idx (bit-and reg7 0x0F) ; Lower 4 bits index the border color
        ;; SMS background palette entries always reside in the second 16-color slot (offset 16+)
        overscan-color (int (aget color-palette-cache (+ border-palette-idx 16)))]
    (->BackgroundData naming-table-start base-scroll-x base-scroll-y max-rows visible-height 
                    overscan-color h-scroll-lock? v-scroll-lock? hide-left-8?)))

(defn- draw-single-background-line-pixel!
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
    (fn [^long pixel-x ^long pixel-y col-v-locked? row-h-locked?]
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
            bg-x             (int (if row-h-locked? pixel-x (memory/signed->unsigned (- pixel-x base-scroll-x))))
            ;; Find which tile column (0-31) contains the targeted pixel x-coordinate
            map-tile-x       (int (quot bg-x 8))
            ;; Calculate the exact fine-X offset (0 to 7) inside that 8x8 graphic matrix cell
            tile-fine-x-act  (int (mod bg-x 8))
            
            ;; 3. READ NAME TABLE ENTRY FROM VRAM
            ;; Every background coordinate is represented by a 2-byte descriptor (16 bits)
            table-idx        (int (+ table-row-offset map-tile-x))
            addr-offset      (int (+ naming-table-start (* table-idx 2)))]
        
        (if (< addr-offset vram-len)
          (let [low-byte        (int (memory/signed->unsigned (aget vram-bytes addr-offset)))
                high-byte       (int (memory/signed->unsigned (aget vram-bytes (unchecked-inc addr-offset))))
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
                local-color-idx (int (color/get-sms-pixel-color-idx vram-bytes tile-index target-y target-x))
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
        color-palette-cache ^ints (color/get-vdp-color-palette cram-ints)
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

(defn- sprite-size-16? 
  "Checks Bit 1 of VDP Register 1 to see if sprites are 8x16.
   Returns:
   - true  : Sprites are 8x16 pixels.
   - false : Sprites are 8x8 pixels."
  [^z80.vdp.VdpState vdp-state]
  (let [vdp-regs ^ints (:regs vdp-state)
        reg1 (if (> (alength vdp-regs) 1) (aget vdp-regs 1) 0)]
    ;; Relies on the global atom @active-vdp. Extracts Bit 1 from Register 1.
    (not= 0 (bit-and reg1 0x02))))

(defn- get-sprite-tile-base
  "Checks Bit 2 of VDP Register 6 to determine if sprites start at 
   Tile index 0 (VRAM 0x0000) or Tile index 256 (VRAM 0x2000).
   Returns:
   - 256 : Sprites start at Tile index 256 (VRAM 0x2000).
   - 0   : Sprites start at Tile index 0 (VRAM 0x0000)."
  [^z80.vdp.VdpState vdp-state]
  (let [vdp-regs ^ints (:regs vdp-state)
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

(defn- parse-sprite-data
  "Parses VDP settings and bundles them with memory arrays into a single fast context object."
  [vdp vdp-regs ^bytes vram-bytes ^ints color-palette-cache ^ints img-pixels]
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
        large-sprites? (boolean (sprite-size-16? vdp))
        sprite-tile-base (int (get-sprite-tile-base vdp))]
    (SpriteData. visible-height sat-base-addr sat-info-table sprite-tile-base 
                          large-sprites? vram-bytes color-palette-cache img-pixels)))

(defn- draw-single-sprite-line!
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
                                  (+ (int (memory/signed->unsigned raw-tile-index)) tile-offset) 
                                  raw-tile-index))
        final-sprite-tile  (int (+ actual-tile-index sprite-tile-base))
        tile-fine-y        (int (mod fine-y 8))
        dest-row-offset    (int (* current-screen-y 256))]

    ;; 2. HORIZONTAL PIXEL LOOP
    (dotimes [x 8]
      (let [color-idx (int (color/get-sms-pixel-color-idx vram-bytes final-sprite-tile tile-fine-y (int x)))]
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
   Scans all 64 potential sprites and draws their line ONLY if they intersect the active scanline.
   Returns the updated image with matching sprites applied."
  [background-image ^z80.vdp.VdpState vdp-state scanline mode-224?]
  (let [vdp                 vdp-state
        vram-bytes          ^bytes (:vram vdp)
        cram-ints           ^ints (:cram vdp)
        vdp-regs            ^ints (:regs vdp)
        color-palette-cache ^ints (color/get-vdp-color-palette cram-ints)
        img-pixels          ^ints (.pixels ^processing.core.PImage background-image)
        vram-len            (int (alength vram-bytes))
        ;; Parse and gather VDP constraints into a single memory block record
        ^SpriteData ctx     (parse-sprite-data vdp vdp-regs vram-bytes color-palette-cache img-pixels)
        sat-base-addr       (int (.-sat-base-addr ctx))
        sat-info-table      (int (.-sat-info-table ctx))
        sprite-height       (int (if (.-large-sprites? ctx) 16 8))]

    ;; Loop through all 64 possible sprite descriptors stored inside the Sprite Attribute Table (SAT)
    (loop [sprite-id (int 0)]
      (when (< sprite-id 64)
        (let [y-addr (int (+ sat-base-addr sprite-id))
              raw-y  (int (if (< y-addr vram-len) (memory/signed->unsigned (aget vram-bytes y-addr)) 0))]
          
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
                      sprite-x       (int (if (< x-addr vram-len) (memory/signed->unsigned (aget vram-bytes x-addr)) 0))
                      raw-tile-index (int (if (< tile-addr vram-len) (memory/signed->unsigned (aget vram-bytes tile-addr)) 0))]
                  
                  ;; Fire single-row renderer for matching intersection targets
                  (draw-single-sprite-line! ctx sprite-x raw-tile-index fine-y scanline)))
              
              ;; Advance to parse the next sprite entry block
              (recur (inc sprite-id)))))))))
