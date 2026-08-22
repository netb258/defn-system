(ns z80.display
  (:require [z80.memory :as memory]
            [z80.color :as color]))

;; This module basically diggs through the VRAM and VDP registers and displays the information inside them on the screen.
;; More info on the SMS VRAM layout can be found here:
;; https://www.smspower.org/Development/VRAMMemoryMap

;; More info on the VPD Registers can be found here: https://www.smspower.org/Development/VDPRegisters

;; Basically the Master System's VRAM is split up like this:
;; $0000 - $1FFF Graphics for tiles 0 - 255
;; $2000 - $37FF Graphics for tiles 256 - 447
;; $3800 - $3EFF Tilemap
;; $3F00 - $3FFF Sprite Attribute Table (SAT)

;; The code in this module needs to parse the information available in the VDP's registers, the TileMap and the SAT
;; and then draw the tiles located in VRAM addresses $0000 - $37FF.

;; Notice that this module does not include Quil. The reason is that all image processing is done on primitive arrays of bytes.
;; This way the performance is superior.
;; On that NOTE: The functions here GREATLY impact overall performance. Types will be explicitly added as often as possible.

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
;; For more info on the Naming Table (also called a Tile Map): https://www.smspower.org/Development/Tilemap
;; This is how the static background is handled. The actual moving sprites are a different story.

(defrecord BackgroundData
  [^int naming-table-start ;; VRAM starting address for the Tile Map (Name Table)
   ^int base-scroll-x      ;; Hardware horizontal scroll offset (Register 8)
   ^int base-scroll-y      ;; Hardware vertical scroll offset (Register 9)
   ^int max-rows           ;; Max rows in the tile map (28 for 192-line mode, 32 for 224-line mode)
   ^int visible-height     ;; Vertical resolution in pixels (192 or 224)
   ^int overscan-color     ;; Border/Overscan color value fetched from the palette cache
   ^boolean h-scroll-lock? ;; Disables horizontal scrolling for rows 0-15 (Register 0, Bit 6 - for game HUDs)
   ^boolean v-scroll-lock? ;; Disables vertical scrolling for columns 24-31 (Register 0, Bit 7 - for game HUDs)
   ^boolean hide-left-8?]) ;; Blanks out the leftmost 8 pixels using the overscan color (Register 0, Bit 5)

(defn- parse-background-data
  "Parses VDP registers and packs them into a single BackgroundData record."
  [^ints vdp-regs ^ints color-palette-cache]
  ;; --- Register 1: Video Mode Control ---
  (let [reg1 (int (aget vdp-regs 1))
        ;; Bit 3 of Register 1 determines if the display is in the extended 224-line mode
        mode-224? (not= 0 (bit-and reg1 2r00001000))
        visible-height (int (if mode-224? 224 192))
        max-rows (int (if mode-224? 32 28))
        ;; --- Register 2: Name Table Base Address ---
        reg2 (int (aget vdp-regs 2))
        ;; Bits 1-3 determine the base VRAM address and it gets multiplied by 1024 bytes (1KB).
        ;; This means that there are only 8 location where the naming table might start (3 bits represent up to dec 8).
        naming-table-start (int (* (bit-and reg2 2r00001110) 1024))
        ;; --- Register 0: Mode Control 1 ---
        reg0 (int (aget vdp-regs 0))
        h-scroll-lock? (not= 0 (bit-and reg0 2r01000000)) ;; Bit 6: Locks horizontal scrolling for top 2 rows
        v-scroll-lock? (not= 0 (bit-and reg0 2r10000000)) ;; Bit 7: Locks vertical scrolling for rightmost 8 columns
        hide-left-8?   (not= 0 (bit-and reg0 2r00100000)) ;; Bit 5: Masks column 0 to hide scrolling artifacts
        ;; --- Registers 8 & 9: Scrolling Offsets ---
        base-scroll-x (int (memory/signed->unsigned (aget vdp-regs 8)))
        raw-scroll-y (int (memory/signed->unsigned (aget vdp-regs 9)))
        ;; In 192-line mode, if Y scroll >= 224, it wraps around through 32 pixels instead of 256
        base-scroll-y (int (if mode-224? raw-scroll-y (if (>= raw-scroll-y 224) (mod raw-scroll-y 32) raw-scroll-y)))
        ;; --- Register 7: Border / Overscan Color ---
        reg7 (int (aget vdp-regs 7))
        border-palette-idx (bit-and reg7 2r00001111) ;; Lower 4 bits index the border color
        ;; SMS background palette entries always reside in the second 16-color slot (offset 16+)
        overscan-color (int (aget color-palette-cache (+ border-palette-idx 16)))]
    (->BackgroundData
       naming-table-start
       base-scroll-x
       base-scroll-y
       max-rows
       visible-height 
       overscan-color
       h-scroll-lock?
       v-scroll-lock?
       hide-left-8?)))

;; Apart from the info inside VDP registers (extracted by parse-background-data), the background drawing functions
;; will also need the info from entries in the Naming Table. These entries are described below:
;;
;; Every entry in the Name Table is composed of exactly 2 bytes (stored as Low Byte then High Byte):
;; 
;; Byte 0 (Low Byte / Tile Index)
;; [MSB                         LSB]
;; +---+---+---+---+---+---+---+---+
;; |          Tile Index           | 
;; +---+---+---+---+---+---+---+---+
;;   7   6   5   4   3   2   1   0
;;
;; Byte 1 (High Byte / Control flags)                 
;; [MSB                                                      LSB]          
;; +---+---+---+----------+---------+-------+-------+-----------+
;; |   Unused  | Priority | Palette | VFlip | HFlip | Tile Index|
;; +---+---+---+----------+---------+-------+-------+-----------+
;;   7   6   5      4          3        2       1         0                    
;;
;; Byte 1 Bits (high byte):
;; [7-5] - Unused / Reserved
;; [4]   - Priority (1 = Draw over sprites, 0 = Normal background)
;; [3]   - Palette selection (1 = Sprite palette, 0 = Background palette)
;; [2]   - Vertical flip (1 = Flip vertically)
;; [1]   - Horizontal flip (1 = Flip horizontally)
;; [0]   - Tile Index Bit 8 (Most Significant Bit of the 9-bit tile index)
;;
;; Byte 0 Bits (low byte):
;; All of them - Tile Index Bits 7-0 (Least Significant Bits of the 9-bit tile index)
;; Note, that the SMS can store around 500 tiles. 8 bits only represent 0-255 indexes.

(defn- draw-single-background-line-pixel!
  "Renders a single pixel for a scanline, factoring in horizontal/vertical locks, flips and more.
  Returns a high-performance closure meant to be run repeatedly across individual horizontal loops.
  Basically the whole thing is one big let form that does a lot of digging/calculating with the VDP's memory
  and sets a single pixel at the end.

  Tracks backtround/sprite drawing priority using Bit 24 (0x01000000) in the current Quil image layer.
  NOTE that bit 24 is perfectly safe to use. Quil keeps the frame buffer as an array of ints (32 bits per int).
  Out of these 32 bits, only 24 bits are the visible RGB values (bits 0 - 23).
  The last 8 bits (24 - 31) are the alpha channel, they are used for transparancy and are not visible.
  
  Usually the sprites get drawin in-front of the background.
  However, sometimes the background needs to be drawn in-front of the sprites.
  The corresponding sprite drawing function (draw-single-sprite-line!), checks bit 24 and draws accordingly."
  [^BackgroundData cfg ^bytes vram-bytes ^ints color-palette-cache ^ints img-pixels]
  ;; Extract layout configurations once to avoid map property lookups inside the hot inner pixel loop
  (let [naming-table-start (int (:naming-table-start cfg))
        base-scroll-x      (int (:base-scroll-x cfg))
        base-scroll-y      (int (:base-scroll-y cfg))
        max-rows           (int (:max-rows cfg))
        overscan-color     (int (:overscan-color cfg))
        hide-left-8?       (boolean (:hide-left-8? cfg))]
    (fn [^long pixel-x ^long pixel-y col-v-locked? row-h-locked?]
      ;; 1. VERTICAL AXIS LOOKUPS
      ;; If vertical scroll locking is active (for column entries >= 24), bypass VDP scroll offsets.
      (let [scrolled-y       (int (if col-v-locked? pixel-y (+ pixel-y base-scroll-y)))
            ;; Find which tile row (0-27 or 0-31 depending on mode-224) contains the targeted pixel y-coordinate
            tile-row         (int (mod (quot scrolled-y 8) max-rows))
            ;; Calculate the exact fine-Y offset (0 to 7) inside that 8x8 graphic matrix cell
            ;; NOTE: The pixel position within an 8x8 tile is typically called the FINE offset. We'll stick to that naming.
            fine-y           (int (mod scrolled-y 8))
            ;; The VDP Name Table is exactly 32 tiles wide (holds data for 32 horizontal tile columns per tile row)
            table-row-offset (int (* tile-row 32))

            ;; 2. HORIZONTAL AXIS LOOKUPS
            ;; If horizontal scroll locking is active (for tile rows 0 and 1), bypass VDP scroll offsets.
            scrolled-x      (int (if row-h-locked? pixel-x (memory/signed->unsigned (- pixel-x base-scroll-x))))
            ;; Find which tile column (0-31) contains the targeted pixel x-coordinate
            tile-col        (int (quot scrolled-x 8))
            ;; Calculate the exact fine-X offset (0 to 7) inside that 8x8 graphic matrix cell
            fine-x          (int (mod scrolled-x 8))

            ;; 3. READ NAME TABLE ENTRY FROM VRAM
            ;; Every background coordinate is represented by a 2-byte descriptor (16 bits)
            ;; NOTE: An entry in the naming table is typically called an NTE (naming table entry). We'll stick to that.
            nte-idx         (int (+ table-row-offset tile-col))
            nte-offset      (int (+ naming-table-start (* nte-idx 2)))

            low-byte        (int (memory/signed->unsigned (aget vram-bytes nte-offset)))
            high-byte       (int (memory/signed->unsigned (aget vram-bytes (unchecked-inc nte-offset))))
            ;; Bit 0 of High-Byte paired with Low-Byte creates the 9-bit pattern tile index (0 to 511)
            ;; The SMS can hold a total of 512 tiles (sprite and background tiles included).
            ;; With this index, the tiles are very easy to locate, as they start at VRAM address 0.
            vram-tile-index      (int (bit-or low-byte (bit-shift-left (bit-and high-byte 2r00000001) 8)))

            ;; 4. PARSE TILE DESCRIPTOR RENDERING FLAGS
            h-flip?         (not= 0 (bit-and high-byte 2r00000010)) ;; Bit 1: Flip tile pixels horizontally
            v-flip?         (not= 0 (bit-and high-byte 2r00000100)) ;; Bit 2: Flip tile pixels vertically
            use-palette-1?  (not= 0 (bit-and high-byte 2r00001000)) ;; Bit 3: Palette select (0 = Palette 0, 1 = Palette 1)
            palette-offset  (if use-palette-1? 16 0)                ;; System background colors reside in palette entries 16-31

            ;; Map fine coordinates depending on active flip vectors
            render-y        (int (if v-flip? (- 7 fine-y) fine-y))
            render-x        (int (if h-flip? (- 7 fine-x) fine-x))

            ;; Extract the 4-bit pixel color using the SMS planar unpacking routine (SMS patterns are stored in a 4bpp format)
            tile-color-idx  (int (color/get-sms-pixel-color-idx vram-bytes vram-tile-index render-y render-x))
            cram-idx        (+ tile-color-idx palette-offset)
            pixel-color     (int (aget color-palette-cache cram-idx))

            ;; 5. ADD METADATA FOR SPRITE LAYER PRIORITY
            ;; Extract Background Priority Flag (Bit 4 of Name Table High-Byte)
            bg-priority?    (not= 0 (bit-and high-byte 2r00010000))

            ;; - Bit 24: Store Priority State (0 = No priority, 1 = Priority active)
            ;; - Bits 25-28: Store local 4-bit palette color index (0 to 15, to determine background transparency)
            final-pixel   (bit-or (bit-and pixel-color 0x00FFFFFF) 
                                  (if bg-priority? 0x01000000 0x00000000)
                                  (bit-shift-left tile-color-idx 25))

            ;; Compute linear destination index for the 256-wide SMS frame buffer
            frame-buffer-idx (int (+ (* pixel-y 256) pixel-x))]

        ;; 6. WRITE PIXEL TO THE FRAME BUFFER
        ;; If Register 0 Bit 5 is checked, Column 0 (leftmost 8 pixels) blanks out to hide scrolling artifacts.
        (if (and hide-left-8? (< pixel-x 8))
          (aset img-pixels frame-buffer-idx overscan-color)
          (aset img-pixels frame-buffer-idx final-pixel))))))

(defn draw-background-line!
  "Renders only the background pixels for the current active scanline into the Quil image buffer."
  [^z80.vdp.VdpState vdp background-image scanline]
  (let [vram-bytes  ^bytes (:vram vdp)
        cram-ints   ^ints (:cram vdp)
        vdp-regs    ^ints (:regs vdp)
        ;; Pull the system palette configuration out of CRAM (Color RAM holding system colors)
        color-palette-cache ^ints (color/get-vdp-color-palette cram-ints)
        cfg ^BackgroundData (parse-background-data vdp-regs color-palette-cache)
        visible-height (int (:visible-height cfg))
        overscan-color (int (:overscan-color cfg))
        h-scroll-lock? (boolean (:h-scroll-lock? cfg))
        v-scroll-lock? (boolean (:v-scroll-lock? cfg))

        ;; Open the pixel array for direct, fast primitive writes.
        img-pixels ^ints (.pixels ^processing.core.PImage background-image)
        draw-pixel! (draw-single-background-line-pixel! cfg vram-bytes color-palette-cache img-pixels)

        ;; Remember we are drawing a line here so the y coordinates (the row index) stays static (we loop across x).
        pixel-y (int scanline)
        tile-y (int (quot pixel-y 8))
        ;; SMS feature: If bit 6 of Reg 0 is set, horizontal scrolling is locked for rows 0 and 1 (game Scoreboard/HUD)
        row-h-locked? (and h-scroll-lock? (< tile-y 2))]

    ;; Check if the current line falls within the currently active VDP video height (192 vs 224 mode)
    (if (< pixel-y visible-height)
      ;; LOOP 1: Render the 32 tile columns for this scanline.
      (dotimes [tile-x 32]
        (let [;; SMS feature: If bit 7 of Reg 0 is set, vertical scrolling is locked for columns 24 to 31 (Side HUD)
              col-v-locked? (and v-scroll-lock? (>= tile-x 24))]
          ;; Loop across the 8 horizontal fine pixels belonging to this tile column (each tile consists of 8x8 pixels).
          ;; NOTE: The pixel position within an 8x8 tile is typically called the FINE offset. We'll stick to that naming.
          (dotimes [fine-x 8]
            (let [pixel-x (int (+ (* tile-x 8) fine-x))]
              (draw-pixel! pixel-x pixel-y col-v-locked? row-h-locked?)))))

      ;; LOOP 2: Handle offscreen color writing (e.g., lines 193-224 when running in 192-line mode)
      ;; Blanks out the remainder of the 224-tall texture canvas with the official overscan/border color
      (let [render-row-offset (int (* pixel-y 256))]
        (dotimes [pixel-x 256]
          (aset img-pixels (int (+ render-row-offset pixel-x)) overscan-color))))
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
;; Byte 2: The Tile Index number (which 8x8 graphic to pull from VRAM)
;; Remember that the tiles start at VRAM address 0x00. So a simple index is very effective in fetching them.

;; NOTE on "Pattern Generator" 
;; Instead of storing full images, old consoles store a library of 8x8 shapes. This works like a mosaic puzzle.
;; The "Pattern Generator" is the region of VRAM where the actual raw, physical pixel graphics (the patterns/tiles) are stored.
;; The SAT is the blueprint that puts the puzzle together.

;; More info on the SAT can be found here: https://www.smspower.org/Development/Sprites

(defn- sprite-size-16? 
  "Checks Bit 1 of VDP Register 1 to see if sprites are 8x16.
   Returns:
   - true  : Sprites are 8x16 pixels.
   - false : Sprites are 8x8 pixels."
  [^z80.vdp.VdpState vdp-state]
  (let [vdp-regs ^ints (:regs vdp-state)
        reg1 (aget vdp-regs 1)]
    ;; Extracts Bit 1 from Register 1.
    (not= 0 (bit-and reg1 2r00000010))))

(defn- get-sprite-tile-base
  "Checks Bit 2 of VDP Register 6 to determine if sprites start at 
   Tile index 0 (VRAM 0x0000) or Tile index 256 (VRAM 0x2000).
   Returns:
   - 256 : Sprites start at Tile index 256 (VRAM 0x2000).
   - 0   : Sprites start at Tile index 0 (VRAM 0x0000)."
  [^z80.vdp.VdpState vdp-state]
  (let [vdp-regs ^ints (:regs vdp-state)
        reg6 (aget vdp-regs 6)]
    ;; Bit 2 of Register 6 controls the base tile set offset (0 or 256)
    (if (not= 0 (bit-and reg6 2r00000100))
      256
      0)))

(defrecord SpriteData
  [^int visible-height       ;; Active screen height (192, 224, or 240)
   ^int sat-base-addr        ;; VRAM address of Sprite Attribute Table (Y-coords)
   ^int sat-info-table       ;; VRAM address of Sprite Info Table (X-coords/Tiles)
   ^int sprite-tile-base     ;; Base index modifier for pattern generator (0 or 256)
   ^boolean large-sprites?   ;; Sprite size mode (false = 8x8, true = 8x16)
   ^bytes vram-bytes         ;; VRAM array reference
   ^ints color-palette-cache ;; System color palette cache
   ^ints img-pixels          ;; Framebuffer - pixel destination array
   ^boolean shift-sprites-left-8px?]) ;; An early shift in sprite positions is possible (VDP reg 0).

(defn- parse-sprite-data
  "Parses VDP registers and packs them into a single SpriteData record."
  [vdp vdp-regs ^bytes vram-bytes ^ints color-palette-cache ^ints img-pixels]
  (let [;; Extract video display mode and height from Register 1, Bit 3
        reg1             (int (aget ^ints vdp-regs 1))
        mode-224?        (not= 0 (bit-and reg1 2r00001000))
        visible-height   (int (if mode-224? 224 192))
        ;; The Sprite Attribute Table (SAT) base is derived from Register 5.
        ;; Shifting (reg5 AND 0x7E) left by 7 bytes points to the Y-coordinate array.
        reg5             (aget ^ints vdp-regs 5)
        sat-base-addr    (int (bit-shift-left (bit-and (int reg5) 2r01111110) 7))
        ;; The X-coordinate and Tile indexes starts exactly 128 bytes past the SAT base.
        sat-info-table   (int (+ sat-base-addr 128))
        large-sprites?   (boolean (sprite-size-16? vdp))
        sprite-tile-base (int (get-sprite-tile-base vdp))
        ;; Read Register 0 to check for the Early Clock (EC) Sprite Shift flag (Bit 3)
        reg0             (int (aget vdp-regs 0))
        shift-sprites-left-8px? (boolean (not= 0 (bit-and reg0 2r00001000)))]
    (SpriteData.
      visible-height
      sat-base-addr
      sat-info-table
      sprite-tile-base 
      large-sprites?
      vram-bytes
      color-palette-cache
      img-pixels
      shift-sprites-left-8px?)))

;; Apart from the info inside VDP registers (extracted by parse-sprite-data), the sprite drawing functions
;; will also need the info from the Sprite Attribute Table. The SAT is described below:
;;
;; NOTE: All coordinates below are global coordinates within the space 256x244.
;; They are NOT local 'fine' coordinates within the sprite.
;; 
;; The Y-Coordinate Table (64 Bytes):
;; The Sprite Attribute Table starts with a 64-byte block for Y-coordinates.
;; Each byte corresponds to one sprite index (0 to 63):
;;
;; Byte Y (Sprite Y-Position)
;; [MSB                          LSB]
;; +---+---+---+---+---+---+---+---+
;; |        Y - Coordinate         |
;; +---+---+---+---+---+---+---+---+
;;   7   6   5   4   3   2   1   0
;;
;; Bits 7-0 : Y-Coordinate of the sprite. In SEGA Master System hardware, sprites are always delayed by exactly 1 scanline.
;;            This delay (relative to position Y) means that emulators must increment Y buy 1, in order for Y to match correctly.
;;            Value of $D0 (208) acts as a terminator: stops processing subsequent sprites.
;;
;; The X-Coordinate & Tile Info Table (128 Bytes / 64 Pairs):
;; Located immediately after the Y block, this contains 64 pairs of bytes.
;; For any sprite *i*, the data is located at (SAT_Base + 128 + i*2):
;;
;; Byte 1 (Tile Index)                              Byte 0 (Sprite X-Position)
;; [MSB                          LSB]        [MSB                         LSB]
;; +---+---+---+---+---+---+---+---+        +---+---+---+---+---+---+---+---+
;; |          Tile Index           |        |         X - Coordinate        |
;; +---+---+---+---+---+---+---+---+        +---+---+---+---+---+---+---+---+
;;   7   6   5   4   3   2   1   0            7   6   5   4   3   2   1   0
;;
;; Byte 1 Bits:
;; [7-0]: Tile Index for the sprite (0 to 255). 
;;        If 8x16 sprites are enabled, Bit 0 is ignored/forced to 0 for the top tile.
;;
;; Byte 0 Bits:
;; [7-0]: X-Coordinate of the sprite.
;;
;; NOTE: This setup explains a lot of stuff going on in parse-sprite-data.
;; For example, the SAT tile info table is at: (+ sat-base-addr 128).
;; Also the get-sprite-tile-base function whould now be clear. 
;; The SAT contains only 1 byte for sprite tile indexes (0-255). But, the SMS can hold around 500 tiles.
;; We need an additional 1 bit from VDP register 6. This way we have 9 bits (0-511).

;; NOTE: The sprite drawing functions will also perform VDP collision detection.
(defn- draw-single-sprite-line!
  "Renders only a SINGLE horizontal row of a SINGLE sprite matching the current scanline.
   This function draws one horizontal row of 8 pixels for one individual sprite on the current scanline.

   Note that the image buffer inside our SpriteData is a primitive array, ensuring high-speed rendering.
   Loops across the 8 horizontal pixels of the matching row to perform transparency,
   clipping, and priority evaluations against pre-rendered background.
   
   Tracks hardware sprite-on-sprite pixel collisions using Bit 29 (0x20000000) in the current background layer.
   NOTE that bit 29 is perfectly safe to use. Quil keeps the frame buffer as an array of ints (32 bits per int).
   Out of these 32 bits, only 24 bits are the visible RGB values (bits 0 - 23).
   The last 8 bits (24 - 31) are the alpha channel, they are used for transparancy and are not visible.

   For every horizontal pixel in the sprite (8 in total)
   we are setting a single unused bit in the background layer (matching the same coordinates) to 1.
   When the function gets called again, to draw another sprite line,
   it will check the background layer bit and if it's already 1, then we have a sprite collision.

   NOTE, that we never clear the background layer bit to 0.
   It will be reset to 0 naturally when a new frame is drawn with a fresh background."
  [^SpriteData ctx pixel-x sat-tile-index tile-row scanline collision-atom]
  ;; Extract context fields directly from the primitive record to avoid reflection or map lookups
  (let [sprite-tile-base    (int (.-sprite-tile-base ctx))
        large-sprites?      (boolean (.-large-sprites? ctx))
        vram-bytes          ^bytes (.-vram-bytes ctx)
        color-palette-cache ^ints (.-color-palette-cache ctx)
        img-pixels          ^ints (.-img-pixels ctx)

        ;; 1. 8x16 SPRITE TILE INDEX CALCULATIONS
        tile-row-offset    (int (quot tile-row 8))
        sized-tile-index   (int (if large-sprites? 
                                  ;; SMS Hardware Rule: Force bit 0 to 0 for 8x16 sprites
                                  (+ (bit-and (int (memory/signed->unsigned sat-tile-index)) 2r11111110) tile-row-offset) 
                                  sat-tile-index))
        ;; The absolute, finalized index of the 8-byte graphics pattern (tile) inside VRAM. It will be a number between 0 and 511.
        ;; The SMS can hold a total of 512 tiles (sprite and background tiles included).
        ;; With this index, the tiles are very easy to locate, as they start at VRAM address 0.
        vram-tile-index    (int (+ sized-tile-index sprite-tile-base))
        ;; NOTE: The pixel position within an 8x8 tile is typically called the FINE offset. We'll stick to that naming.
        ;; Remember we are drawing a line here so the y coordinates (the row index) stays static (we loop across x).
        ;; NOTE: Typically tile-row would contain a value between 0 - 7 (which is exactly what we need).
        ;; However, If we are dealing with 8x16 sprites, then it will contain 0 - 15. Every tile in the SMS is drawn as a single 8x8 tile.
        ;; Even with 8x16 tiles, the system stores that as two 8x8 tiles in memory and only one 8x8 tile is ever drawn at a time.
        ;; This means (for 8x16) we should convert the 0 - 15 row indexes to 0 - 7 indexes. This is what the modulo math is doing.
        tile-fine-y        (if large-sprites? (mod tile-row 8) tile-row)
        render-row         (int (* scanline 256))]

    ;; 2. HORIZONTAL PIXEL LOOP
    (dotimes [tile-fine-x 8]
      (let [color-idx (int (color/get-sms-pixel-color-idx vram-bytes vram-tile-index tile-fine-y (int tile-fine-x)))]
        ;; Hardware Rule: Sprite color index 0 is always transparent and does not paint or cause collisions.
        (when (> color-idx 0)
          (let [current-screen-x (int (+ pixel-x (int tile-fine-x)))]
            ;; Boundary protection against offscreen horizontal coordinates (viewport clipping)
            (when (and (>= current-screen-x 0) (< current-screen-x 256))
              ;; All we really need for drawing are render-idx and pixel-color. The rest just do extra features like collision checking.
              (let [render-idx (int (+ render-row current-screen-x))
                    ;; SMS sprites map exclusively to the second 16-color block of the system palette
                    sprite-color-idx (int (+ color-idx 16))
                    pixel-color      (int (aget color-palette-cache sprite-color-idx))
                    ;; 3. DECODE BACKGROUND LAYER METADATA AND CHECK FOR COLLISION
                    ;; Wrap read values in unchecked-int to bypass safe integer conversion traps.
                    bg-pixel-raw     (unchecked-int (aget img-pixels render-idx))

                    ;; HARDWARE COLLISION CHECK: If Bit 29 is already set, another sprite's pixel is here!
                    ;; Remember: Quil keeps the image as an int array (32 bits per int). But only 24 bits are used for visible RGB.
                    _ (when (not= 0 (bit-and bg-pixel-raw 0x20000000))
                        (reset! collision-atom true))

                    ;; Inject Bit 29 into our metadata track to flag this coordinate for future sprites
                    marked-bg-pixel (unchecked-int (bit-or bg-pixel-raw 0x20000000))

                    ;; Extract 4-bit color index stored at bits 25-28 of the background pixel
                    bg-color-idx     (int (bit-and (bit-shift-right bg-pixel-raw 25) 2r00001111))
                    ;; Extract 1-bit background priority flag stored at bit 24
                    bg-has-priority? (not= 0 (bit-and bg-pixel-raw 0x01000000))

                    ;; 4. EVALUATE LAYER MIXING PRIORITY
                    should-draw?     (or (not bg-has-priority?) 
                                         (= bg-color-idx 0))]

                (if should-draw?
                  ;; Commit pixel to frame buffer array:
                  ;; - Keep bits 24-31 unchanged (retains background AND our newly set sprite collision metadata)
                  ;; - Overwrite bits 0-23 with the new 24-bit sprite RGB values
                  (aset img-pixels render-idx (bit-or (bit-and marked-bg-pixel 0xFF000000) 
                                                      (bit-and pixel-color 0x00FFFFFF)))
                  ;; Even if hidden by background priority, write back the marked metadata 
                  ;; because overlapping hidden sprites STILL trigger collisions!
                  (aset img-pixels render-idx marked-bg-pixel))))))))))

(defn draw-all-sprites-line-for-scanline!
  "Takes a Quil image with the current background drawn on it.
   Basically draws a single line of all sprites matching the current scanline.
   Scans all 64 potential sprites and draws their line ONLY if they intersect the active scanline.
   Returns the updated image with matching sprites applied."
  [background-image vdp-atom scanline mode-224?]
  (let [vram-bytes          ^bytes (:vram @vdp-atom)
        cram-ints           ^ints  (:cram @vdp-atom)
        vdp-regs            ^ints  (:regs @vdp-atom)
        color-palette-cache ^ints  (color/get-vdp-color-palette cram-ints)
        img-pixels          ^ints  (.pixels ^processing.core.PImage background-image)
        ;; Parse and gather VDP constraints into a single record
        ctx                 ^SpriteData (parse-sprite-data @vdp-atom vdp-regs vram-bytes color-palette-cache img-pixels)
        sat-base-addr       (int (.-sat-base-addr ctx))
        sat-info-table      (int (.-sat-info-table ctx))
        sprite-height       (int (if (.-large-sprites? ctx) 16 8))
        collision-triggered? (atom false)
        shift-sprites-left-8px? (boolean (.shift-sprites-left-8px? ctx))]

    ;; Loop through all 64 possible sprite descriptors stored inside the Sprite Attribute Table (SAT)
    ;; 1. First, find which sprites actually hit this scanline (up to the 8-sprite limit)
    ;; They will be returned as a verctor of maps.
    (let [matching-sprites
          (loop [sprite-id 0 acc []]
            (if (and (< sprite-id 64)) ; Stop at 64 sprites
              (let [y-addr (int (+ sat-base-addr sprite-id))
                    sat-y  (int (memory/signed->unsigned (aget vram-bytes y-addr)))]
                (if (and (not mode-224?) (= sat-y 0xD0)) acc ;; A vertical coordinate entry of 0xD0 signals the VDP to drop subsequent sprite calculations.
                  (let [sprite-y   (int (inc sat-y))
                        sprite-row (int (- scanline sprite-y))]
                    ;; VERTICAL SCANLINE INTERSECTION CHECK
                    (if (and (>= sprite-row 0) (< sprite-row sprite-height))
                      ;; Sprite intersects with scanline. Gather its data and continue.
                      (let [attr-idx       (int (* sprite-id 2))
                            x-addr         (int (+ sat-info-table attr-idx))
                            tile-addr      (int (inc x-addr))
                            sat-x          (int (memory/signed->unsigned (aget vram-bytes x-addr)))
                            sat-tile-index (int (memory/signed->unsigned (aget vram-bytes tile-addr)))
                            ;; Shift left by 8 pixels only when the VDP Early Clock (Register 0, Bit 3) is active.
                            base-x-offset  (int (if shift-sprites-left-8px? -8 0))
                            corrected-x    (+ sat-x base-x-offset)]
                        (recur (inc sprite-id) (conj acc {:x corrected-x :tile-idx sat-tile-index :sprite-row sprite-row})))
                      ;; Didn't intersect, just check the next sprite
                      (recur (inc sprite-id) acc)))))
              acc))]

      ;; 2. DRAW BACKWARD: Iterate from the end of our list back to index 0
      ;; This guarantees Sprite 0 details overwrite higher indices on the canvas!
      (doseq [sprite (reverse (take 8 matching-sprites))]
        (draw-single-sprite-line! ctx (:x sprite) (:tile-idx sprite) (:sprite-row sprite) scanline collision-triggered?))

      ;; If the program tried to do more than 8 sprites, then the VDP must keep track of that.
      (when (> (count matching-sprites) 8)
        (swap! vdp-atom assoc :sprite-overflow? true)))

    ;; Update the VDP flag if draw-single-sprite-line! indicated there was a sprite collision.
    (when @collision-triggered?
      (swap! vdp-atom assoc :sprite-collision? true))))
