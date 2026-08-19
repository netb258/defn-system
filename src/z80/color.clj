(ns z80.color
  (:require [z80.memory :as memory]
            [quil.core :as q]))

(defn- sms-color->rgb
  "Converts a 6-bit SMS color byte (00 BB GG RR) to a standard 0-255 RGB vector.
   The Master System encodes each RGB value with only 4 possible intensities: 0, 1, 2 or 3.
   That means we only get 64 possible colors (4 x 4 x 4 = 64).
   The function is shifting all RGB bit pairs to the right, because we need a result of 0-3.
   In other words, each RGB value should be decoded into one of these:
   2r00000000 - 0
   2r00000001 - 1
   2r00000010 - 2
   2r00000011 - 3"
  [sms-color-byte]
  (let [clean-byte (memory/signed->unsigned (int sms-color-byte))
        r (bit-and clean-byte 2r00000011)                     ;; Lower 2 bits are Red
        g (bit-shift-right (bit-and clean-byte 2r00001100) 2) ;; Middle 2 bits are Green
        b (bit-shift-right (bit-and clean-byte 2r00110000) 4) ;; Upper 2 bits are Blue
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


;; The below function (get-sms-pixel-color-idx) is very important to all graphical output.
;; In order to understand this function, we must understand how the Master System stores tiles in VRAM.
;; A tile is basically just a 8x8 grid of colored squares (blocks).
;; However, inside VRAM, tiles are stored like this:
;;
;; Every tile in VRAM (the tiles start at address 0), is composed of exactly 8 of these rows (4 byte rows):
;; 
;; Byte 3 (Bitplane 3)       Byte 2 (Bitplane 2)       Byte 1 (Bitplane 1)       Byte 0 (Bitplane 0)
;; [MSB                 LSB] [MSB                 LSB] [MSB                 LSB] [MSB                 LSB]
;; +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
;; |P0|P1|P2|P3|P4|P5|P6|P7| |P0|P1|P2|P3|P4|P5|P6|P7| |P0|P1|P2|P3|P4|P5|P6|P7| |P0|P1|P2|P3|P4|P5|P6|P7|
;; +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
;; 
;; The bytes above contain encoded indexes into the VDP's CRAM.
;; These CRAM indexes have to be fetched and combined across all 4 bytes in the row.
;; 
;; For example, in order to get the color (as a CRAM index) for pixel 2, we need to combine all bits above marked as P2.

(defn get-sms-pixel-color-idx
  "Extracts the exact 4-bit color palette index (0-15) for a specific 
   horizontal pixel (0-7) in a 4bpp planar Sega Master System tile row.

   The Master System's VDP does not store colors directly in VRAM.
   It has separate memory for this purpose, called CRAM, which holds 32 colors at any moment (32 bytes of CRAM).
   In order to tie the pixels in VRAM with the colors in CRAM, the SMS uses a 4bit indexing scheme encoded in VRAM.
   Four bits have 16 possible values. The first half of CRAM (0-15) is used for background colors. The rest is used for sprite colors.
   This function returns the CRAM color index for a specific pixel encoded in VRAM.
   Note, that the function works as is when drawing background images. When drawing sprites, 16 needs to be added to the returned index.

   Basically, get-vdp-color-palette returns an array and this function retruns an index in that array.
   Note, that the pixel position within an 8x8 tile is typically called the FINE offset. We'll stick to that naming.
   The parameter 'pixel-fine-y' is basically the tile's row.
   Also 'pixel-fine-x' is the tile's col."
  ^long [^bytes vram ^long vram-tile-index ^long pixel-fine-y ^long pixel-fine-x]
  (let [;; Each 8x8 tile is stored as 4bpp (4 bits per pixel). 
        ;; 8 pixels * 4 bits = 32 bits (4 bytes) per vertical row.
        ;; Therefore, a single 8x8 tile takes up exactly 32 bytes of VRAM.
        tile-base-addr (* vram-tile-index 32)
        ;; Each vertical row within the tile spans 4 planar bytes.
        row-offset (* pixel-fine-y 4)
        addr (+ tile-base-addr row-offset)
        ;; --- 4BPP PLANAR UNPACKING ---
        ;; The SMS uses a chunky-planar format where a single pixel's color index is 
        ;; sliced across 4 separate bitplanes (bytes). 
        ;; Byte 0 holds Bit 0 of all 8 pixels in the row.
        ;; Byte 1 holds Bit 1 of all 8 pixels in the row.
        ;; Byte 2 holds Bit 2 of all 8 pixels in the row.
        ;; Byte 3 holds Bit 3 of all 8 pixels in the row.
        ;; Force absolute unsigned parsing to ensure color bits don't spill or break bounds.
        byte0 (memory/signed->unsigned (aget vram addr))
        byte1 (memory/signed->unsigned (aget vram (inc addr)))
        byte2 (memory/signed->unsigned (aget vram (+ addr 2)))
        byte3 (memory/signed->unsigned (aget vram (+ addr 3)))
        ;; Pixels are ordered from Left to Right (MSB to LSB).
        ;; Index 0 (Pixel 0) needs a shift of 7. Index 7 (Pixel 7) needs a shift of 0.
        shift (- 7 pixel-fine-x)]
    ;; Reconstruct the final 4-bit color index by combining the individual bits
    (bit-or
     (bit-and (bit-shift-right                 byte0 shift)    2r00000001)
     (bit-and (bit-shift-left (bit-shift-right byte1 shift) 1) 2r00000010)
     (bit-and (bit-shift-left (bit-shift-right byte2 shift) 2) 2r00000100)
     (bit-and (bit-shift-left (bit-shift-right byte3 shift) 3) 2r00001000))))
