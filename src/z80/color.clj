(ns z80.color
  (:require [z80.memory :as memory]
            [quil.core :as q]))

(defn- sms-color->rgb
  "Converts a 6-bit SMS color byte (00BBGGRR) to a standard 24-bit RGB vector."
  [sms-color-byte]
  ;; Guarantee that we mask out any signed integer junk before splitting bits
  (let [clean-byte (memory/signed->unsigned (int sms-color-byte))
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

(defn get-sms-pixel-color-idx
  "Extracts the exact 4-bit color palette index (0-15) for a specific 
   horizontal pixel (0-7) in a 4bpp planar Sega Master System tile row.
   Basically, get-vdp-color-palette returns an array and this function retruns an index in that array."
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
        b0 (if (< addr (count vram)) (memory/signed->unsigned (aget vram addr)) 0)
        b1 (if (< (inc addr) (count vram)) (memory/signed->unsigned (aget vram (inc addr))) 0)
        b2 (if (< (+ addr 2) (count vram)) (memory/signed->unsigned (aget vram (+ addr 2))) 0)
        b3 (if (< (+ addr 3) (count vram)) (memory/signed->unsigned (aget vram (+ addr 3))) 0)
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
