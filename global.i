; Global constants

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

;SYSTEM	eqm NTSC	; 6560-101: 65 cycles per raster line, 261 lines
;SYSTEM	eqm PAL 	; 6561-101: 71 cycles per raster line, 312 lines

;STANDALONE = 0	; set this to nonzero if you want to compile
		; stand-alone versions for no memory expansion

#if STANDALONE	; define the memory top
MEMTOP = $2000	; no expansion
#else
MEMTOP = $4000	; 8k expansion
#endif

loader = $4000	; start address of the IRQ loader
nextpart = $41DB
		; vector to the start address of next demo part
		; (set by the loader)
fasave = $3ff	; device number of the disk drive used (needed in the end part)

; start addresses of different parts
chessboardzoom = $1000	; chessboard zoomer.  Used memory: $0000-$1fff
bitmapscroller = $2000	; bitmap scroller.  Used memory: $1000-$3xxx
copperscroller = $3880	; the copper scroller.  Used memory: $2e00-$3fff
vectormania = $1000	; the vector part
plasma = $3658
rotator = $1600
lastpart = $159c	; the last part, as the name says

; This is ugly, but there is no other way to continue the music of Plasma
; in Rotator.

plasmamusic = $3d2d
plasmamdata = $3fb5

; Memory map of different parts
;
; File name   Approximate memory usage while:
;             1)being loaded 2)loading next part
; init        $1201-$1fff    $0100-$01ff
; 1-intro     $1000-$1fff    $1000-$1fff
; 2-scroll    $2000-$2fff    $1000-$2fff (approx)
; 3-copper    $3880-$3fff    $2e00-$3fff
; 4-vector    $1000-$2bff    $1000-$32ff
; 5-plasma    $3658-$3fff    $1000-$15ff, $3b45-$3fff
; 6-rotator   $1600-$306a    $1000-$156a
; 7-end       $159c-$3fff
