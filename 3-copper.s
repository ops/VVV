; The third demopart by Marko Mäkelä

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

#processor 6502

#include "basic.i"
#include "timings.i"
#include "global.i"

#if SYSTEM==PAL
CLINES = 210	; height of copper area
PIXELS = 8	; number of pixels per copper line
#else
CLINES = 162
PIXELS = 7
#endif
CHARHEIGHT = 8	; height of copper characters in copper lines
LINEHEIGHT = 6	; height of copper lines in raster lines

TEXTWIDTH = 14	; size of the bouncing text screen
TEXTHEIGHT = 12

#if ((TEXTWIDTH * TEXTHEIGHT) & 1) || (TEXTWIDTH * TEXTHEIGHT > 255)
  echo "Screen size must be even and smaller than 256!"
  err
#endif

; first copper-written rasterline
FIRST_CLINE = FIRST_LINE + (LINES - CLINES - FIRST_LINE) / 2

#if CLINES % LINEHEIGHT
#echo "The copper area height must be a multiple of lineheight."
#err
#endif

#seg.u zeropage
#org 0
;-----------------------
;Music routine variables
zp:
  dc.w 0

;Variables for notecontrol
pos:
  dc.b 0,0,0
delay:
  dc.b 0,0,0
delaybase:
  dc.b 0,0,0

;Other music routine variables

volfademode:
  dc.b 0
volfadespd:
  dc.b 0

; Zero page work area for initializations.
; The area will be overwritten by the copper subroutine.

zpraster:
cnt:	ds.b 1
tmp:	ds.b 1
ptr:	ds.b 2

#seg main

#mac trashable	; trashable code (initializations)
initmusic:	; initialize the music
  ldx #2
loop$:
  lda #1
  sta delaybase,x
  sta delay,x

  lda #0
  sta pos,x
  sta sndbase,x

  dex
  bpl loop$

  sta volfademode	;No fading
  sta volfadespd	;Clear previous fading
  sta sndbase+3		;disable channel 3 output
  lda #8
  sta volumereg
  rts

vidinit:	; initialize the video chip
  ldx #textlen$
  ldy #collen$
writescreen$:
  lda screentext$-1,x
  dex
  sta $200,x
  lda screencolours$-1,y
  sta $9600,x
  dex
  lsr
  lsr
  lsr
  lsr
  sta $9600,x
  lda screentext$,x
  sta $200,x
  dey
  bne writescreen$

  ldx #textlen$
clearscreen$:
  lda #" "	; clear the rest of the screen
  sta $200,x
  lda #cFg2
  sta $9600,x	; and set the screen colour
  inx
  bne clearscreen$

  lda #(LAST_COLUMN - FIRST_COLUMN) / 2 - TEXTWIDTH + FIRST_COLUMN
  sta $9000		; set the x coordinate of screen origin
  lda #((LINES - FIRST_LINE - 8 * TEXTHEIGHT) / 2 + FIRST_LINE) / 2
  sta $9001		; set the y coordinate of screen origin
  lda #$80 + TEXTWIDTH	; set width, fix text matrix at $200
  sta $9002
  lda #TEXTHEIGHT * 2	; set height, use 8x8 characters
  sta $9003
  lda #$80		; text matrix at $200, character generator at $8000
  sta $9005
  rts

; Here is the text for the bouncing screen, in screen codes:
screentext$:
  hex 31352019050112132020e9896965
  hex 1609032d323021e98e69cfe34d65
  hex 20202020e98569cfe34de5a04e20
  hex 20e99669cfe34de5a0e989696520
  hex 20cfe34de5a0e98469cfe34d6520
  hex 20e5a0e98969cfe34de5a04e2020
  hex e99669cfe34de5a04e2020202020
  hex cfe34de5a0e9836965313938312d
  hex e5a04e2020cfe34d653139393620
  hex 2020202020e5a0e9a16965202020
  hex 20202020202020cfe34d65202020
  hex 20202020202020e5a04e20202020
textlen$ = . - screentext$

; And here are the colours (each hex digit corresponds to one char cell)
screencolours$:
  hex 11111111117771
  hex 11111117776611
  hex 11117776616611
  hex 15556616677711
  hex 16616677766111
  hex 16655566166111
  hex 77766166111111
  hex 66166555111111
  hex 66111661111111
  hex 11111665551111
  hex 11111116611111
  hex 17771116611111
collen$ = . - screencolours$
#endm

#if STANDALONE
  basicline $1001, 1996	; create the BASIC SYS line (for the unexpanded VIC-20)
#else
  org copperscroller

  jmp start
  trashable	; generate the trashable initialization parts here
#endif
start:
  sei
  ; initialize the video
  jsr vidinit
  ; initialize the music
  jsr initmusic
  ; create the copper line routines (blank pixels) and the jump tables
  jsr createprogram
  ; initialize the colour bars
  jsr colourize

nontrashable:	; beginning of non-overwritable code
  ; copy the raster routine to its place
  ldx #rastersize-1
zpcopy$:
  lda raster,x
  sta zpraster,x
  dex
  bpl zpcopy$

  ; set the interrupt so that the copper area will be centered
  setirq (FIRST_CLINE - 9) / 2, 5, irq, FALSE
  cli
#if !STANDALONE
  jsr loader
  bcs .
  lda #0
  sta exitirq	; tell the interrupt routine to finish
waitexit:
  jmp waitexit	; wait for the effect to be finished
  jmp (nextpart)
#else
  jmp .
#endif

lines:
  word linetab	; start address of the display area
blitcnt:
  byte LINEHEIGHT * CHARHEIGHT

irq:
  irqsync
  ldx #CLINES - 1
  ldy #cFg
  sec
  jsr copper	; perform the copper effect

#if !STANDALONE
  lda #$a9
  eor mincmp	; check if the colour bars have started disappearing
  bne notdone$

  lda volfadespd
  bne fading$
  lda #20	; fade out the volume in this many frames between the steps
  sta volfademode
  sta volfadespd
fading$:

  lda angle
  cmp #sinsize	; has the last colour bar disappeared?
  bcc notdone$
  lda #$7f	; yes, quit the interrupt routine
  sta $912e	; disable interrupts
  sta $912d	; acknowledge the interrupt
  lda #$ad
  sta waitexit	; let the main program to continue
  jmp domusic	; return to main program
notdone$:
#endif

  lda lines	; initialize the line table pointers and increment them
  sta.z addr
  adc #2	; carry==0
  sta lines
  lda lines+1
  sta.z addr+1
  bcc noinc$
  inc lines+1
noinc$:

  lda lines	; check for wraparound
  cmp #<(linetae-2*(CLINES+CHARHEIGHT*LINEHEIGHT))
  lda lines+1
  sbc #>(linetae-2*(CLINES+CHARHEIGHT*LINEHEIGHT))
  bcc nowrap$
  lda #<linetab
  sta lines
  lda #>linetab
  sta lines+1
  clc
nowrap$:

  dec blitcnt	; blit another char if necessary
  bne noblit$
  lda #LINEHEIGHT * CHARHEIGHT
  sta blitcnt
  jsr blitchar
noblit$:

		; bounce the text box
  ldx $9000	; x coordinate
bouncex$:
  inx		; self-modifying code: inx or dex
  cpx #FIRST_COLUMN + 1
  bcc chgdirx$	; check the bounds
  cpx #LAST_COLUMN - 2 * TEXTWIDTH - 1
  bcc bouncedx$
chgdirx$:
  lda #$ca ^ $e8
  eor bouncex$
  sta bouncex$	; change the direction
bouncedx$:
  stx $9000

  ldx $9001
bouncey$:
  inx		; self-modifying code: inx or dex
  cpx #FIRST_CLINE / 2
  bcc chgdiry$	; check the bounds
  cpx #(FIRST_CLINE + CLINES - 8 * TEXTHEIGHT) / 2 + 1
  bcc bouncedy$
chgdiry$:
  lda #$ca ^ $e8
  eor bouncey$
  sta bouncey$
bouncedy$:
  stx $9001

  jsr colourize	; rotate the colourbars

  lda $9124	; acknowledge the interrupt
domusic:	; do music and return to main program
sndbase = $900a
volumereg = $900e

  ;Volume and volume fade control:
#if !STANDALONE		;Save memory in the stand-alone version
  lda volfademode
  beq nofade$
  dec volfademode
  bne nofade$
  dec volumereg
  lda volumereg
  and #$0f
  beq fadedone$

  lda volfadespd

fadedone$:
  sta volfademode

nofade$:
#endif
  ;Process each voice:

  ldx #2
nextvoice$:
  dec delay,x		;Are we done with the current note?
  bne nochg$

next$:
  lda startsl,x
  sta zp
  lda startsh,x
  sta zp+1
  ldy pos,x
next2$:
  lda (zp),y
  iny		; no check for page boundaries (tunes < 256 bytes)

  cmp #firstcode
  bcs noduration$
  sta delaybase,x
  bcc next2$	;branch always
noduration$:
  cmp #silencecode	;Is it a note or silence?
  bcs realnote$
#if !STANDALONE		;Save memory in the stand-alone version
  cmp #setvolcode
  beq processsetvol$
  cmp #volfadecode
  beq processfadevol$
#endif
; cmp #gotocode		;default case: goto command
  lda (zp),y
  tay
; clc
  bcc next2$		;and read from there

realnote$:		;note or silence
  sta sndbase,x
  lda delaybase,x
  sta delay,x
  sty pos,x

nochg$:
  dex
  bpl nextvoice$

  jmp $eb18		;return to main program (pla:tay:pla:tax:pla:rti)

  ;Process fade volume
  ;-------------------
#if !STANDALONE		;Save memory in the stand-alone version
processfadevol$:
  lda (zp),y
  sta volfadespd
  sta volfademode
; sec
  bcs donext$

  ;Process set volume
  ;------------------

processsetvol$:
  lda volumereg
  and #$f0
  ora (zp),y
  sta volumereg
#endif

donext$:
  iny
  bne next2$	;branch always (tunes < 256 bytes)

;Music-data
;----------

#include "mus.canonind"

; Program data.

; Colour bar sets.
; You can define the colour of each colour bar.  There must be a colour entry
; for each colour bar, that is barsize * sinsize / maxangle bytes.

; The basic colours are in luminance order:
; 1 (white),  7 (yellow), 3 (cyan),    5 (green),
; 4 (purple), 2 (red),	  6 (blue) and 0 (black).

; All colours are in luminance order:
; 1 (white),  f (light yellow), b (light cyan),	  d (light green),
; 7 (yellow), c (light purple), 9 (light orange), 3 (cyan),
; a (pink),   e (light blue),	5 (green),	  8 (orange),
; 4 (purple), 2 (red),		6 (blue)    and	  0 (black).

; If bit 0 is set, the colour bar will be on the foreground
; (it will cover other copper graphics).

barsize = 7

#if 1
coltab:	hex 8b af 9f c9 9f af 8b	; orange to lt purple (on the top)
	hex 8b 9f cf d9 cf 9f 8b	; orange to lt green (2nd brightest)
	hex 9d cb df f9 df cb 9d	; lt orange to lt yellow (brightest)
	hex 8b 9f cf d9 cf 9f 8b	; orange to lt green (2nd brightest)
	hex 8b af 9f c9 9f af 8b	; orange to lt purple (on the bottom)
	hex 2e 4a 8a ac 8a 4a 2e	; red to pink (2nd darkest)
	hex 6e 2a 4a 8c 4a 2a 6e	; blue to orange (darkest)
	hex 2e 4a 8a ac 8a 4a 2e	; red to pink (2nd darkest)
#else
coltab:	hex 8b ab 9b cb 9b ab 8b	; orange to lt purple (on the top)
	hex 8b 9b cb db cb 9b 8b	; orange to lt green (2nd brightest)
	hex 9b cb db fb db cb 9b	; lt orange to lt yellow (brightest)
	hex 8b 9b cb db cb 9b 8b	; orange to lt green (2nd brightest)
	hex 8b ab 9b cb 9b ab 8b	; orange to lt purple (on the bottom)
	hex 2e 4e 8e ae 8e 4e 2e	; red to pink (2nd darkest)
	hex 6e 2e 4e 8e 4e 2e 6e	; blue to orange (darkest)
	hex 2e 4e 8e ae 8e 4e 2e	; red to pink (2nd darkest)
#endif
colend:

#if (colend - coltab) != barsize * sinsize / maxangle
#echo "colour table size mismatch!"
#err
#endif

cBg = $08 ; background colour: black
cFg = $19 ; foreground colour: white
cFg2 = 7  ; text colour: yellow

; Sinus tables.
sinsize = 240	; Sinus table size (common for all tables).
barcount = 8	; Amount of colour bars.
maxangle = sinsize / barcount
#if sinsize % barcount
#echo "Sinus table size must be a multiple of colour bar count."
#err
#endif
#if sinsize > 255
#echo "The sinus table is too big."
#err
#endif

; amp = CLINES - barheight
; sinus table: amp=210-7, circle=240, phase=45, entl=16
sinus:
#if SYSTEM==PAL
  dc.b 195,196,197,198,198,199,200,200,201,201,202,202,202,202,202,203
  dc.b 202,202,202,202,202,201,201,200,200,199,198,198,197,196,195,194
  dc.b 193,191,190,189,188,186,185,183,182,180,178,176,175,173,171,169
  dc.b 167,165,163,161,158,156,154,152,149,147,145,142,140,137,135,132
  dc.b 130,127,125,122,119,117,114,112,109,106,104,101,98,96,93,90
  dc.b 88,85,83,80,77,75,72,70,67,65,62,60,57,55,53,50
  dc.b 48,46,44,41,39,37,35,33,31,29,27,26,24,22,20,19
  dc.b 17,16,14,13,12,11,9,8,7,6,5,4,4,3,2,2
  dc.b 1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,2
  dc.b 2,3,4,4,5,6,7,8,9,11,12,13,14,16,17,19
  dc.b 20,22,24,26,27,29,31,33,35,37,39,41,44,46,48,50
  dc.b 53,55,57,60,62,65,67,70,72,75,77,80,83,85,88,90
  dc.b 93,96,98,101,104,106,109,112,114,117,119,122,125,127,130,132
  dc.b 135,137,140,142,145,147,149,152,154,156,158,161,163,165,167,169
  dc.b 171,173,175,176,178,180,182,183,185,186,188,189,190,191,193,194
#else
; sinus table: amp=162-7, circle=240, phase=45, entl=16
  dc.b 149,149,150,151,151,152,152,153,153,154,154,154,154,154,154,155
  dc.b 154,154,154,154,154,154,153,153,152,152,151,151,150,149,149,148
  dc.b 147,146,145,144,143,142,141,140,138,137,136,135,133,132,130,129
  dc.b 127,126,124,123,121,119,117,116,114,112,110,109,107,105,103,101
  dc.b 99,97,95,93,91,89,87,85,83,81,79,77,75,73,71,69
  dc.b 67,65,63,61,59,57,55,53,51,49,47,45,44,42,40,38
  dc.b 37,35,33,31,30,28,27,25,24,22,21,19,18,17,16,14
  dc.b 13,12,11,10,9,8,7,6,5,5,4,3,3,2,2,1
  dc.b 1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
  dc.b 2,2,3,3,4,5,5,6,7,8,9,10,11,12,13,14
  dc.b 16,17,18,19,21,22,24,25,27,28,30,31,33,35,37,38
  dc.b 40,42,44,45,47,49,51,53,55,57,59,61,63,65,67,69
  dc.b 71,73,75,77,79,81,83,85,87,89,91,93,95,97,99,101
  dc.b 103,105,107,109,110,112,114,116,117,119,121,123,124,126,127,129
  dc.b 130,132,133,135,136,137,138,140,141,142,143,144,145,146,147,148
#endif
#if . - sinus != sinsize
#echo "sinus table size mismatch"
#err
#endif

;; Subroutines.

; createprogram: create the copper line routines and the jump tables
; -------------
; Parameters:
;   none (called once)
; Registers affected:
;   .A .X .Y .P

createprogram:
  lda #<prgs
  sta.z ptr
  lda #>prgs
  sta.z ptr+1
  ldx #CLINES / LINEHEIGHT + CHARHEIGHT
  stx.z cnt
nextline$:
  ldx #PIXELS	; create the pixel code (multiple sta $900f's)
  ldy #0
nextpixel$:
  lda #$8d ; sta abs
  sta (ptr),y
  iny
  lda #$0f
  sta (ptr),y
  iny
  lda #$90
  sta (ptr),y
  iny
  dex
  bne nextpixel$
  lda #$4c	; write the return jump (jmp zpreturn)
  sta (ptr),y
  iny
  lda #<zpreturn
  sta (ptr),y
  iny
  lda #>zpreturn
  sta (ptr),y
  tya
  sec
  adc.z ptr
  sta.z ptr
  bcc noinc$
  inc.z ptr+1
noinc$:
  dec.z cnt
  bne nextline$
	 ; programs created, now go for the jump table
  lda #<linetab
  sta.z ptr
  lda #>linetab
  sta.z ptr+1
  jsr create$	; make two copies of the table
create$:
  ldy #CLINES / LINEHEIGHT + CHARHEIGHT
  sty.z cnt
  ldy #0
  lda #<prgs
  ldx #>prgs
  sta.z tmp
loop$:
  sta (ptr),y
  iny
  txa
  sta (ptr),y
#repeat LINEHEIGHT - 1
  iny
  lda.z tmp
  sta (ptr),y
  iny
  txa
  sta (ptr),y
#repend
  ldy #0
  lda #(PIXELS+1)*3
  clc
  adc.z tmp	; update the routine start address
  sta.z tmp
  bcc skip1$
  inx
  clc
skip1$:
  lda #2*LINEHEIGHT ; update the jump table address
  adc.z ptr
  sta.z ptr
  bcc skip2$
  inc.z ptr+1
skip2$:
  lda.z tmp
  dec.z cnt
  bne loop$
  rts

; colourize: create the colour bars to the temp area, increment angle
; --------
; Parameters:
;   none (static variable "angle")
; Registers affected:
;   .A .X .Y .P

colourize:
  lda #cBg		; First erase the existing colour bars.
  ldx #CLINES+2
erase$:
  dex
  sta colours,x
  bne erase$

angle = . + 1
  ldx #0		; increment and read the angle
  inx
  cpx #maxangle
  bcc notbig$

mincmp = .
  ldx #0		; limit the angle
  lda maxcmp$		; add the colour bars one by one to the screen
  cmp #sinsize
  bcs notbig$
  adc #maxangle
  sta maxcmp$

notbig$:
  stx angle

newbar$:		; render a colour bar at angle .X
  ldy sinus,x		; get the colour bar position

  txa
  pha
  ldx #barsize-1	; copy the colour bar
copybar$:
  lda colours+2,y	; check if there is already colour on that position
  eor #cBg
  bne skip$
copysrc$ = . + 1
  lda coltab,x
  sta colours+2,y	; skip the first elements (on the bottom)
skip$:
  iny
  dex
  bpl copybar$
  pla

  clc
  adc #maxangle		; prepare for drawing the next colour bar
#if sinsize + maxangle > 255
  bcs end$
#endif
  tax
  lda #barsize
  adc copysrc$
  sta copysrc$
#if >colend != >coltab
  bcc noinc$		; adjust the high address if needed
  inc copysrc$+1
noinc$:
#endif
maxcmp$ = . + 1
  cpx #maxangle
  bcc newbar$		; draw the bar
end$:
  lda #<coltab		; restore original colour set
  sta copysrc$
#if >colend != >coltab
  lda #>coltab
  sta copysrc$+1
#endif
  rts

; blitchar: blit a scroll text char in the pixel code, increment addresses
; --------
; Parameters:
;   none (static variables)
; Registers affected:
;   .A .X .Y .P

charmem = $8800 ; character generator ROM: normal upper/lowercase letters

blitchar:
  lda #>charmem / 8
  sta charmem$+1
text$ = . + 1
  lda text	; self-modifying address
  asl
  rol charmem$+1
  asl
  rol charmem$+1
  asl
  rol charmem$+1
  sta charmem$
  ldy #$80
nextline$:
  ldx #6
  txa
  ora charmem$
  sta charmem$
  bne skipdec$	; branch always
nextpixel$:
  dec charmem$
skipdec$:
  tya
  ldy #$8d	; sta absolute (generate a transparent pixel)
charmem$ = . + 1
  bit charmem	; self-modifying code
  beq blit$	; transparent pixel, do not change the opcode
  dey		; sty absolute (generate an opaque pixel)
display$ = . + 1
blit$:
  sty prgs	; change the colour (self-modifying address)
  tay
  clc		; increment the display address
  lda #3
  adc display$
  sta display$
  bcc noinc$
  inc display$+1
noinc$:
  dex
#if PIXELS==8
  bpl nextpixel$
#else
#if PIXELS==7
  bne nextpixel$
  dec charmem$
#else
  echo "unsupported pixel count in blitchar"
  err
#endif
#endif
  clc		; increment the display address
  lda #6	; (skip the last sta and the jmp instruction)
  adc display$
  sta display$
  bcc noinc2$
  inc display$+1
noinc2$:
  tya
  lsr
  tay

  lda display$	; line blitted, check for display wraparound
  cmp #<prge
  lda display$+1
  sbc #>prge
  bcc nowrap$
  lda #<prgs	; wrap the display from beginning
  sta display$
  lda #>prgs
  sta display$+1
nowrap$:
  tya
  bne nextline$	; go to next line

  inc text$	; update the scroll text pointer
  bne noinc3$
  inc text$+1
noinc3$:
  lda text$	; check for wraparound
  cmp #<textend
  lda text$+1
  sbc #>textend
  bcc nowrap2$
#if !STANDALONE
exitirq = . + 1
  bcs notdone$	; placeholder for self-modifying code:
  lda #$a9	; wrap the scroll from beginning
		; if the next part hasn't been loaded
  sta mincmp	; all done: make the colour bars disappear
  bcs nowrap2$	; branch always (do not perform the wraparound in this case)
notdone$:
#endif
  lda #<text	; wrap the text from beginning
  sta text$
  lda #>text
  sta text$+1
nowrap2$:
  rts

; The most important thing: the scrolltext
;			    --------------
; The following ASCII text is converted to char codes:
;;;
; Hi there! What, a copper routine on the poor old Vic!
;;;
; Conversion filter:
;; cut -c3- | tr @a-z\\012 '\0-\032 ' | hexdump -e '"hex " 32/1 "%02x" "\n"'
text:	hex 48092014080512052120570801142c200120030f1010051220120f1514090e05
	hex 200f0e2014080520100f0f12200f0c04205609032120
textend:
#if !STANDALONE
; the text is followed by enough spaces to clear the screen
#repeat (CLINES / LINEHEIGHT + CHARHEIGHT - 1) / CHARHEIGHT
	dc.b " "
#repend
#endif

raster:
#rorg zpraster
#if SYSTEM==PAL
m: byte 1
n: byte 2
zploop:
  inc.b addr+1
  dex
copper:
  lda colours,x
  sta $900f
  bit m		; Is it an opaque colour bar?
  bne zpdelay	; Yes, skip the blitter part.
addr = . + 1
  jmp (linetab) ; Blit the graphics.
zpreturn:
  lda addr
  adc #1	; Carry == 1
  sta addr
  bcs zploop	; Jump if the high address byte changes.  There
		; is no time to compare the counter in this case.
  cpx n		; n == 2
  dex
  bcs copper	; Continue until x==1 or x==0.	There are two
		; possibilities, because of the zploop thing.
  rts
zpdelay:	; spend 39 cycles
  jsr zpdelay-1
  jsr zpdelay-1
  jsr zpdelay-1
  jmp zpreturn
#else
n: byte 2
zploop:
  inc.b addr+1
  dex
copper:
  lda colours,x
  sta $900f	; On NTSC, there is no time to do opaque colour bars.
  bit n		; But there is time for this stupid delay.
addr = . + 1
  jmp (linetab) ; Blit the graphics.
zpreturn:
  lda addr
  adc #1	; Carry == 1
  sta addr
  bcs zploop	; Jump if the high address byte changes.  There
		; is no time to compare the counter in this case.
  cpx n		; n == 2
  dex
  bcs copper	; Continue until x==1 or x==0.	There are two
		; possibilities, because of the zploop thing.
  rts
#endif
#rend
rastersize = . - raster

endcode:	; The program bytes after this label will be overwritten.

#if STANDALONE
  trashable	; generate the trashable code
#else
  if . > MEMTOP
    echo "The copper part is", . - MEMTOP, " bytes too long!"
    err
  endif

  if . < MEMTOP
    echo "The copper part could be", MEMTOP - ., "bytes higher in memory"
  endif
#endif

; Absolute work area.

#seg.u tempdata
; calculate the temporary area size

#if STANDALONE	; Determine the optimal start address for the temp area
  org endcode
#else
  org (nontrashable-(CLINES+CHARHEIGHT*LINEHEIGHT)*4-(CLINES/LINEHEIGHT+CHARHEIGHT)*(PIXELS+1)*3-(CLINES+2)) & ~255
#endif
temparea:

#if !STANDALONE
#align 256
colours: ds.b CLINES+2
#endif
#align 2
linetab: ds.b (CLINES+CHARHEIGHT*LINEHEIGHT)*2*2
linetae:
prgs:	 ds.b (CLINES/LINEHEIGHT+CHARHEIGHT)*(PIXELS+1)*3
prge:
#if STANDALONE
#align 256
colours: ds.b CLINES+2
#endif
#if . > MEMTOP
  echo "The copper part is", . - MEMTOP, " bytes too long!"
  err
#endif

#if !STANDALONE && (. > nontrashable)
  echo "Temp area overwrites the non-trashable code by ", . - nontrashable, "bytes!"
  err
#endif
