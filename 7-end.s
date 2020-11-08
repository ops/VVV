; The last demopart by Marko Mäkelä

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

#processor 6502

; Jonas Hultén <md6cbm@mdstud.chalmers.se> refused to release the source code
; of his music.  We are sorry for the inconvenience.
JONAS = 1	;set this to 0, if you want to compile with Penny Lane instead

#seg.u zeropage
#org 0

#if JONAS
  include "mus.jonas"
  musicdata
#else

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
#endif

#seg code

#include "global.i"
#include "timings.i"

#if STANDALONE
#echo "This part does not work on an unexpanded VIC!"
#err
#endif

#org lastpart

; KERNAL definitions

secnd	= $ff93	; send secondary address for LISTEN
ciout	= $ffa8	; write serial data
unlsn	= $ffae	; send UNLISTEN command
listn	= $ffb1	; send LISTEN command
AMOUNT = $20	; amount of data bytes to transfer with one M-W command

; constants

fWidth = ((LAST_COLUMN - FIRST_COLUMN) / 2 + 1) & ~1 ; scroll text width

blank = $1000 ;blank screen matrix (for the colourbars)
scroll = $1200 ;text matrix (scroll buffer)
scroll0 = scroll
scroll1 = scroll0 + fWidth
scroll2 = scroll1 + fWidth
scroll3 = scroll2 + fWidth
scroll4 = scroll3 + fWidth
scroll5 = scroll4 + fWidth
scroll6 = scroll5 + fWidth
scroll7 = scroll6 + fWidth

volumereg = $900e
sndbase = $900a

cFg0 = 9  ; white
cFg1 = 13 ; green
cFg2 = 12 ; purple
cFg3 = 11 ; cyan
cFg4 = 9  ; white
cFg5 = 13 ; green
cFg6 = 12 ; purple
cFg7 = 11 ; cyan

cAx = 7 ; auxiliary colour for the scrolls: yellow
cBg = 6 ; background colour for the scrolls: blue/black

barheight = 128 + barsize ; height of colour bar area

  jmp setup	; jump to the beginning 

;---------------------------------------
; the drive code

drvcode:

#rorg $500

;---------------------------------------

;---------------------------------------
; Morse routine for the drive
;---------------------------------------
; The 1581-code by Pasi 'Albert' Ojala, albert@cs.tut.fi

;---------------------------------------
; FOR 1581
;---------------------------------------
ciapa = $4000			; There is also a CIA on the 1571 here, and
ciapb = $4001			; port A is written also in 15[47][10] code,
				; but the ports are unconnected on the 1571.

;---------------------------------------
; FOR 1540/41/70/71
;---------------------------------------
via1pb	= $1800
via2pb	= $1c00
dzp = $14	; buffer for current code (on 1581 sector for job 4)

TIMEBASE = 50	; morseing basic delay

;---------------------------------------

drive:
  cld
  sei
  lda ciapa	; read the 1581 CIA state
  and #$9f
  sta ledoff81$
  ora #$40
  sta ledon81$
  lda via2pb	; read the 15[47][01] VIA state (or a 1581 memory location)
  pha		; prepare to restore it
  and #$f3	; turn off the motor and the LED
  sta ledoff$
  ora #8
  sta ledon$

  ldy #0
  sty dzp
morseloop$:
  asl dzp
  bcc breaks$
ledon$ = . + 1	; now we get - or .
  ldx #8
  stx via2pb	; turn on the LED
ledon81$ = . + 1
  ldx #$40
  stx ciapa	; turn on the LED on the 1581
  asl dzp
  bcc short$
  ldx #3*TIMEBASE
  hex 2c
short$:
  ldx #TIMEBASE
  jmp dodelay$

breaks$:	; inter-char or inter-word break
  asl dzp
  bcc icb$
  ldx #5*TIMEBASE
  hex 2c
icb$:
  ldx #2*TIMEBASE

dodelay$:
  lda $ff54	; see if we are running on a 1581
  and #$df	; jmp abs or jmp (abs)?
  eor #$4c	; jmp abs
  beq delay1581$
dloop$:
  lda via1pb	; quit if ATN was asserted
  bmi gotatn$
  dey
  bne dloop$
  dex
  bne dloop$
ledoff$ = . + 1
  lda #0
  sta via2pb	; turn off the LED
  ldx #TIMEBASE
  bne idelay$	; branch always

delay1581$:
dloop1581$:
  nop		; need longer delay on the 1581
  nop
  nop
  nop
  nop
  lda ciapb	; quit if ATN was asserted
  bmi gotatn$
  dey
  bne dloop1581$
  dex
  bne dloop1581$
ledoff81$ = . + 1
  ldx #0
  stx ciapa	; turn off the LED on 1581
  ldx #2*TIMEBASE
idelay$:
iloop$:		; make an inter-symbol pause
  dey
  bne iloop$
  dex
  bne iloop$
  sec
counter$ = . + 1
  lda #0	; update the morse token counter
  adc #0
  and #3
  sta counter$
  bne morseloop$; process next morse token
msgsrc$ = . + 1
  lda message$
  sta dzp
  ldx msgsrc$
  inx
  stx msgsrc$
  cpx #<mesgend$
  bcc morseloop$
  ldx #<message$
  stx msgsrc$
  bne morseloop$; branch always
gotatn$:	; got ATN => exit
  pla
  sta via2pb	; restore VIA (or more importantly the 1581 track cache) state
  cli
  rts

; The message.  00 = char break, 01 = word break, 10 = dit, 11 = dah
message$:
  dc.b %11110011, %11110010, %11100010, %10100010, %01101110, %00101011
  dc.b %00101110, %10001000, %10101001, %11001010, %10100010, %10001010
  dc.b %10011010, %00101010, %01110010, %10101000, %10011010, %10100010
  dc.b %10001110, %10001110, %10001000, %11100111, %11001000, %10101000
  dc.b %10101000, %10110011, %11100010, %01111111, %00101011, %10011111
  dc.b %11001010, %11001011, %10011110, %10001000, %11110011, %11110111
  dc.b %10111000, %11111100, %11101000, %10001110, %10011110, %10100011
  dc.b %10111101, %11110010, %11001011, %10001110, %11001111, %11011111
  dc.b %00101110, %11001110, %11001000, %10111010, %01101110, %11011011
  dc.b %11111100, %11111111, %10001111, %11111000, %11101010, %10010101
mesgend$:
edrvcode:
#rend

setup:
  lda #$7f
  sta $913e	; disable and acknowledge any NMIs and IRQs
  sta $913d

  lda #0
  sta $9002	; zero screen width (blank screen during setup)

  ; set up the scroll text colours
  ldy #fWidth
scrollc$:
  lda #cFg0
  sta $93ff + [scroll0 & $3ff],y
  lda #cFg1
  sta $93ff + [scroll1 & $3ff],y
  lda #cFg2
  sta $93ff + [scroll2 & $3ff],y
  lda #cFg3
  sta $93ff + [scroll3 & $3ff],y
  lda #cFg4
  sta $93ff + [scroll4 & $3ff],y
  lda #cFg5
  sta $93ff + [scroll5 & $3ff],y
  lda #cFg6
  sta $93ff + [scroll6 & $3ff],y
  lda #cFg7
  sta $93ff + [scroll7 & $3ff],y
  dey
  bne scrollc$

; initialize the music
#if JONAS
  playerinit
  lda #volume | (cAx << 4)
  sta volumereg		; set the auxiliary colour for the scroller
#else
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

  sta sndbase+3		;disable channel 3 output
  lda #8 | (cAx << 4)	;set the auxiliary colour for the scroller
  sta volumereg		;and the volume level of the music
#endif
  jsr colourize		;initialize the colour bars

  ; set up the scroll text
  lda #" " * 2
  ldy #8 * fWidth
scroll$:
  dey
  sta scroll,y
  sta blank,y
  bne scroll$

  ; set the interrupt
  setirq (LINES + FIRST_LINE - barheight) / 4 - 5, 0, irq, FALSE
  lda #(LINES + FIRST_LINE + 128) / 4

waitbl$:	;wait for vertical blank
  lda $9004
  bne waitbl$
  ; initialize the video chip
  lda #FIRST_COLUMN
  sta $9000	; screen origin x
  lda #(LINES + FIRST_LINE - 128) / 4
  sta $9001	; screen origin y
  lda #fWidth | ((scroll >> 2) & $80)
  sta $9002	; screen width
  lda #17
  sta $9003	; screen height
  lda #((scroll >> 6) & $f0) | (fontstart >> 10) | $88
  sta $9005	; graphics buffer pointer
  lda #cBg
  sta $900f	; screen colour
  jmp .		; loop forever

; colourize: create the colour bars to the temp area, increment angle
; ---------
; Parameters:
;   none (static variable "angle")
; Registers affected:
;   .A .X .Y .P

maxangle = 42	; maximum colour bar angle

colourize:
  lda #cBg		; First erase the existing colour bars.
  ldx #barheight
erase$:
  dex
  sta colours,x
  bne erase$

  ldx angle		; increment and read the angle
  inx
  cpx #maxangle
  bcc notbig$
  ldx #0
notbig$:
  stx angle

newbar$:		; render a colour bar at angle .X
  ldy sinus,x		; get the colour bar position

  txa
  pha
  ldx #barsize-1	; copy the colour bar
copybar$:
  lda colours+1,y	; check if there is already colour on that position
  eor #cBg
  bne skip$
copysrc$ = . + 1
  lda coltab,x
  sta colours+1,y	; skip the first element (it's on the bottom)
skip$:
  iny
  dex
  bpl copybar$
  pla

  clc
  adc #maxangle		; prepare for drawing the next colour bar
#if sinsize + maxangle > 255
  bcs finished$
  tax
  lda #barsize
  adc copysrc$
  sta copysrc$
#if >colend != >coltab
  bcc newbar$
  inc copysrc$+1
#endif
  jmp newbar$
finished$:
#else
  tax
  lda #barsize
  adc copysrc$
  sta copysrc$
#if >colend != >coltab
  bcc noinc$		; adjust the high address if needed
  inc copysrc$+1
noinc$:
#endif
  cpx #sinsize
  bcc newbar$		; draw the bar
#endif
  lda #<coltab		; restore original colour set
  sta copysrc$
#if >colend != >coltab
  lda #>coltab
  sta copysrc$+1
#endif
  rts

#align 1024
fontstart:
#incbin "font.big"
#if . > $2000
  echo "The font must be in internal memory!"
  err
#endif

irq:		; The raster interrupt routine
  irqsync	; Syncronize 100% to the screen.
  ldx #barheight
loop$:
  lda colours-1,x
  ora #8
  tay
  lda colours-1,x
  and #8
  asl
  asl
  asl
  asl
  eor #fWidth | $80
  sta $9002
  sty $900f
#if SYSTEM == PAL
  ldy #6
  dey
  bne *-1
  bit $24
#endif
#if SYSTEM == NTSC
  ldy #5
  dey
  bne *-1
  nop
#endif
  dex
  bne loop$

  jsr colourize	; rotate the colourbars

orgx = . + 1
  lda #FIRST_COLUMN
  sta $9000
  eor #1
  sta orgx
  eor #FIRST_COLUMN
  bne doscroll$
  jmp domusic		; do music and return to main program

doscroll$:
scrp$ = . + 1
  ldx #fWidth
  ldy #fWidth / 2
scroll0$:
scroll0src$ = . + 1
  lda text0,y
  asl
  cpx #fWidth
  bcs . + 7
  ora #1
  sta scroll0,x
  dex
  and #$fe
  sta scroll0,x
  dey
  dex
  bpl scroll0$

  ldx scrp$
  ldy #fWidth / 2
scroll1$:
scroll1src$ = . + 1
  lda text1,y
  asl
  cpx #fWidth
  bcs . + 7
  ora #1
  sta scroll1,x
  dex
  and #$fe
  sta scroll1,x
  dey
  dex
  bpl scroll1$

  ldx scrp$
  ldy #fWidth / 2
scroll2$:
scroll2src$ = . + 1
  lda text2,y
  asl
  cpx #fWidth
  bcs . + 7
  ora #1
  sta scroll2,x
  dex
  and #$fe
  sta scroll2,x
  dey
  dex
  bpl scroll2$

  ldx scrp$
  ldy #fWidth / 2
scroll3$:
scroll3src$ = . + 1
  lda text3,y
  asl
  cpx #fWidth
  bcs . + 7
  ora #1
  sta scroll3,x
  dex
  and #$fe
  sta scroll3,x
  dey
  dex
  bpl scroll3$

  ldx scrp$
  ldy #fWidth / 2
scroll4$:
scroll4src$ = . + 1
  lda text4,y
  asl
  cpx #fWidth
  bcs . + 7
  ora #1
  sta scroll4,x
  dex
  and #$fe
  sta scroll4,x
  dey
  dex
  bpl scroll4$

  ldx scrp$
  ldy #fWidth / 2
scroll5$:
scroll5src$ = . + 1
  lda text5,y
  asl
  cpx #fWidth
  bcs . + 7
  ora #1
  sta scroll5,x
  dex
  and #$fe
  sta scroll5,x
  dey
  dex
  bpl scroll5$

  ldx scrp$
  ldy #fWidth / 2
scroll6$:
scroll6src$ = . + 1
  lda text6,y
  asl
  cpx #fWidth
  bcs . + 7
  ora #1
  sta scroll6,x
  dex
  and #$fe
  sta scroll6,x
  dey
  dex
  bpl scroll6$

  ldx scrp$
  ldy #fWidth / 2
scroll7$:
scroll7src$ = . + 1
  lda text7,y
  asl
  cpx #fWidth
  bcs . + 7
  ora #1
  sta scroll7,x
  dex
  and #$fe
  sta scroll7,x
  dey
  dex
  bpl scroll7$

  dec scrp$
  ldx scrp$
  cpx #fWidth - 1
  bcc doupdate$
  jmp domusic		; do music and return to main program

doupdate$:
  ldx #fWidth
  stx scrp$

  inc scroll0src$	; update the scroll text pointer
  bne noupdhi0$
  inc scroll0src$ + 1
noupdhi0$:
  ldy scroll0src$
  lda scroll0src$ + 1
  cpy #<text0end	; check for wraparound
  sbc #>text0end
  bcc nowrap0$

  lda #<text0		; restart text from beginning
  sta scroll0src$
  lda #>text0
  sta scroll0src$ + 1
nowrap0$:

  inc scroll1src$	; update the scroll text pointer
  bne noupdhi1$
  inc scroll1src$ + 1
noupdhi1$:
  ldy scroll1src$
  lda scroll1src$ + 1
  cpy #<text1end	; check for wraparound
  sbc #>text1end
  bcc nowrap1$

  lda #<text1		; restart text from beginning
  sta scroll1src$
  lda #>text1
  sta scroll1src$ + 1
nowrap1$:

  inc scroll2src$	; update the scroll text pointer
  bne noupdhi2$
  inc scroll2src$ + 1
noupdhi2$:
  ldy scroll2src$
  lda scroll2src$ + 1
  cpy #<text2end	; check for wraparound
  sbc #>text2end
  bcc nowrap2$

  lda #<text2		; restart text from beginning
  sta scroll2src$
  lda #>text2
  sta scroll2src$ + 1
nowrap2$:

  inc scroll3src$	; update the scroll text pointer
  bne noupdhi3$
  inc scroll3src$ + 1
noupdhi3$:
  ldy scroll3src$
  lda scroll3src$ + 1
  cpy #<text3end	; check for wraparound
  sbc #>text3end
  bcc nowrap3$

  lda #<text3		; restart text from beginning
  sta scroll3src$
  lda #>text3
  sta scroll3src$ + 1
nowrap3$:

  inc scroll4src$	; update the scroll text pointer
  bne noupdhi4$
  inc scroll4src$ + 1
noupdhi4$:
  ldy scroll4src$
  lda scroll4src$ + 1
  cpy #<text4end	; check for wraparound
  sbc #>text4end
  bcc nowrap4$

  lda #<text4		; restart text from beginning
  sta scroll4src$
  lda #>text4
  sta scroll4src$ + 1
nowrap4$:

  inc scroll5src$	; update the scroll text pointer
  bne noupdhi5$
  inc scroll5src$ + 1
noupdhi5$:
  ldy scroll5src$
  lda scroll5src$ + 1
  cpy #<text5end	; check for wraparound
  sbc #>text5end
  bcc nowrap5$

  lda #<text5		; restart text from beginning
  sta scroll5src$
  lda #>text5
  sta scroll5src$ + 1
nowrap5$:

  inc scroll6src$	; update the scroll text pointer
  bne noupdhi6$
  inc scroll6src$ + 1
noupdhi6$:
  ldy scroll6src$
  lda scroll6src$ + 1
  cpy #<text6end	; check for wraparound
  sbc #>text6end
  bcc nowrap6$

  lda #<text6		; restart text from beginning
  sta scroll6src$
  lda #>text6
  sta scroll6src$ + 1
nowrap6$:

  inc scroll7src$	; update the scroll text pointer
  bne noupdhi7$
  inc scroll7src$ + 1
noupdhi7$:
  ldy scroll7src$
  lda scroll7src$ + 1
  cpy #<text7end	; check for wraparound
  sbc #>text7end
  bcc nowrap7$

  lda #<text7		; restart text from beginning
  sta scroll7src$
  lda #>text7
  sta scroll7src$ + 1
nowrap7$:

domusic:		; do music and return to main program
#if JONAS
  jsr play
#else
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
#endif
  lda $9124		;acknowledge the interrupt
  jmp $eb18		;return to main program (pla:tay:pla:tax:pla:rti)

; Sinus tables.
sinsize = 252	; Sinus table size

; sinus table: amp=128, circle=252, phase=84, entl=14
sinus:	dc.b 119,118,117,116,115,115,114,113,111,110,109,108,107,106
	dc.b 105,103,102,101,100,98,97,96,94,93,91,90,88,87
	dc.b 85,84,82,81,79,78,76,75,73,71,70,68,67,65
	dc.b 64,62,60,59,57,56,54,52,51,49,48,46,45,43
	dc.b 42,40,39,37,36,34,33,32,30,29,27,26,25,24
	dc.b 22,21,20,19,18,17,16,14,13,12,12,11,10,9
	dc.b 8,7,7,6,5,5,4,3,3,2,2,1,1,1
	dc.b 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dc.b 0,1,1,1,2,2,3,3,4,5,5,6,7,7
	dc.b 8,9,10,11,12,12,13,14,16,17,18,19,20,21
	dc.b 22,24,25,26,27,29,30,31,33,34,36,37,39,40
	dc.b 42,43,45,46,48,49,51,52,54,56,57,59,60,62
	dc.b 63,65,67,68,70,71,73,75,76,78,79,81,82,84
	dc.b 85,87,88,90,91,93,94,95,97,98,100,101,102,103
	dc.b 105,106,107,108,109,110,111,113,114,115,115,116,117,118
	dc.b 119,120,120,121,122,122,123,124,124,125,125,126,126,126
	dc.b 127,127,127,127,127,127,127,127,127,127,127,127,127,127
	dc.b 127,126,126,126,125,125,124,124,123,122,122,121,120,120
;#pagecheck sinus
#if . - sinus != sinsize
#echo "sinus table size mismatch"
#err
#endif

; Colour bar sets.
; You can define the colour of each colour bar.  There must be a colour entry
; for each colour bar, that is barsize * sinsize / maxangle bytes.

; If bit 3 is set, the colour bar will be on the foreground
; (it covers any text on the screen).

barsize = 5

coltab:	hex 8a 9a ca 9a 8a	; orange to lt purple (2nd brightest)
;	hex 9a ca da ca 9a	; lt orange to lt green (brightest)
	hex 8a 9a ca 9a 8a	; orange to lt purple (2nd brightest)
	hex 8a aa 9a aa 8a	; orange to lt orange (on the bottom)
	hex 22 42 82 42 22	; red to orange (2nd darkest)
;	hex 62 22 42 22 62	; blue to purple (darkest)
	hex 22 42 82 42 22	; red to orange (2nd darkest)
	hex 8a aa 9a aa 8a	; orange to lt orange (on the top)
#pagecheck coltab
colend:

#if (colend - coltab) != barsize * sinsize / maxangle
#echo "colour table size mismatch!"
#err
#endif

#if JONAS
  player
#else
  include "mus.pennylane"
#endif

; The scrolltexts.  Conversion filter:
;; cut -c3-|tr @A-ZÄÖÅfsÆØÜÉ\\012 '\0-\037\044-\046\042 ' \
;; | hexdump -e '"  hex " 32/1 "%02x" "\n"'
text0:
; blank padding at start of text (I am lazy)
#repeat fWidth/2+2
  dc.b " "
#repend
; The 1st scrolltext. The following text is converted to char codes:
;;;
; THAT WAS ALL, FOLKS. NOW IT IS THE TIME FOR THE GREETINGS. OUR GREETINGS GO
; TO ALL VIC-20 USERS, ESPECIALLY TO PAUL LE BRASSE AND TO WARD SHRAKE, WHO
; HAVE ARCHIVED LOADS OF VIC-20 CARTRIDGES, AND MIGUEL GORDILLO, WHO
; HELPED US IN MEASURING THE SCREEN SIZE OF THE 6560. WE ALSO GREET
; DANIEL DALLMANN, WHO DONATED HIS NTSC VIC-20 TO US. LAST BUT NOT LEAST,
; WE WOULD LIKE TO THANK RICHARD STALLMAN AND LINUS TORVALDS FOR THE
; OPERATING ENVIRONMENT USED IN DEVELOPING THE DEMO. BY THE WAY, DID YOU
; KNOW THAT LINUS STARTED COMPUTING ON THE VIC-20?
;;;
  hex 140801142017011320010c0c2c20060f0c0b132e200e0f172009142009132014
  hex 08052014090d0520060f1220140805200712050514090e07132e200f15122007
  hex 12050514090e071320070f20140f20010c0c201609032d32302015130512132c
  hex 20051310050309010c0c1920140f201001150c200c052002120113130520010e
  hex 0420140f201701120420130812010b052c2017080f2008011605200112030809
  hex 160504200c0f010413200f06201609032d323020030112141209040705132c20
  hex 010e04200d090715050c20070f1204090c0c0f2c2017080f2008050c10050420
  hex 151320090e200d0501131512090e07201408052013031205050e2013091a0520
  hex 0f062014080520363536302e20170520010c130f2007120505142004010e0905
  hex 0c2004010c0c0d010e0e2c2017080f20040f0e0114050420080913200e141303
  hex 201609032d323020140f2015132e200c01131420021514200e0f14200c050113
  hex 142c20170520170f150c04200c090b0520140f201408010e0b20120903080112
  hex 04201314010c0c0d010e20010e04200c090e151320140f1216010c041320060f
  hex 1220140805200f1005120114090e0720050e1609120f0e0d050e142015130504
  hex 20090e20040516050c0f10090e07201408052004050d0f2e2002192014080520
  hex 1701192c2004090420190f15200b0e0f172014080114200c090e151320131401
  hex 1214050420030f0d101514090e07200f0e20140805201609032d32303f
text0end:
text1:
; blank padding between texts (I am lazy)
#repeat fWidth/2+2
  dc.b " "
#repend
; The 2nd scrolltext.
;;;
; DET VAR ALLT VI HAR. VI ÖVERGÅR TILL HÄLSNINGARNA, SOM GÅR TILL ALLA
; VIC20-ANVÄNDARE, SPECIELLT PAUL LE BRASSE OCH WARD SHRAKE, SOM HAR
; ARKIVERAT MASSOR AV VIC20-CARTRIDGES, OCH TILL MIGUEL GORDILLO, SOM
; HJÄLPTE OSS ATT MÄTA SKÄRMSTORLEKEN PÅ 6560. VI HÄLSAR OCKSÅ TILL
; DANIEL DALLMANN, SOM DONERADE SIN NTSC VIC-20 TILL OSS. SIST MEN
; INTE MINST, VILL VI TACKA RICHARD STALLMAN OCH LINUS TORVALDS FÖR
; OPERATIVSYSTEMET VI ANVÄNDE SOM UTVECKLINGSMILJÖ FÖR
; DEMOT. FÖRRESTEN, VISSTE NI ATT LINUS BÖRJADE SIN DATORKARRIÄR PÅ
; VIC-20?
;;;
  hex 0405142016011220010c0c14201609200801122e201609201c160512071d1220
  hex 14090c0c20081b0c130e090e0701120e012c20130f0d20071d122014090c0c20
  hex 010c0c012016090332302d010e161b0e040112052c201310050309050c0c1420
  hex 1001150c200c0520021201131305200f0308201701120420130812010b052c20
  hex 130f0d200801122001120b091605120114200d0113130f122001162016090332
  hex 302d030112141209040705132c200f03082014090c0c200d090715050c20070f
  hex 1204090c0c0f2c20130f0d20080a1b0c101405200f131320011414200d1b1401
  hex 20130b1b120d13140f120c050b050e20101d20363536302e20160920081b0c13
  hex 0112200f030b131d2014090c0c2004010e09050c2004010c0c0d010e0e2c2013
  hex 0f0d20040f0e05120104052013090e200e141303201609032d32302014090c0c
  hex 200f13132e2013091314200d050e20090e1405200d090e13142c2016090c0c20
  hex 1609201401030b012012090308011204201314010c0c0d010e200f0308200c09
  hex 0e151320140f1216010c041320061c12200f1005120114091613191314050d05
  hex 1420160920010e161b0e040520130f0d2015141605030b0c090e07130d090c0a
  hex 1c20061c122004050d0f142e20061c1212051314050e2c20160913131405200e
  hex 0920011414200c090e151320021c120a0104052013090e200401140f120b0112
  hex 12091b1220101d201609032d32303f
text1end:
text2:
; blank padding between texts (I am lazy)
#repeat fWidth/2+2
  dc.b " "
#repend
; The 3rd scrolltext.
;;;
; TÄSSÄ TÄMÄ DEMO NYT SITTEN OLI. SAAKO LÄHETTÄÄ TERVEISIÄ? NO, JOS
; NYT TÄMÄN KERRAN. TERVEHDIMME KAIKKIA VIC-20:N KÄYTTÄJIÄ, ERITOTEN
; PAUL LE BRASSEA JA WARD SHRAKEA, JOTKA KERÄSIVÄT TALTEEN
; MODUULIPELEJÄ URAKALLA. NTSC-MUUNNOS OLISI JÄÄNYT TEKEMÄTTÄ, ELLEI
; DANIEL DALLMANN OLISI LAHJOITTANUT KONETTAAN. LISÄKSI MIGUEL
; GORDILLOSTA OLI SUURI APU NTSC-KUVAN KOON MITTAAMISESSA. LOPUKSI
; HALUAMME KIITTÄÄ RICHARD STALLMANIA JA LINUS TORVALDSIA DEMON
; KEHITYSYMPÄRISTÖN RUNGOSTA. TIESITTEKÖ MUUTEN, ETTÄ LINUS ALOITTI
; TIETOKONEILUNSA VIC-20:LLA?
;;;
  hex 141b13131b20141b0d1b2004050d0f200e19142013091414050e200f0c092e20
  hex 1301010b0f200c1b080514141b1b2014051216050913091b3f200e0f2c200a0f
  hex 13200e191420141b0d1b0e200b051212010e2e2014051216050804090d0d0520
  hex 0b01090b0b0901201609032d32303a0e200b1b1914141b0a091b2c2005120914
  hex 0f14050e201001150c200c052002120113130501200a01201701120420130812
  hex 010b05012c200a0f140b01200b05121b1309161b142014010c1405050e200d0f
  hex 0415150c0910050c050a1b201512010b010c0c012e200e1413032d0d15150e0e
  hex 0f13200f0c091309200a1b1b0e19142014050b050d1b14141b2c20050c0c0509
  hex 2004010e09050c2004010c0c0d010e0e200f0c091309200c01080a0f09141401
  hex 0e1514200b0f0e05141401010e2e200c09131b0b1309200d090715050c20070f
  hex 1204090c0c0f131401200f0c0920131515120920011015200e1413032d0b1516
  hex 010e200b0f0f0e200d09141401010d0913051313012e200c0f10150b13092008
  hex 010c15010d0d05200b090914141b1b2012090308011204201314010c0c0d010e
  hex 0901200a01200c090e151320140f1216010c041309012004050d0f0e200b0508
  hex 09141913190d101b120913141c0e2012150e070f1314012e2014090513091414
  hex 050b1c200d151514050e2c200514141b200c090e151320010c0f091414092014
  hex 0905140f0b0f0e05090c150e1301201609032d32303a0c0c013f
text2end:
text3:
; blank padding between texts (I am lazy)
#repeat fWidth/2+2
  dc.b " "
#repend
; The 4th scrolltext.
;;;
; DAS WAR ALLES, UND ES IST DIE ZEIT FÜR BEGRÜSSUNGEN. WIR BEGRÜSSEN
; ALLE VC-20-BENUTZER, BESONDERS PAUL LE BRASSE UND WARD SHRAKE, DIE
; SEHR VIELE VC-20-SPIELMODULE ARCHIVIERT HABEN. UNSEREN LOB BEKOMMEN
; AUCH DANIEL DALLMANN, DER SEINEN NTSC-VC-20-BOARD UNS VERSCHENKT HAT,
; UND MIGUEL GORDILLO, DER UNS BEI DER VERMESSUNG VON NTSC-BILDGRÖSSE
; GEHOLFEN HAT. ZUM SCHLUSS MÖCHTEN WIR NOCH RICHARD STALLMAN UND LINUS
; TORVALDS FÜR DIE ENTWICKLUNGSUMGEBUNG DANKEN. WUSSTET IHR ÜBRIGENS, DASS
; LINUS SEINE RECHNERKARRIÄRE AUF DEM VC-20 ANGEFANGEN HAT?
;;;
  hex 0401132017011220010c0c05132c20150e042005132009131420040905201a05
  hex 0914200626122002050712261313150e07050e2e201709122002050712261313
  hex 050e20010c0c052016032d32302d02050e15141a05122c200205130f0e040512
  hex 13201001150c200c052002120113130520150e04201701120420130812010b05
  hex 2c200409052013050812201609050c052016032d32302d131009050c0d0f0415
  hex 0c05200112030809160905121420080102050e2e20150e130512050e200c0f02
  hex 2002050b0f0d0d050e20011503082004010e09050c2004010c0c0d010e0e2c20
  hex 040512201305090e050e200e1413032d16032d32302d020f01120420150e1320
  hex 160512130308050e0b14200801142c20150e04200d090715050c20070f120409
  hex 0c0c0f2c2004051220150e132002050920040512201605120d051313150e0720
  hex 160f0e200e1413032d02090c0407121c131305200705080f0c06050e20080114
  hex 2e201a150d201303080c151313200d1c030814050e20170912200e0f03082012
  hex 090308011204201314010c0c0d010e20150e04200c090e151320140f1216010c
  hex 0413200626122004090520050e141709030b0c150e0713150d070502150e0720
  hex 04010e0b050e2e201715131314051420090812202602120907050e132c200401
  hex 1313200c090e1513201305090e0520120503080e05120b011212091b12052001
  hex 15062004050d2016032d323020010e070506010e07050e200801143f
text3end:
text4:
; blank padding between texts (I am lazy)
#repeat fWidth/2+2
  dc.b " "
#repend
; The 5th scrolltext.
;;;
; DET VAR DET. NU ER DET TID TIL HILSNER. VORES HILSNER GÅR TIL ALLE
; VIC-20 BRUGERE, SPECIELT PAUL LE BRASSE OG WARD SHRAKE SOM HAR
; ARKIVERET MASSER AF VIC-20 MODULER, OG MIGUEL GORDILLO, SOM HJALP OS
; MED AT MÅLE SKÆRMSTØRRELSEN AF EN 6560. VI SENDER OGSÅ EN HILSEN TIL
; DANIEL DALLMANN, SOM DONEREDE SIN NTSC VIC-20 TIL OS. SIDST, MEN
; IKKE SIDST, VIL VI GERNE TAKKE RICHARD STALLMAN OG LINUS TORVALDS
; FOR UDVIKLINGSMILJØET BRUGT TIL AT REALISERE DEMOEN. NU VI ER VED
; DET, VIDSTE DU FORRESTEN, AT LINUS BEGYNDTE SIN COMPUTERKARRIERE PÅ
; VIC-20'EN?
;;;
  hex 04051420160112200405142e200e1520051220040514201409042014090c2008
  hex 090c130e05122e20160f1205132008090c130e051220071d122014090c20010c
  hex 0c05201609032d323020021215070512052c201310050309050c14201001150c
  hex 200c0520021201131305200f07201701120420130812010b0520130f0d200801
  hex 122001120b091605120514200d0113130512200106201609032d3230200d0f04
  hex 150c05122c200f07200d090715050c20070f1204090c0c0f2c20130f0d20080a
  hex 010c10200f13200d0504200114200d1d0c0520130b24120d1314251212050c13
  hex 050e20010620050e20363536302e2016092013050e040512200f07131d20050e
  hex 2008090c13050e2014090c2004010e09050c2004010c0c0d010e0e2c20130f0d
  hex 20040f0e05120504052013090e200e141303201609032d32302014090c200f13
  hex 2e2013090413142c200d050e20090b0b052013090413142c2016090c20160920
  hex 0705120e052014010b0b052012090308011204201314010c0c0d010e200f0720
  hex 0c090e151320140f1216010c041320060f1220150416090b0c090e07130d090c
  hex 0a2505142002121507142014090c200114201205010c09130512052004050d0f
  hex 050e2e200e1520160920051220160504200405142c2016090413140520041520
  hex 060f1212051314050e2c200114200c090e151320020507190e0414052013090e
  hex 20030f0d10151405120b0112120905120520101d201609032d323027050e3f
text4end:
text5:
; blank padding between texts (I am lazy)
#repeat fWidth/2+2
  dc.b " "
#repend
; The 6th scrolltext.
;;;
; SPECIAL GREETINGS TO SOME IMPORTANT VIC-20 USERS AND NON-VIC20
; SCENERS: JEROEN VAN DRONGELEN, RICHARD MELICK, BEARD/CORONERS,
; LUCA/FIRE, MR.SEX/BYTERAPERS, MOTLEY/F4CG, REPOSE/STYLE, ANDREAS
; BOOSE, CHRISTIAN BAUER, FRANK KONTROS, LANCE EWING, PAUL ROBSON,
; BORIS VAN SCHOOTEN AND EVERYBODY WE HAVE FORGOTTEN.
;;;
  hex 1310050309010c200712050514090e071320140f20130f0d0520090d100f1214
  hex 010e14201609032d323020151305121320010e04200e0f0e2d16090332302013
  hex 03050e0512133a200a05120f050e2016010e2004120f0e07050c050e2c201209
  hex 0308011204200d050c09030b2c2002050112042f030f120f0e0512132c200c15
  hex 03012f060912052c200d122e1305182f021914051201100512132c200d0f140c
  hex 05192f063403072c201205100f13052f1314190c052c20010e04120501132002
  hex 0f0f13052c2003081209131409010e2002011505122c200612010e0b200b0f0e
  hex 14120f132c200c010e0305200517090e072c201001150c20120f02130f0e2c20
  hex 020f1209132016010e201303080f0f14050e20010e04200516051219020f0419
  hex 201705200801160520060f12070f1414050e2e
text5end:
text6:
; blank padding between texts (I am lazy)
#repeat fWidth/2+2
  dc.b " "
#repend
; The 7th scrolltext.
;;;
#if JONAS
; CREDITS FOR THIS PART: * CODE: MARKO MÄKELÄ * MUSIC: JONAS HULTÉN.
; CHARACTER SET: ANDERS CARLSSON AND
#else
; CREDITS FOR THIS PART: * CODE: MARKO MÄKELÄ * MUSIC: ANDERS CARLSSON
; * MUSIC ROUTINE: ASGER ALSTRUP. CHARACTER SET: ANDERS CARLSSON AND
#endif
; MARKO MÄKELÄ *** CREDITS FOR THE WHOLE DEMO: MOST PARTS BY MARKO
; MÄKELÄ AND MOST MUSIC BY ANDERS CARLSSON. * THE VECTOR ROUTINES WERE
; ORIGINALLY WRITTEN FOR THE COMMODORE 64 BY GEORGE TAYLOR AND STEPHEN
; JUDD. * THE ROTATOR AND PLASMA PARTS WERE PROGRAMMED BY ADAM
; BERGSTRÖM, AND THE MUSIC FOR THEM WAS BROUGHT TO YOU BY JONAS
; HULTÉN. * IRQ-LOADER, RASTER ROUTINES, LINKING, CODE ADAPTATION AND
; NTSC FIXING BY MARKO MÄKELÄ. * PUBLIC RELATIONS BY JENS SCHÖNFELD. *
; EVERYTHING ELSE EXCEPT JONAS' MUSIC IS FREE SOFTWARE (SEE THE TEXT
; BELOW).
;;;
#if JONAS
  hex 0312050409141320060f12201408091320100112143a202a20030f04053a200d
  hex 01120b0f200d1b0b050c1b202a200d151309033a200a0f0e01132008150c1422
  hex 0e2e20030801120103140512201305143a20010e04051213200301120c13130f
  hex 0e20010e04200d01120b0f200d1b0b050c1b202a2a2a20031205040914132006
  hex 0f12201408052017080f0c052004050d0f3a200d0f1314201001121413200219
  hex 200d01120b0f200d1b0b050c1b20010e04200d0f1314200d1513090320021920
  hex 010e04051213200301120c13130f0e2e202a2014080520160503140f1220120f
  hex 1514090e05132017051205200f120907090e010c0c19201712091414050e2006
  hex 0f122014080520030f0d0d0f040f12052036342002192007050f120705201401
  hex 190c0f1220010e04201314051008050e200a1504042e202a2014080520120f14
  hex 01140f1220010e0420100c01130d0120100112141320170512052010120f0712
  hex 010d0d0504200219200104010d20020512071314121c0d2c20010e0420140805
  hex 200d1513090320060f12201408050d201701132002120f1507081420140f2019
  hex 0f15200219200a0f0e01132008150c14220e2e202a200912112d0c0f01040512
  hex 2c2012011314051220120f1514090e05132c200c090e0b090e072c20030f0405
  hex 2001040110140114090f0e20010e04200e14130320060918090e07200219200d
  hex 01120b0f200d1b0b050c1b2e202a201015020c09032012050c0114090f0e1320
  hex 0219200a050e13201303081c0e06050c042e202a2005160512191408090e0720
  hex 050c130520051803051014200a0f0e011327200d151309032009132006120505
  hex 20130f06141701120520281305052014080520140518142002050c0f17292e
#else
  hex 0312050409141320060f12201408091320100112143a202a20030f04053a200d
  hex 01120b0f200d1b0b050c1b202a200d151309033a20010e04051213200301120c
  hex 13130f0e202a200d1513090320120f1514090e053a20011307051220010c1314
  hex 1215102e20030801120103140512201305143a20010e04051213200301120c13
  hex 130f0e20010e04200d01120b0f200d1b0b050c1b202a2a2a2003120504091413
  hex 20060f12201408052017080f0c052004050d0f3a200d0f131420100112141320
  hex 0219200d01120b0f200d1b0b050c1b20010e04200d0f1314200d151309032002
  hex 1920010e04051213200301120c13130f0e2e202a2014080520160503140f1220
  hex 120f1514090e05132017051205200f120907090e010c0c19201712091414050e
  hex 20060f122014080520030f0d0d0f040f12052036342002192007050f12070520
  hex 1401190c0f1220010e04201314051008050e200a1504042e202a201408052012
  hex 0f1401140f1220010e0420100c01130d0120100112141320170512052010120f
  hex 0712010d0d0504200219200104010d20020512071314121c0d2c20010e042014
  hex 0805200d1513090320060f12201408050d201701132002120f1507081420140f
  hex 20190f15200219200a0f0e01132008150c14220e2e202a200912112d0c0f0104
  hex 05122c2012011314051220120f1514090e05132c200c090e0b090e072c20030f
  hex 04052001040110140114090f0e20010e04200e14130320060918090e07200219
  hex 200d01120b0f200d1b0b050c1b2e202a201015020c09032012050c0114090f0e
  hex 13200219200a050e13201303081c0e06050c042e202a2005160512191408090e
  hex 0720050c130520051803051014200a0f0e011327200d15130903200913200612
  hex 050520130f06141701120520281305052014080520140518142002050c0f1729
  hex 2e
#endif
text6end:
text7:
; blank padding between texts (I am lazy)
#repeat fWidth/2+2
  dc.b " "
#repend
; The 8th scrolltext.
;;;
; THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
; IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
; THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
; (AT YOUR OPTION) ANY LATER VERSION. THE ORIGINAL DISTRIBUTION (WITH
; SOURCE CODE) IS AVAILABLE FROM HTTP://WWW.FUNET.FI/PUB/CBM/VIC20/DEMOS/.
;;;
  hex 140809132010120f0712010d200913200612050520130f0614170112053b2019
  hex 0f152003010e2012050409131412090215140520091420010e042f0f12200d0f
  hex 0409061920091420150e04051220140805201405120d13200f06201408052007
  hex 0e152007050e0512010c201015020c0903200c0903050e130520011320101502
  hex 0c091308050420021920140805200612050520130f06141701120520060f150e
  hex 040114090f0e3b200509140805122016051213090f0e2032200f062014080520
  hex 0c0903050e13052c200f122028011420190f1512200f1014090f0e2920010e19
  hex 200c011405122016051213090f0e2e20140805200f120907090e010c20040913
  hex 141209021514090f0e20281709140820130f1512030520030f04052920091320
  hex 011601090c01020c052006120f0d20081414103a2f2f1717172e06150e05142e
  hex 06092f1015022f03020d2f16090332302f04050d0f132f2e
text7end:
; blank padding at end of text (I am lazy)
#repeat fWidth/2+2
  dc.b " "
#repend

endcode:

driveinit:	; install the drive routine
; send the m-w command to write the data
mwloop$:
  jsr inidev$
  ldx #lmwcmd$ - 1
smwcmd$:
  lda mwcmd$,x
  jsr ciout
  jsr c1581wait
  dex
  bpl smwcmd$

; send the actual data bytes  
  ldx #0
mwbyte$:
  lda drvcode,x
  jsr ciout
  jsr c1581wait
  inx
  cpx #AMOUNT
  bne mwbyte$

; complete the command
  jsr unlsn

; update the addresses
  clc
  lda #AMOUNT
  adc mwbyte$ + 1
  sta mwbyte$ + 1
  bcc noupdhi1$
  clc
  inc mwbyte$ + 2
noupdhi1$:

  lda #AMOUNT
  adc mwcmd$ + 2
  sta mwcmd$ + 2
  tax
  lda #0
  adc mwcmd$ + 1
  sta mwcmd$ + 1
  cpx #<edrvcode
  sbc #>edrvcode
  bcc mwloop$

; send m-e to start the routine
  jsr inidev$
  lda #"U"
  jsr ciout
  jsr c1581wait
  lda #"3"
  jsr ciout
  jsr c1581wait
; perform the command
  jsr unlsn
  jmp c1581wait

; subroutine: make the current drive listen

inidev$:
  lda fasave
  jsr listn
  lda #$6f
  jmp secnd

; the m-w command backwards

mwcmd$:
  dc.b AMOUNT,>drive,<drive,"W-M"
lmwcmd$ = . - mwcmd$

c1581wait:
  ldy #50
  dey
  bne *-1
  rts

#seg.u tempdata
#org endcode
#align 256
angle: ds.b 1
colours: ds.b barheight

#if . > MEMTOP
  echo "End part is", . - MEMTOP, "bytes too long!"
  err
#endif
