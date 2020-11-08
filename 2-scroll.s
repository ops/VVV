; The second demopart by Marko Mäkelä

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

#processor 6502

#include "timings.i"
#include "global.i"

#if STANDALONE
#echo "This part doesn't work on an unexpanded VIC!"
#err
#endif

; Graphics size
gWidth = 1024 / 8
gHeight = 128	; this must be a power of 2 and smaller than 256

#if 256 % gHeight
#echo "Add destination wraparound checking to the blitting code!"
#err
#endif

; Screen size
fWidth = (LAST_COLUMN - FIRST_COLUMN) / 2
fHeight = gHeight / 16	; we are using the 8x16 char mode

; Graphics buffer addresses
screen = $200
graph = $1000

; Screen origin
orgx = FIRST_COLUMN
orgy = ((LINES - FIRST_LINE - gHeight) / 2 + FIRST_LINE) / 2

; colours
cGr = 1	; graphics colour: white
cBg1 = $7f	; old screen colour: yellow
cBg2 = $f	; new screen colour: black foreground, yellow background
cBg3 = 8	; final screen colour: black

#seg.u variables
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

sndbase = $900a
volumereg = $900e

;---------------
;Other variables
tmp:   dc.b 0
grsrc: dc.w 0

#seg code

  org bitmapscroller

start:
  sei
#if 0 ; The musics have already been initialized.
; Initialize the music
; --------------------
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
#endif

; Screen initializations
; ----------------------

  lda #cBg1
  sta $900f	; set original screen colour
  ldx #0
  stx $9002	; blank screen (zero width)

  lda #cGr	; set the graphics colour
colours$:
  sta $9400 + [screen & $200],x
  inx
  bne colours$

  ldx #$10	; clear the graphics buffer
  ldy #0
  tya
clear$:
  sta graph,y	; self-modifying code
  iny
  bne clear$
  inc clear$ + 2
  dex
  bne clear$

	; synchronize with the screen
  screensync (LINES + FIRST_LINE) / 4 - 4
#if SYSTEM==NTSC
  ldx #5	; extra delay for NTSC
  dex
  bne .-1
#endif
	; set the graphics pointer
  lda #<graphics
  sta grsrc
  lda #>graphics
  sta grsrc + 1

  lda #<(TIMER_VALUE - CYCLES_PER_LINE)
  ldx #>(TIMER_VALUE - CYCLES_PER_LINE)
  sta $9126	; load the timer low byte latch
  stx $9125	; start Timer A on VIA 1 (IRQ)

	; set the screen coordinates
  lda #orgx
  sta $9000	; left edge at the very left
  lda #orgy
  sta $9001	; center the image vertically
	; set the screen size and the graphics buffer addresses
  lda #fWidth + (screen & $200) / 4
  sta $9002
  lda #2 * fHeight + 1
  sta $9003
  lda #((screen / $40) & $f0) + ((graph / $400) & $f) | $88
  sta $9005

	; set the interrupt vectors
  lda #<begint
  sta $314
  lda #>begint
  sta $315
  lda #<endint
  sta $318
  lda #>endint
  sta $319

  ; A small delay (wait CYCLES_PER_LINE-66+22-3 cycles)
  ; - Without this delay, there would be 66 cycles between
  ;   the stx $9125 and stx $9115 instructions.
  ; - The KERNAL NMI handler is 22 cycles faster than the IRQ handler.
  ; - The color is changed 3 cycles later in my NMI routine
  ;   than in my IRQ routine.

#if SYSTEM==PAL
  ldx #4
  bit $24
  dex
  bne . - 1
#else
  ldx #3
  nop
  dex
  bne . - 1
#endif

  lda #<(TIMER_VALUE + CYCLES_PER_LINE)
  ldx #>(TIMER_VALUE + CYCLES_PER_LINE)
  sta $9116	; load the timer low byte latch
  stx $9115	; start Timer A on VIA 2 (NMI)
  cli

  lda #$c0
  sta $912e	; enable the timer interrupts, IRQ
  sta $911e	; and NMI.

  jsr loader	; load the next part
  bcs .		; loop in case of a load error

  lda #$4c
  sta exit	; modify the interrupt routine
waitexit:
  jmp waitexit	; wait for the effect to complete
  jmp (nextpart); start the next part

begint:		; the irq routine
  lda #cBg2
  sta $900f
  lda $9124	; acknowledge the interrupt
  lda $9004
  cmp #orgy + 1
  bcc nextpart$
  jmp domusic	; do music and return (pla:tay:pla:tax:pla:rti)

nextpart$:
  lda #<TIMER_VALUE
  ldx #>TIMER_VALUE
  sta $9116
  stx $9117	; freeze the timers
  sta $9126
  stx $9127

  lda #<notdone
  sta $314	; set new IRQ vector
  lda #>notdone
  sta $315

  lda #<scrollscr
  sta $318	; set new NMI vector
  lda #>scrollscr
  sta $319

  jmp domusic	; do music and return (pla:tay:pla:tax:pla:rti)

endint:		; the NMI routine
  pha
  lda #cBg1
  sta $900f	; set the colors
  lda $9114	; acknowledge the interrupt
  pla
  rti		; exit to main program

	; another NMI routine
scrollscr:
  pha		; store the flags
  txa
  pha
  tya
  pha
  lda $9114	; acknowledge the interrupt

scrpos1 = . + 1
  ldx #orgx	; self-modifying code
  stx $9000
  cpx #orgx
  beq hardscroll$
  ldx #orgx
  stx scrpos1
  jmp $eb18	; return to the main program

hardscroll$:
  dex
  stx scrpos1
scrpos2 = . + 1
  ldx #0	; self-modifying code for screen position
  txa
  clc
  adc #fHeight
  sta scrpos2

  lda #0	; scroll the text matrix
  clc
nextcol$:
  sta tmp
  ldy #fHeight
nextrow$:
  sta scr$
scr$ = . + 1
  stx screen
  inx
  adc #fWidth
  dey
  bne nextrow$
  sec
  lda tmp
  adc #0
  cmp #fWidth
  bcc nextcol$

	; blit the graphics in the new column
  clc		; calculate destination address
  lda #graph / $1000
  sta grdest1$ + 1
  lda screen + fWidth - 1
  asl
  rol grdest1$ + 1
  asl
  rol grdest1$ + 1
  asl
  rol grdest1$ + 1
  asl
  rol grdest1$ + 1
  sta grdest1$
  sta grdest2$
  lda grdest1$ + 1
  sta grdest2$ + 1

  ldx #0	; blit one graphics column
  ldy #0
blitloop$:
  lda (grsrc),y	; get length of data packet
  sta tmp
  bmi raw$	; branch if it is raw data
  iny
  lda (grsrc),y
  iny
rle$:	; block of run-length encoded data
grdest1$ = . + 1
  sta graph,x	; self-modifying address
  inx
  dec tmp
  bpl rle$
  cpx #gHeight
  bcc blitloop$
  bcs blitdone$	; branch always

raw$:	; block of raw data
  iny
  lda (grsrc),y	; get the data byte
grdest2$ = . + 1
  sta graph,x	; self-modifying address
  inx
  dec tmp
  bmi raw$
  iny
  cpx #gHeight
  bcc blitloop$

blitdone$:
  tya		; increment the source bitmap address
  clc
  adc grsrc
  tay
  lda #0
  adc grsrc + 1
  sta grsrc + 1
		; check if the scroller is done
  cpy #<graphend
  sbc #>graphend
  bcc nowrap$

exit = .
  lda nextpart$	; this op code is modified by the main program
  ldy #<graphics
  lda #>graphics
  sta grsrc + 1
nowrap$:
  sty grsrc
  jmp $eb18	; return to main program

nextpart$:	; scroller finished.  now increment the screen size.
  lda #<endint
  sta $318	; set new NMI vector
  lda #>endint
  sta $319

  lda volfadespd
  bne fading$
  lda #7	; fade out the volume in 7 frames between volume changes
  sta volfademode
  sta volfadespd
fading$:

	; load the timers
  lda #<(TIMER_VALUE - CYCLES_PER_LINE)
  ldx #>(TIMER_VALUE - CYCLES_PER_LINE)
  sta $9126	; load the timer latch
  stx $9127

  lda #<(TIMER_VALUE + CYCLES_PER_LINE)
  ldx #>(TIMER_VALUE + CYCLES_PER_LINE)
  sta $9116	; load the timer latch
  stx $9117

  lda #<begint2
  sta $314
#if >begint2 != >notdone
  lda #>begint2
  sta $315
#endif

  jmp $eb18	; return to main program (pla:tay:pla:tax:pla:rti)

begint2:	; the irq routine
  lda #cBg3
  sta $900f
  lda $9004
  cmp #FIRST_LINE / 2
  bcs notdone
  lda #$7f
  sta $912e	; disable the interrupts
  sta $911e
  lda #$ad
  sta waitexit	; modify the main program
notdone:
  lda $9124	; acknowledge the interrupt
domusic:	; play music and return (pla:tay:pla:tax:pla:rti)
  ;Volume and volume fade control:

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
  cmp #setvolcode
  beq processsetvol$
  cmp #volfadecode
  beq processfadevol$

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

donext$:
  iny
  bne next2$	;branch always (tunes < 256 bytes)

;Music-data
;----------

#include "mus.säkkijärvi"

graphics:
  incbin "2-scroll.bin"

#repeat fWidth	; empty space at end of scroller
temp set gHeight - 1
#  repeat 1 + gHeight / 128
     dc.b temp & 127, 0
#    if temp > 128
       temp set temp - 128
#    endif
#  repend
#repend
graphend:

#if . > copperscroller
#echo "The scroller part is too long!"
#err
#endif
