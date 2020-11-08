; Rotator by Adam Bergström
; Restructured and optimized for the demo by Marko Mäkelä

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

#processor 6502

; Jonas Hultén <md6cbm@mdstud.chalmers.se> refused to release the source code
; of his music.  We are sorry for the inconvenience.
MUSIC = 1	;set this to 0, if you want to compile without the music

#seg.u zeropage
#org 0

#if MUSIC
  include "mus.popcorn"
#endif

zpstart:

#include "global.i"
#include "timings.i"
#include "basic.i"

#seg code

#if STANDALONE
  echo "The rotator part does not work on an unexpanded VIC!"
  err
#endif

#org rotator

buff1 = $1c00	;text matrix areas
buff2 = $1e00

; screen resolution
fWidth = 21
fHeight = 24
#if fWidth * fHeight > 512
  echo "The screen is too big!"
  err
#endif

cBg = $11	;white screen, reverse mode

start:
  lda #$7f
  sta $913d	;disable and acknowledge
  sta $913e	;NMIs and IRQs

  ldx #$ff
  txs		;set stack pointer
  ; initialize screen
  ldy #0
  sty $9002	;blank screen (zero screen width)
  lda #(FIRST_COLUMN + LAST_COLUMN - fWidth * 2) / 2
  sta $9000	;screen origin x
  lda #(FIRST_LINE + LINES - fHeight * 8) / 4
  sta $9001	;screen origin y
  lda #fHeight*2
  sta $9003	;screen height
  lda #((buff1 >> 6) & $f0) | $80
  sta $9005	;set screen matrix and ROM font

  ; move the zero/stack page routine to its place
#if zpsize > $100
  ldy #zpsize & $ff
relocate1$:
  lda zpsrc-zpstart+$ff,y
  dey
  sta $100,y
  bne relocate1$
#endif
  ldy #zpstart
relocate0$:
  lda zpsrc-zpstart,y
  sta 0,y
  iny
  bne relocate0$

  jmp mainpart	;jump to the main part

zpsrc:
#rorg zpstart
;-------------------------------
calc:
  ldy #16-11
  sty.z count_y
loop_x:
ycos = . + 1
  lda ysinus,y	;A = y*cos(anglecos)
  sta.z ycosangle

ysin = . + 1
  lda ysinus,y	;A = y*sin(anglesin)
  sta.z ysinangle

  ldy #16-11
  sty.z count_x
loop_pic:
xcos = . + 1
  lda xsinus,y	;A = x*cos(anglecos)
  sec
ysinangle = . + 1
  sbc #0	;A -= y*sin(anglesin)
  tay
  ldx scale_x,y	;x2

  ldy.z count_x
xsin = . + 1
  lda xsinus,y	;A = x*sin(anglesin)
  clc
ycosangle = . + 1
  adc #0	;A += y*cos(anglecos)
  tay
  txa
  ora scale_y,y	;y2 OR x2 -> pic(x,y)

  tay
  lda pic,y
screen = . + 1
  sta $9600
  inc screen
  bne nowrap$
  inc screen + 1
nowrap$:
  inc.z count_x
count_x = . + 1
  ldy #16-11
  cpy #16-11+fWidth
  bne loop_pic

  inc.z count_y
count_y = . + 1
  ldy #16-11
  cpy #16-11+fHeight
  bne loop_x

; increment the angles
sinangle = . + 1
  lda #0
  clc
  adc #1
  and #127
  sta sinangle

  asl
  asl
  asl
  asl
  asl
  sta xsin
  sta ysin

  lda sinangle
  lsr
  lsr
  lsr
  clc
  adc #>xsinus
  sta xsin+1
  adc #>(ysinus - xsinus)
  sta ysin+1

cosangle = . + 1
  lda #32
  clc
  adc #3
  and #127
  sta cosangle

  asl
  asl
  asl
  asl
  asl
  sta xcos
  sta ycos

  lda cosangle
  lsr
  lsr
  lsr
  clc
  adc #>xsinus
  sta xcos+1
  adc #>(ysinus - xsinus)
  sta ycos+1

  rts

mainpart:
; ldy #0
#if MUSIC	;relocate the player to its place
  ldx #>playersize
relocplayer$:
plrs$ = . + 2
  lda playersrc,y
plrd$ = . + 2
  sta playerdest,y
  iny
  bne relocplayer$
  inc plrs$
  inc plrd$
  dex
  bpl relocplayer$

  jsr initplayer

  lda #<irqplay	; set IRQ vector
  sta $314
  lda #>irqplay
  sta $315

  lda #$c0
  sta $912e	;enable the timer interrupts (the interrupt is in its place)
#endif

  ldy #0
  ldx #8
relocysin$:	;relocate and expand the sinus tables
relysrc$ = . + 2
  lda sinussrc + $f00,y
relyds1$ = . + 2
  sta ysinus + $700,y
  sec
  eor #$ff
  adc #0
relyds2$ = . + 2
  sta ysinus + $f00,y
  iny
  bne relocysin$
  dec relysrc$
  dec relyds1$
  dec relyds2$
  dex
  bne relocysin$
  ldx #8
relocxsin$:
relxsrc$ = . + 2
  lda sinussrc + $700,y
relxds1$ = . + 2
  sta xsinus + $700,y
  sec
  eor #$ff
  adc #0
relxds2$ = . + 2
  sta xsinus + $f00,y
  iny
  bne relocxsin$
  dec relxsrc$
  dec relxds1$
  dec relxds2$
  dex
  bne relocxsin$

fill$:		;fill both text matrix buffers and the colour memory
  lda #" "
  sta buff1,y
  sta buff1+$100,y
  sta buff2,y
  sta buff2+$100,y
  lda #1
  sta $9400,y
  sta $9500,y
  sta $9600,y
  sta $9700,y
  iny
  bne fill$

#if MUSIC
  sei
waitraster2$:
  lda $9004	;coarse synchronization will do here
  bne waitraster2$

  lda #cBg
  sta $900f	;set screen colour
  lda #fWidth | ((buff1 >> 2) & $80)
  sta $9002	;set screen width

  lda #<irq	;set the new interrupt vector
  sta $314
  lda #>irq
  sta $315
  cli
#else
  lda #<irq	;set the interrupt vector
  sta $314
  lda #>irq
  sta $315

waitraster$:
  lda $9004	;coarse synchronization will do here
  bne waitraster$

  ldx #<TIMER_VALUE
  ldy #>TIMER_VALUE
  stx $9126	;load the timer low byte latch
  sty $9125	;start Timer A on VIA 1 (IRQ)

  lda #cBg
  sta $900f	;set screen colour
  lda #fWidth | ((buff1 >> 2) & $80)
  sta $9002	;set screen width

  lda #$c0
  sta $912e	;enable the timer interrupts
#endif

waitexit:
  jmp waitexit	;wait for the effect to exit

  jsr loader	;load next part
  bcs .

#if MUSIC	;start fading out the music
  lda #$20
  sta exiting

waitexit2:
  jmp waitexit2
#endif

  jmp (nextpart);start the next part

irq:
  lda $9124	; acknowledge the interrupt
grbuf = . + 1
  lda #fWidth | ((buff1 >> 2) & $80)
  sta $9002	; set the graphics area
#if MUSIC
  jsr play	; play the music
#endif
doexit = .
  lda.w finish$	; placeholder for self-modifying code (exiting the effect)
call$:
  jmp doit$	; calculate next frame
  jmp $eb18	; return to main program

doit$:
  ldx #$ad
  stx call$	; prevent re-entrant calls
  cli		; enable interrupts

  ; calculate next frame
  lda grbuf
  asl		; determine the buffer address
  lda #0
  sta screen
  rol
  rol
  eor #>($9400 + (buff2 & $200))
  sta screen + 1

  jsr calc

  lda grbuf	; swap graphics buffers
  eor #$80
  sta grbuf
  ldx #$4c
  stx call$	; re-enable the subroutine call

  dec exitcnt$
  bne dontexit$
  dec exitcnt$ + 1
  bne dontexit$
  stx doexit
dontexit$:
  jmp $eb18	; return to main program

exitcnt$:
  dc.w $400	; amount of frames to calculate (framerate = 25 or 30 Hz)

finish$:
  lda #0	; exiting the effect
  sta $9002	; zero screen width
  sta $900f	; make the screen black
#if MUSIC
  lda #<irqplay	; set new IRQ vector
  sta $314
  lda #>irqplay
  sta $315
#else
  lda #$7f
  sta $912e	; disable IRQs
#endif
  lda #$ad
  sta waitexit	; notify the main program
  jmp $eb18	; return to main program

#if MUSIC
irqplay:
  jsr play	; play
exiting = .
  lda.w stopmusic$
  lda $9124	; acknowledge the interrupt
  jmp $eb18	; return to main program

stopmusic$:
  lda #$ad
  sta volset	; disable volume settings
stopcnt$ = . + 1
  ldx #0
  dex
  txa
  and #7	; decrement the volume level every 8 frames
  sta stopcnt$
  bne skipit$
  lda $900e
  bne nostop$
stopdone$:
  lda #$7f
  sta $912e	; disable IRQs
  lda #$ad
  sta waitexit2	; notify the main program
  rts
nostop$:
  dec $900e
skipit$:
  rts
#endif

zpsize:
#if . > $1d0
  echo "The zero/stack page is too big!"
  err
#endif
#rend

#align 256

; The picture.
pic:	dc.b 2,1,2,6,6,6,6,7,7,6,6,6,6,6,6,6
	dc.b 2,1,2,6,6,6,6,7,7,6,6,6,6,6,6,6
	dc.b 2,1,2,6,6,6,6,7,7,6,6,6,6,6,6,6
	dc.b 2,1,2,7,7,7,7,7,7,7,7,7,7,7,7,7
	dc.b 2,1,2,7,7,7,7,7,7,7,7,7,7,7,7,7
	dc.b 2,1,2,6,6,6,6,7,7,6,6,6,6,6,6,6
	dc.b 1,1,1,6,6,6,6,7,7,6,6,6,6,6,6,6
	dc.b 2,1,2,6,6,6,6,7,7,6,6,6,6,6,6,6
	dc.b 7,2,0,1,1,1,1,6,6,1,1,1,1,1,1,1
	dc.b 7,2,0,1,1,1,1,6,6,1,1,1,1,1,1,1
	dc.b 7,2,0,1,1,1,1,6,6,1,1,1,1,1,1,1
	dc.b 7,2,0,6,6,6,6,6,6,6,6,6,6,6,6,6
	dc.b 7,2,0,6,6,6,6,6,6,6,6,6,6,6,6,6
	dc.b 7,2,0,1,1,1,1,6,6,1,1,1,1,1,1,1
	dc.b 7,2,0,1,1,1,1,6,6,1,1,1,1,1,1,1
	dc.b 7,2,0,1,1,1,1,6,6,1,1,1,1,1,1,1

scale_y:
	dc.b 112,112,112,112,128,128,128,128,128,128,128,128,144,144,144,144
	dc.b 144,144,144,144,160,160,160,160,160,160,160,160,176,176,176,176
	dc.b 176,176,176,176,192,192,192,192,192,192,192,192,208,208,208,208
	dc.b 208,208,208,208,224,224,224,224,224,224,224,224,240,240,240,240
	dc.b 240,240,240,240,  0,  0,  0,  0,  0,  0,  0,  0, 16, 16, 16, 16
	dc.b  16, 16, 16, 16, 32, 32, 32, 32, 32, 32, 32, 32, 48, 48, 48, 48
	dc.b  48, 48, 48, 48, 64, 64, 64, 64, 64, 64, 64, 64, 80, 80, 80, 80
	dc.b  80, 80, 80, 80, 96, 96, 96, 96, 96, 96, 96, 96,112,112,112,112
	dc.b 112,112,112,112,128,128,128,128,128,128,128,128,144,144,144,144
	dc.b 144,144,144,144,160,160,160,160,160,160,160,160,176,176,176,176
	dc.b 176,176,176,176,192,192,192,192,192,192,192,192,208,208,208,208
	dc.b 208,208,208,208,224,224,224,224,224,224,224,224,240,240,240,240
	dc.b 240,240,240,240,  0,  0,  0,  0,  0,  0,  0,  0, 16, 16, 16, 16
	dc.b  16, 16, 16, 16, 32, 32, 32, 32, 32, 32, 32, 32, 48, 48, 48, 48
	dc.b  48, 48, 48, 48, 64, 64, 64, 64, 64, 64, 64, 64, 80, 80, 80, 80
	dc.b  80, 80, 80, 80, 96, 96, 96, 96, 96, 96, 96, 96,112,112,112,112

scale_x:
	dc.b  7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9
	dc.b  9, 9, 9, 9,10,10,10,10,10,10,10,10,11,11,11,11
	dc.b 11,11,11,11,12,12,12,12,12,12,12,12,13,13,13,13
	dc.b 13,13,13,13,14,14,14,14,14,14,14,14,15,15,15,15
	dc.b 15,15,15,15, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1
	dc.b  1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3
	dc.b  3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5
	dc.b  5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7
	dc.b  7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9
	dc.b  9, 9, 9, 9,10,10,10,10,10,10,10,10,11,11,11,11
	dc.b 11,11,11,11,12,12,12,12,12,12,12,12,13,13,13,13
	dc.b 13,13,13,13,14,14,14,14,14,14,14,14,15,15,15,15
	dc.b 15,15,15,15, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1
	dc.b  1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3
	dc.b  3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5
	dc.b  5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7

sinussrc:
#incbin "6-rotator.bin"

#if MUSIC
playersrc:
  rorg $1000
playerdest:
initplayer:
#if 0
  playerinit	;initialize the player (already done in 5-plasma.s)
  lda #volume
  sta $900e
#else		;This is really ugly, but we got no choice.
  ldx #dataend - datastart - 1
loop0$:
  lda plasmamdata,x
  sta datastart,x
  dex
  bpl loop0$
  ldx #14
loop$:
  lda trackaddr,x
  sec
  sbc #<(plasmamusic - music)
  sta trackaddr,x
  lda trackaddr + 1,x
  sbc #>(plasmamusic - music)
  sta trackaddr + 1,x
  dex
  dex
  bpl loop$
#endif
  rts

  player
playersize = . - playerdest
  if . > lastpart
    echo "The player is", . - lastpart, "bytes too big!"
    err
  endif
  rend
#endif

endcode:

; the sinus tables for calculating the coordinates
#seg.u tables
#org $2000
xsinus: ds.b $1000
ysinus: ds.b $1000

#if xsinus < sinussrc
  echo "The sinus table source is", sinussrc - xsinus, "bytes too high!"
  err
#endif

#if MUSIC
  seg.u mydata
  org $200
  musicdata
  if . > $300
    echo "Music data area is too long!"
    err
  endif
#endif

#if endcode > MEMTOP
  echo "Rotator part is", endcode - MEMTOP, "bytes too long!"
  err
#endif
