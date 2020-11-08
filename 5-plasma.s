; Plasma by Adam Bergström
; Rewritten (restructured, optimized and improved) for the demo by Marko Mäkelä

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

calcounter ds.b 1	;counter for the calculations
framecount ds.b 1	;framecounter for the plasma
zpstart:	;start of the zeropage-based routine

#include "global.i"
#include "timings.i"
#include "basic.i"

#seg code

#if STANDALONE
  basicline $1001, 1996 ; create the BASIC SYS line (unexpanded)
#else
  org plasma
#endif

#if SYSTEM==PAL
rows = 17
#else
rows = 15
#endif
columns	= (LAST_COLUMN - FIRST_COLUMN) / 2

sinus = $200	;sinus table
buff1 = $1000	;text matrix areas
buff2 = $1200
#if STANDALONE
font = $1c00	;character generator
#else
font = $1400	;character generator
#endif

  jmp start

;---------
;Data area

fontsrc:	; the font with shading patterns
  hex fffffffffffffffffffffffffffffffffffff7ffffff7ffffffff7ffffff7fff
  hex ffff77ffffff77ffffff77ffffff77fffdff77ffdfff77fffdff77ffdfff77ff
  hex ddff77ffddff77ffddff77ffddff77ffddff75ffddff57ffddff75ffddff57ff
  hex ddff55ffddff55ffddff55ffddff55ffd5ff55ff5dff55ffd5ff55ff5dff55ff
  hex 55ff55ff55ff55ff55ff55ff55ff55ff55fb55ff55bf55ff55fb55ff55bf55ff
  hex 55bb55ff55bb55ff55bb55ff55bb55ff55bb55ef55bb55fe55bb55ef55bb55fe
  hex 55bb55ee55ab55ee55bb55ee55ab55ee55ba55ee55aa55ee55ba55ee55aa55ee
  hex 55aa55ae55aa55ee55aa55ae55aa55ee55aa55aa55aa55ea55aa55aa55aa55ea
  hex 55aa55aa55aa15aa55aa55aa55aa15aa55aa51aa55aa11aa55aa51aa55aa11aa
  hex 55aa11aa45aa11aa55aa11aa45aa11aa54aa11aa44aa11aa54aa11aa44aa11aa
  hex 44aa11aa44aa01aa44aa11aa44aa01aa44aa10aa44aa00aa44aa10aa44aa00aa
  hex 44aa00aa00aa00aa44aa00aa00aa00aa40aa00aa00aa00aa40aa00aa00aa00aa
  hex 00a200aa002a00aa00a200aa002a00aa002200aa002200aa002200aa002200aa
  hex 0022008a002200a80022008a002200a800220088002200880022008800220088
  hex 0020008800020088002000880002008800000088000000880000008800000088
  hex 0000000800000080000000080000008000000000000000000000000000000000

sinussrc:
  dc.b 32,32,33,34,35,35,36,37,38,38,39,40,41,41,42,43
  dc.b 44,44,45,46,46,47,48,48,49,50,50,51,51,52,53,53
  dc.b 54,54,55,55,56,56,57,57,58,58,59,59,59,60,60,60
  dc.b 61,61,61,61,62,62,62,62,62,63,63,63,63,63,63,63
  dc.b 63,63,63,63,63,63,63,63,62,62,62,62,62,61,61,61
  dc.b 61,60,60,60,59,59,59,58,58,57,57,56,56,55,55,54
  dc.b 54,53,53,52,51,51,50,50,49,48,48,47,46,46,45,44
  dc.b 44,43,42,41,41,40,39,38,38,37,36,35,35,34,33,32
  dc.b 32,31,30,29,28,28,27,26,25,25,24,23,22,22,21,20
  dc.b 19,19,18,17,17,16,15,15,14,13,13,12,12,11,10,10
  dc.b  9, 9, 8, 8, 7, 7, 6, 6, 5, 5, 4, 4, 4, 3, 3, 3
  dc.b  2, 2, 2, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0
  dc.b  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2
  dc.b  2, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9
  dc.b  9,10,10,11,12,12,13,13,14,15,15,16,17,17,18,19
  dc.b 19,20,21,22,22,23,24,25,25,26,27,28,28,29,30,31

zpsrc:
#rorg zpstart
calc:
w2_start = . + 1
  lda #0
  sta.z w2_pos
w3_start = . + 1
  lda #0
  sta.z w3_pos

  ldy #rows
  sty calcounter
loop_x$:
  ldx #columns
w0_start = . + 1
  lda #0
  sta.z w0_pos
w1_start = . + 1
  lda #0
  sta.z w1_pos

w2_pos = . + 1
  lda sinus
w3_pos = . + 1
  adc sinus
  tay
loop_pic$:
  clc
  tya
w0_pos = . + 1
  adc sinus
w1_pos = . + 1
  adc sinus
; clc
intensity = . + 1
  adc #0
  bcc set_it$
  lda #255
set_it$:
  lsr
  lsr
  lsr
plasma_mem = . + 1
  sta buff2
  lda w0_pos
  clc
  adc #7;9
  sta w0_pos
  lda w1_pos
  clc
  adc #5;7
  sta w1_pos
  inc plasma_mem
  bne nowrap$
  inc plasma_mem + 1
nowrap$:
  dex
  bne loop_pic$

  lda w2_pos
  clc
  adc #7;9
  sta w2_pos
  lda w3_pos
  clc
  adc #9;11
  sta w3_pos
  dec calcounter
  bne loop_x$

  ; calculations done, now move the waves
move_waves:
  clc
  lda w0_start
w0_speed = . + 1
  adc #2;1
  sta w0_start
  clc
  lda w1_start
w1_speed = . + 1
  adc #-4;-2
  sta w1_start
  clc
  lda w2_start
w2_speed = . + 1
  adc #6;3
  sta w2_start
  clc
  lda w3_start
w3_speed = . + 1
  adc #-8;-4
  sta w3_start

int$ = . + 1
  lda #0	;change the intensity as needed
  bne decr$
  inc intensity
  bpl finished$
  eor #$ff
  sta int$
colour$ = . + 1
  ldy #1
  dey
  bpl nowrap$
  ldy #colours - 1
nowrap$:
  sty colour$
  lda colourtab,y
  sta.z colour
#if !STANDALONE
exit = . + 1
  lda #$ad	;self-modifying code for exiting the effect
  sta.z doexit
#endif
decr$:
  dec intensity
  bne finished$
  eor #$ff
  sta int$
finished$:
  rts

colourtab:
  dc.b $10,$f0,$b0,$d0,$70,$c0,$90,$30
  dc.b $a0,$e0,$50,$80,$40,$20,$60
  dc.b $20,$40,$80,$50,$e0,$a0
  dc.b $30,$90,$c0,$70,$d0,$b0,$f0
colours = . - colourtab

calcsize = . - calc
#if . > $100
  echo "Zero page overflow by", . - $100, "bytes!"
  err
#endif

irq:
  lda $9124	; acknowledge the interrupt
colour = . + 1
  lda #$10
  sta $900f	;set screen colour
grbuf = . + 1
  lda #columns | ((buff1 >> 2) & $80)
  sta $9002	; set the graphics area
#if MUSIC
  jsr play	; play the music
#endif
#if !STANDALONE
doexit = .
  lda.w finish$	; placeholder for self-modifying code (exiting the effect)
#endif
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
  sta plasma_mem
  rol
  rol
  eor #>buff2
  sta plasma_mem + 1
  jsr calc
  lda grbuf	; swap graphics buffers
  eor #$80
  sta grbuf
  ldx #$4c
  stx call$	; re-enable the subroutine call
  jmp $eb18	; return to main program

#if !STANDALONE
finish$:
  lda #0	; exiting the effect
  sta $9002	; zero screen width
  lda #$7f
  sta $912e	; disable IRQs
  lda #$ad
  sta waitexit	; notify the main program
  jmp $eb18	; return to main program
#endif

mainprog:
; ldy #0
loop$:
  lda fontsrc,y	; copy the font to its place
  sta font,y
  lda fontsrc+$100,y
  sta font+$100,y
  dey
  bne loop$
  ; clear screen
; ldy #0
clear$:
  lda #0	;make everything black
  sta $9400,y
  sta $9500,y
  sta $9600,y
  sta $9700,y
  lda #$1f	;fill with blanks
  sta buff1,y
  sta buff1+$100,y
  sta buff2,y
  sta buff2+$100,y
  iny
  bne clear$

  ; Set up the raster interrupt at vertical blank.

waitraster$:
  lda $9004	;coarse synchronization will do here
  bne waitraster$

  ldx #<TIMER_VALUE
  ldy #>TIMER_VALUE
  stx $9126	;load the timer low byte latch
  sty $9125	;start Timer A on VIA 1 (IRQ)

  lda #columns | ((buff1 >> 2) & $80)
  sta $9002	;set screen width

  lda #$c0
  sta $912e	;enable the timer interrupts

#if STANDALONE
  jmp .		;loop forever
#else
  jsr loader	;load next part
  bcs .

  lda #$4c
  sta exit	;tell the effect to exit

waitexit:
  jmp waitexit	;wait for the effect to exit
  jmp (nextpart);start the next part
#endif

othersize = . - $100
#if othersize > $d0
  echo "Warning: stack is running out."
#endif
#rend

start:
  lda #$7f
  sta $913d	;disable and acknowledge
  sta $913e	;NMIs and IRQs

  ldx #$ff
  txs		;set stack pointer
  ; initialize screen
  ldy #0	;blank screen (zero screen width)
  sty $9002
  lda #FIRST_COLUMN
  sta $9000	;screen origin x
  lda #FIRST_LINE / 2
  sta $9001	;screen origin y
  lda #rows*2+1
  sta $9003	;screen height
  lda #((buff1 >> 6) & $f0) | (font >> 10) | $88
  sta $9005	;font and screen matrix

#if MUSIC	;initialize the player
  playerinit
  lda #volume
  sta $900e
#endif

  ; move the stack page routine to its place
  ldy #othersize
relocate1$:
  lda zpsrc-zpstart+$ff,y
  dey
  sta $100,y
  bne relocate1$

  ; move the zero page routine and the sinus table to their places
relocate0$:
  lda zpsrc-zpstart,y
  sta 0,y
  lda sinussrc,y
  sta sinus,y
  dey
  bne relocate0$

  lda #<irq
  sta $314	;set the IRQ vector
  lda #>irq
  sta $315
  
  lda #$40
  sta $912b	;enable Timer A free run on VIA 1
; ldy #0
  jmp mainprog	;do the rest of initializations

#if MUSIC
#if . < buff2 + $200
  echo "Music will be overwritten by graphics!", .
  err
#endif

  player

  if !STANDALONE && music != plasmamusic
    echo "global.i: Wrong definition, should be plasmamusic =", music
;    err
  endif
endcode:
  seg.u mydata
  org endcode

  if !STANDALONE && . != plasmamdata
    echo "global.i: Wrong definition, should be plasmamdata =", .
;    err
  endif
  musicdata
#endif
top:

#if STANDALONE && top > font
  echo "Plasma part is", top - font, "bytes too long!"
;  err
#endif
#if . > MEMTOP
  echo "Plasma part is", top - MEMTOP, "bytes too long!"
;  err
#endif
