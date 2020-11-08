; The first demopart by Marko Mäkelä

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

#processor 6502

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

;----------------
; Other variables
blitcnt: dc.b 0	; counter: amount of characters to blit

#seg code

#include "global.i"
#include "timings.i"
#include "basic.i"

CURTAIN_SPEED = 2	; screen blanking speed in lines per frame

#if STANDALONE
  basicline $1001, 1996	; create the BASIC SYS line (unexpanded)
#else
  org chessboardzoom
#endif

intro:
  jsr initmusic
#if STANDALONE	; include the curtain effect with the stand-alone version only
  ; initialize the timers for the interrupts
  screensync 0
#if SYSTEM==NTSC
  ldx #13	; extra delay for NTSC
  dex
  bne .-1
#endif
  lda #$40
  sta $912b	; enable Timer A free run on VIA 1
  sta $911b	; and on VIA 2

  lda #<TIMER_VALUE
  ldx #>TIMER_VALUE
  sta $9126	; load the timer low byte latch
  stx $9125	; start Timer A on VIA 1 (IRQ)

  lda #<curtains
  sta $314	; set the IRQ vector
  lda #>curtains
  sta $315

  lda #<curtaine
  sta $318	; set the NMI vector
  lda #>curtaine
  sta $319

  lda $9005
  and #$f2	; Fix character generator at $8000 or $8800.
  sta $9005

  ; A small delay (wait CYCLES_PER_LINE-48+22-11 cycles)
  ; - Without this delay, there would be 48 cycles between
  ;   the stx $9125 and stx $9115 instructions.
  ; - The KERNAL NMI handler is 22 cycles faster than the IRQ handler.
  ; - The color is changed 11 cycles later in my NMI routine
  ;   than in my IRQ routine.
#if SYSTEM == PAL
  ldx #6
  dex
  bne *-1
  bit $24
#else
  ldx #5
  dex
  bne *-1
  nop
#endif

  lda #<(TIMER_VALUE + CURTAIN_SPEED * CYCLES_PER_LINE)
  ldx #>(TIMER_VALUE + CURTAIN_SPEED * CYCLES_PER_LINE)
  sta $9116	; load the timer low byte latch
  stx $9115	; start Timer A on VIA 2 (NMI)

  lda $900f
  sta color	; store the original screen color
  lda $9002
  sta orig9002	; and screen width
  eor #$80
  sta blank9002

  asl
  lda #$94 / 4
  rol
  asl
  sta color1$
  adc #1
  sta color2$
  lda #cBg1 & 7	; create a text page with the next part's colour
  ldx #0
  stx $900e	; disable sound output
fill$:
color1$ = . + 2
  sta $9600,x
color2$ = . + 2
  sta $9700,x
  inx
  bne fill$

  lda #$c0
  sta $912e	; enable the timer interrupts, IRQ
  sta $911e	; and NMI.

  jmp .		; loop forever in the stand-alone version
#else ; !STANDALONE
  lda #<flagsinit
  sta $314
  lda #>flagsinit
  sta $315
  ldx #1
  stx $9126	; load the timer low byte latch
  dex
  stx $9125	; start Timer A on VIA 1 (IRQ)
  lda #$c0
  sta $912e	; enable the timer interrupts

  jsr loader	; load the next part with the fast loader
  bcs .		; loop if a disk error occurred

exitruns = . + 1
  lda #2	; minimum number of times to zoom back and forth
  bne . - 2

  lda #initsqr
  sta maxwidth	; set new maximum square size
  lda #$20
  sta exit	; quit the chessboard effect when the maximum is reached
waitexit:
  jmp waitexit	; wait for the effect to complete
  jmp (nextpart)
#endif

#if STANDALONE
curtains:	; IRQ routine (top of the curtain)
  lda #cBg1	; set the background color of the next part
  sta $900f
blank9002 = . + 1
  lda #22	; placeholder for self-modifying code
  sta $9002	; blank the screen
  jmp domusic	; ack interrupt, play music and return

curtaine:	; NMI routine (bottom of the curtain)
  pha
  lda $9004	; check if we are at end of screen
  cmp #LINES / 2 - 1
  bcs nextpart$
color = . + 1
  lda #0	; placeholder for self-modifying code
  sta $900f	; restore the screen color
orig9002 = . + 1
  lda #$80 + 22 ; placeholder for self-modifying code
  sta $9002	; unblank the screen
  lda $9114	; acknowledge the NMI
  pla
  rti

  ; prepare for next part
nextpart$:
#endif ; STANDALONE

flags:
  txa		; store all registers
  pha
  tya
  pha
flagsinit:

base = $1c00    ; the text memory base is at $1c00
cHeight = 16	; character height (8 or 16 pixels)
cbase = $2000 - 256 * cHeight
		; the character generator is at $1800 or at $1000
#if SYSTEM==PAL
fWidth set 18	; amount of chars per line required for the flag
fHeight = 36 * 8 / cHeight
		; amount of text lines required for the flag
#else
fWidth set 17	; NTSC has less lines => less columns
fHeight = 30 * 8 / cHeight
#endif
xoff = 256 - cHeight
chars set cbase + cHeight * (256 - fWidth)
rightadr = cbase + cHeight * (256 - fWidth / 2) - xoff
leftadr = rightadr - cHeight

#if fWidth * (fHeight + cHeight) > 1024
#echo "too big text+char matrix"
#err
#endif

  ldx #0
  stx $9002     ; blank the screen (zero width)
#if cbase == $1000
  ldx #$fc
#else
  ldx #$fe
#endif
  stx $9005	; set text page at $1c00 and character base at $1800 or $1000

  lda #<base    ; initialize the text matrix
  sta addr$
  lda #>base
  sta addr$ + 1
  ldy #fHeight
nextline$:
  ldx #(chars - cbase) / cHeight
nextchar$:
addr$ = . + 1
  stx base      ; placeholder for self-modifying code
  inc addr$
  bne noinc$
  inc addr$+1
noinc$:
  inx
  bne nextchar$
  dey
  bne nextline$

  lda #<leftadr ; initialize the character memory pointers
  sta leftend
#if cHeight * fWidth > 256
  lda #>leftadr	; update also the high address
  sta leftend + 1
#endif
  lda #<rightadr
  sta rightend
  lda #$01	; and the graphics values
  sta leftval
  lda #$80
  sta rightval

  lda #0	; clear the character images
#if fWidth * cHeight > 256
  ldx #fWidth * cHeight - 256
clear1$:
  dex
  sta chars + 256,x
  bne clear1$
clear2$:
  dex
  sta chars,x
  bne clear2$
#else
  ldx #fWidth * cHeight
clear$:
  dex
  sta chars,x
  bne clear$
#endif
 
  lda colorf    ; set up the color memory
  and #7
#if fWidth * fHeight > 768
  ldx #fWidth * fHeight - 768
color1$:
  dex
  sta $9700,x
  bne color1$
color2$:
  dex
  sta $9400,x
  sta $9500,x
  sta $9600,x
  bne color2$
#else
color$:
  dex
  sta $9400,x
  sta $9500,x 
  sta $9600,x
  bne color$
#endif

  screensync (LINES + FIRST_LINE) / 4 - 5
#if SYSTEM==NTSC
  ldx #5	; extra delay for NTSC
  dex
  bne .-1
#endif

  lda #FIRST_COLUMN
  sta $9000	; set top left text matrix corner at the very left
  lda #FIRST_LINE / 2
  sta $9001	; and at the very top

  lda #<(TIMER_VALUE - 2 * CYCLES_PER_LINE)
  ldx #>(TIMER_VALUE - 2 * CYCLES_PER_LINE)
  sta $9126	; load the timer low byte latch
  stx $9125	; start Timer A on VIA 1 (IRQ)

  lda #fWidth
  sta $9002	; set text width and fix text matrix start at $1c00
#if cHeight == 8
  lda #fHeight * 2
#else
  lda #fHeight * 2 + 1
#endif
  sta $9003	; set text height

  lda #<stripes
  sta $314
  lda #>stripes
  sta $315	; set the new IRQ vector
  lda #<stripee
  sta $318
  lda #>stripee
  sta $319	; set the new NMI vector

  ; A small delay (wait CYCLES_PER_LINE-48+22-11 cycles)
  ; - Without this delay, there would be 48 cycles between
  ;   the stx $9125 and stx $9115 instructions.
  ; - The KERNAL NMI handler is 22 cycles faster than the IRQ handler.
  ; - The color is changed 11 cycles later
  ;   in my NMI routine than in my IRQ routine.
#if SYSTEM == PAL
  ldx #6
  dex
  bne *-1
  bit $24
#else
  ldx #5
  dex
  bne *-1
  nop
#endif

  lda #<(TIMER_VALUE + 2 * CYCLES_PER_LINE)
  ldx #>(TIMER_VALUE + 2 * CYCLES_PER_LINE)
  sta $9116
  stx $9115	; start Timer A on VIA 2 (NMI)

  lda #$c0
  sta $913e	; enable the timer interrupts, IRQ and NMI.
  sta $913d	; acknowledge the NMI and any IRQ that might have occurred
  jmp $eb18	; return to main program (pla:tay:pla:tax:pla:rti)

cBg1 = $2a	; first background color: red
cFg1 = $19	; first foreground color: white
cBg2 = cFg1
cFg2 = $6e	; second foreground color: blue
cBg3 = cFg2
cFg3 = $7f	; third foreground colour: yellow

stripes:
colorf = . + 1
  lda #cFg1	; placeholder for self-modifying code
  sta $900f

  ; widen the vertical bar
  ldx #xoff
widen$:
leftval = . + 1
  lda #$01	; placeholder for self-modifying code
leftend = . + 1
  sta leftadr,x ; placeholder for self-modifying code
rightval = . + 1
  lda #$80	; placeholder for self-modifying code
rightend = . + 1
  sta rightadr,x; placeholder for self-modifying code
  inx
  bne widen$

; update the graphics values and addresses
  lda leftval
  sec
  rol
  bcc noupd1$
  lda leftend
  sbc #cHeight	; carry=1
  sta leftend
#if cHeight * fWidth > 256
  bcs nofix$
  dec leftend+1	; fix the high address
nofix$:
#endif
  lda #0
noupd1$:
  sta leftval

  lda rightval
  sec
  ror
  bcc noupd2$
  lda rightend	; carry=1
  adc #cHeight - 1
  sta rightend
  lda #0
noupd2$:
  sta rightval

  jmp domusic	; ack interrupt, play music and return

stripee:
  pha
  lda $9004	; check if we are at end of screen
  cmp #FIRST_LINE / 2
  bcc nextflag$
colorb = . + 1
  lda #cBg1	; placeholder for self-modifying code
  sta $900f	; restore the screen color
  lda $9114	; acknowledge the NMI
  pla
  rti

nextflag$:
  lda colorb
  cmp #cBg2
  beq colour3$
  cmp #cBg1
  bne nextpart$
  lda #cBg2
  sta colorb
  lda #cFg2
  bne setcolour$	; branch always
colour3$:
  lda #cBg3
  sta colorb
  lda #cFg3
setcolour$:
  sta colorf
  jmp flags

nextpart$:

cChess = 8	; chessboard color: black

chess:
  txa		; store all registers
  pha
  tya
  pha

; full overscan width
fWidth set (LAST_COLUMN - FIRST_COLUMN) / 2
chars set cbase + cHeight * (256 - fWidth)

#if fWidth * (fHeight + cHeight) > 1024
#echo "too big text+char matrix"
#err
#endif

  ldx #0
  stx $9002     ; blank the screen (zero width)

  lda #<base    ; initialize the text matrix
  sta addr$
  lda #>base
  sta addr$ + 1
  ldy #fHeight
nextline$:
  ldx #(chars - cbase) / cHeight
nextchar$:
addr$ = . + 1
  stx base      ; placeholder for self-modifying code
  inc addr$
  bne noinc$
  inc addr$+1
noinc$:
  inx
  bne nextchar$
  dey
  bne nextline$

  ldx #>chars	; Fill the characters (both screens).
  ldy #fWidth * 8 - 1
  jsr blitbars
  ldx #>(chars - $400)
  ldy #fWidth * 8 - 1
  jsr blitbars

syncirq$:
  lda $9004	; synchronize the IRQ timing (exact accuracy not required)
  bne syncirq$

  lda #<TIMER_VALUE
  ldx #>TIMER_VALUE
  sta $9126	; load the timer low byte latch
  stx $9125	; start Timer A on VIA 1 (IRQ)

  screensync 9	; synchronize the NMI timing
#if SYSTEM==NTSC
  ldx #5	; extra delay for NTSC
  dex
  bne .-1
#endif

  lda #<chessy
  sta $314
  lda #>chessy
  sta $315	; set the new IRQ vector
  lda #<chessx
  sta $318
  lda #>chessx
  sta $319	; set the new NMI vector

  lda #<TIMER_VALUE
  ldx #>TIMER_VALUE
  sta $9116	; load the timer low byte latch
  stx $9115	; start Timer A on VIA 2 (NMI)

  lda #$c0
  sta $913e	; enable the timer interrupts, IRQ and NMI.
  sta $913d	; acknowledge the NMI and any IRQ that might have occurred

  lda #fWidth
  sta $9002	; set text width

  jmp $eb18	; return to main program (pla:tay:pla:tax:pla:rti)

; define initial square size
initsqr = 8 * fWidth - 1

; irq routine: modify the square size and blit the vertical bars
chessy:
  ldy sizeadj	; get the square size (0-based)

dir$ = . + 1
  lda #1
  bne decr$
  iny		; increment it
maxwidth = . + 1
  cpy #(LINES - FIRST_LINE) / 2
  bcc nochg$
#if !STANDALONE
exitruns$ = . + 1
  lda exitruns	; minimum number of times to zoom back and forth
  beq exit
  dec exitruns
exit = .
  lda exit$	; call the exit routine (self-modifying opcode)
#endif
  sty dir$	; change the direction when it reaches the maximum
decr$:
  dey		; decrement the square size
  bne nochg$
  sty dir$	; change the direction when it reaches zero
nochg$:
  sty sizeadj	; set the square size (0-based)
  iny		; increment the square size (it must be 1-based)

  lda $9005	; switch the character sets (double buffering)
  eor #7
  sta $9005
  and #3	; see which character set to blit
  beq blit2nd$
  ldx #>chars	; blit the normal charset
  dc.b $2c	; skip the following instruction
blit2nd$:	; blit the alternate charset
  ldx #>(chars-$400)
  jsr blitbars	; do the blitting
exitirq$:
  jmp domusic	; ack interrupt, play music and return

#if !STANDALONE
exit$:		; exit the effect
  lda #$7f
  sta $911e	; disable NMIs
  sta $912e	; and IRQs
  lda #0
  sta $9002	; blank the screen (zero text width)
  pla		; remove the return address
  pla
  lda #$ad
  sta waitexit	; notify the main program
  bne exitirq$	; exit the interrupt (branch always)
#endif

; nmi routine: create the horizontal lines
chessx:
  pha
  txa
  pha
color$ = . + 1
  lda #cChess
  sta $900f	; set the colors
  ldx $9114	; acknowledge the interrupt
counter$ = . + 1
  ldx #0
  beq idle$	; counter zero => this is the last interrupt of the frame
  eor #8
  sta color$	; invert the colors
  dex
  stx counter$	; update the counter
  beq last$	; counter one => last interrupt but one
  pla
  tax
  pla
  rti

last$:		; set the idle timer
  ldx square$
  cpx #(LINES - FIRST_LINE) / 2
  bcc nolimit1$
  ldx #(LINES - FIRST_LINE) / 2 - 1
nolimit1$:
  lda nmidelay_lo,x
  sta $9116
  lda nmidelay_hi,x
  sta $9117
sizeadj = . + 1
  ldx #initsqr	; adjust the square size
  stx square$
  pla
  tax
  pla
  rti

idle$:
  ora #8
  sta color$	; set normal colors
square$ = . + 1
  ldx #initsqr
  cpx #(LINES - FIRST_LINE) / 2
  bcc nolimit$
  ldx #(LINES - FIRST_LINE) / 2 - 1
nolimit$:
  lda nmicount,x
  sta counter$	; reset the counter
		; set the timers
  lda nmirate_lo,x
  sta $9116
  lda nmirate_hi,x
  sta $9117
  pla
  tax
  pla
  rti

;; Subroutines.

; blitbars: blit the vertical lines for the chessboard screen
; --------
; Parameters:
;   .X: high address of the character set
;   .Y: square size (1=smallest)
; Registers affected:
;   .A .X .Y .P

blitbars:
  sty size1$	; store the square sizes
  sty size2$
  cpy #fWidth+1	; check the square size
  bcc nolimitw$
  ldy #fWidth	; limit the blitting index if necessary
nolimitw$:

  sty blitcnt	; store the amount of characters to blit
		; store the character set start address
  stx copysrc$ + 1

  tya		; calculate the character address
  asl
  asl
  asl
#if cHeight == 16
  asl
  bcc noinx$
  inx		; increment the high byte if necessary
  clc
noinx$:
#endif
  adc #<chars
  bcc noinx2$
  inx
noinx2$:
  sta blitadr	; store the character set addresses
  stx blitadr + 1
  sta copyadr$
  stx copyadr$ + 1

  lda #0	; load initial blitting value

smain$:		; create the bit patterns
size1$ = . + 1
  ldx #0	; square size

  ora #0	; see if there is any pattern being blitted
  bne sloop$
  cpx #8	; see if the amount of bits to blit is more than 8
  bcc sno8$
sloop8$:
  lda #$ff	; blit eight '1' bits
  jsr blit$	; blit the graphics, exit on completion

  txa
  sbc #8
  beq cmain$
  tax
  cpx #8
  bcs sloop8$

sno8$:
  lda #$80	; load initial safeguard value

; subloop: create a string of X '1' bits
sloop$:
  sec		; rotate one '1' bit from left
  ror
  bcc snoblit$	; skip the blitting until the byte is complete
  jsr blit$	; blit the graphics, exit on completion
  cpx #9	; optimize the blitting (blit full bytes if possible)
  bcc snoopt$
  dex
  bne sloop8$
snoopt$:
  lda #$80	; load the safeguard value
snoblit$:
  dex
  bne sloop$	; continue making '1' bits until the counter reaches zero

cmain$:
size2$ = . + 1
  ldx #0	; square size
  ora #0	; see if there is any pattern being blitted
  bne cloop$
  cpx #8	; see if the amount of bits to blit is more than 8
  bcc cno8$
cloop8$:
  lda #0	; blit eight '0' bits
  jsr blit$	; blit the graphics, exit on completion
  txa
  sbc #8
  beq smain$
  tax
  cpx #8
  bcs cloop8$

cno8$:
  lda #$80	; load the safeguard value

; subloop: create a string of X '0' bits
cloop$:
  lsr		; rotate one '0' bit from left
  bcc cnoblit$	; skip the blitting until the byte is complete
  jsr blit$	; blit the graphics, exit on completion
  cpx #9	; optimize the blitting (blit full bytes if possible)
  bcc cnoopt$
  dex
  bne cloop8$
cnoopt$:
  lda #$80	; load the safeguard value
cnoblit$:
  dex
  bne cloop$	; continue making '0' bits until the counter reaches zero
  beq smain$	; branch always

blit$:		; blit a character, decrement the address and quit on competion
  tay
  lda blitadr	; first decrement the address
  sec
  sbc #cHeight
  sta blitadr
  bcs blitit$
  dec blitadr+1
  sec

blitit$:
  tya
  ldy #cHeight - 1

blitchar$:
blitadr = . + 1
  sta chars,y
  dey
  bpl blitchar$

  dec blitcnt	; decrement the counter to see if all chars are blitted
  beq finish$
  rts

finish$:
  pla		; remove the return address
  pla

  lda copyadr$ + 1
  and #3
  beq complete$
  ldx #0
  and #1
  beq nocnt$
  lda copyadr$
  eor #$ff	; set the counter to avoid an overrun
  tax
  inx
nocnt$:
  ldy #0	; copy the remaining characters
copyloop$:
copysrc$ = . + 1
  lda chars,y
copyadr$ = . + 1
  sta chars,y
  iny
  beq incadr$
  dex
  bne copyloop$
incadr$:	; increment the high addresses
  inc copysrc$ + 1
  inc copyadr$ + 1
  lda copyadr$ + 1
  and #1
  beq complete$
  lda copyadr$
  eor #$ff	; set the counter to avoid an overrun
  tax
  inx
  jmp copyloop$
complete$:
  rts

nmicount:	; table: number of color change NMIs per frame
n set 1		; index: chessboard square size
#repeat (LINES - FIRST_LINE) / 2
	dc.b (LINES - FIRST_LINE) / 2 / n
n set n + 1
#repend

nmidelay_lo:	; table: cycles spent between the lowmost and the topmost NMI
n set 1		; index: chessboard square size
#repeat (LINES - FIRST_LINE) / 2
	dc.b <((FIRST_LINE + ((LINES - FIRST_LINE) % (2 * n))) * CYCLES_PER_LINE - VIA_RELOAD_TIME)
n set n + 1
#repend

nmidelay_hi:
n set 1
#repeat (LINES - FIRST_LINE) / 2
	dc.b >((FIRST_LINE + ((LINES - FIRST_LINE) % (2 * n))) * CYCLES_PER_LINE - VIA_RELOAD_TIME)
n set n + 1
#repend

nmirate_lo:	; table: number of cycles between the color inverting NMIs
n set 1		; index: chessboard square size
#repeat (LINES - FIRST_LINE) / 2
	dc.b <(2 * n * CYCLES_PER_LINE - VIA_RELOAD_TIME)
n set n + 1
#repend

nmirate_hi:
n set 1
#repeat (LINES - FIRST_LINE) / 2
	dc.b >(2 * n * CYCLES_PER_LINE - VIA_RELOAD_TIME)
n set n + 1
#repend

sndbase = $900a
volumereg = $900e

domusic:	; do music
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

  lda $9124		;acknowledge the interrupt
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

progend:
; The following program data may be overwritten.

initmusic:	; initialize the music
  ldx #2
loop$:
  lda #1
  sta delaybase,x
  sta delay,x

  lda #0
  sta pos,x	; reset the music positions
  sta sndbase,x	; disable channels 0-2

  dex
  bpl loop$

  sta volfademode	;No fading
  sta sndbase+3		;disable channel 3 output
  lda #8
  sta volumereg
  rts
