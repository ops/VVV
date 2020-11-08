; The fourth demopart by Marko Mäkelä
; (portions by Stephen Judd and George Taylor)

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

#processor 6502

#include "global.i"
#include "timings.i"

#if STANDALONE
#echo "This part does not work on an unexpanded VIC!"
#err
#endif

HIDDEN_FACES = 1	; set this to 0 if you don't want hidden face removal

; zero page variables

#seg.u zeropage
#org 0

#include "asger.i"
  musicdata

;----------------
; Other variables
buffer ds.b 2
p1x ds.b 1	;these are temporary storage used in plotting the projection
p1y ds.b 1
p1z ds.b 1
p2x ds.b 1
p2y ds.b 1
p2z ds.b 1
p3x ds.b 1
p3y ds.b 1
p3z ds.b 1
p1t ds.b 1
p2t ds.b 1
p3t ds.b 1

x1 = p1t	; points for drawing a line
y1 = p2t
x2 = p3t
y2 = p3z

oldx = x2	; line drawing variables
chunk = y2

dx = p1z	;line stepping variables
dy = p2z

index	 ds.b 1
countpts ds.b 1

sx ds.b 1	;these are the actual angles in x y and z
sy ds.b 1
sz ds.b 1

dsx ds.b 1	;and these are the angle increments (rotation speeds)
dsy ds.b 1
dsz ds.b 1

t1 = p1x	; these are used as temporary variables
t2 = p1y	; when calculating the rotation matrix
t3 = p1z
t4 = p2x
t5 = p2y
t6 = p2z
t7 = p3x
t8 = p3y
t9 = p3z
t10 = p1t

r11 ds.b 1 ;these are the elements of the xyz rotation matrix
r12 ds.b 1
r13 ds.b 1
r21 ds.b 1 ;the number denotes (row,column)
r22 ds.b 1
r23 ds.b 1
r31 ds.b 1
r32 ds.b 1
r33 ds.b 1

gr ds.w 1	; graphics pointers for the scroll text
gr1 ds.w 1
gr2 ds.w 1

#seg code
#org vectormania

; constants

buff1 = $1000 ;first character set
buff2 = $1400 ;second character set
screen = $200 ;text matrix
scroll = screen + $80 ; scroll buffer

fontstart = $1800 ;start address of the scroll text font

volumereg = $900e
sndbase = $900a

angmax = 128 ;there are angmax angles

cFg1 = 4 ; vector foreground (background) colour: purple
cFg2 = 9 ; scroll text foreground colour: white (multicolour)
cBg1 = $96 ; background colour for the vectors: blue/lt. orange, inverse mode
cBg2 = $ee ; background colour for the scroll: blue/lt. blue
cAx2 = 7   ; auxiliary colour for the scroll: yellow

twidth = ((LAST_COLUMN - FIRST_COLUMN) / 2 + 1) & ~1 ; scroll text width
fWidth = 16 ; vector area width in characters
fHeight = 128 ; vector area height in pixels

SCR_Y = FIRST_LINE + (LINES - FIRST_LINE - fHeight) / 2

; set up the video chip
setup:
  sei
  lda #$7f
  sta $913e	; disable and acknowledge any interrupts
  sta $913d

  lda #0
  sta $9002	; zero screen width (blank screen during setup)

  tay		; set up the character matrix
  tax
  clc
chars$:
  txa
  and #$1f
  sta screen,y
  adc #4	; There are 4 character rows (and 8 columns).
  tax
  iny
  tya
  and #$0f
  bne chars$
  inx
  tya
  eor #$40
  bne notyet$
  txa
  adc #$0c
  tax
  bne chars$	; branch always
notyet$:
  bpl chars$

  lda #cFg1	; set foreground colour
colours$:
  dey
  sta $9400 + [screen & $200],y
  bpl colours$

  ldy #twidth+1	; set up the scroll text
  ldx #" "+twidth+2
scrollclr$:
  dex
  txa
  sta scroll,y
  lda #cFg2
  sta $9400 + [scroll & $3ff],y
  dey
  bpl scrollclr$

; initialize the music
  lda #cAx2 << 4	;set the auxiliary colour for the scroller
  sta $900e

;*** set up initial values

  lda #1
  sta dsx
  lda #2
  sta dsy
  lda #3
  sta dsz
  ldy #0
  sty sx
  sty sy
  sty sz

;*** relocate the rest of the code
  ldx #>relend - >relstart
reloc$:
relocsrchi$ = . + 2
  lda relend-256,y
relocdesthi$ = . + 2
  sta endcode-256,y
  iny
  bne reloc$
  dec relocsrchi$
  dec relocdesthi$
  dex
  bne reloc$

  jmp codestart

relstart:	; the rest will be relocated
#rorg fontstart
#incbin "font.multi"
codestart:
  playerinit
;*** clear the buffers
  ldx #8
  tya
bloop$:
bufhi$ = . + 2
  sta buff1,y
  dey
  bne bloop$
  inc bufhi$
  dex
  bne bloop$

  sta buffer	; set the buffer addresses
  lda #>buff2
  sta buffer+1

  ; set up the raster interrupt
  screensync (SCR_Y + fHeight - 11) / 2
  lda #FIRST_COLUMN + (LAST_COLUMN - FIRST_COLUMN - fWidth * 2) / 2
  sta $9000	; screen origin x
  lda #SCR_Y / 2
  sta $9001	; screen origin y
  lda #19
  sta $9003	; screen height
  lda #((screen >> 6) & $f0) | (buff1 >> 10) | $88
  sta $9005	; graphics buffer pointers

  lda #<irq
  sta $314	; set the IRQ vector
  lda #>irq
  sta $315

  lda #<nmi
  sta $318	; set the NMI vector
  lda #>nmi
  sta $319

  lda #$40
  sta $912b	; enable Timer A free run on VIA 1
  sta $911b	; and on VIA 2

; a small delay to get the interrupt at the right place
#if SYSTEM==PAL
  ldy #5
  dey
  bne .-1
#else
  ldx #9
  dex
  bne .-1
#endif

  ldx #<TIMER_VALUE
  ldy #>TIMER_VALUE
  stx $9126	; load the timer low byte latch
  sty $9125	; start Timer A on VIA 1 (IRQ)

  lda #(SCR_Y + fHeight + 17) / 2
wait$:
  cmp $9004
  bne wait$

  stx $9116	; load the timer low byte latch
  sty $9115	; start Timer A on VIA 2 (NMI)

  lda #$c0
  sta $913e	; enable the timer interrupts, IRQ and NMI.

wait2$:
  lda $9004
  bne wait2$
  lda #fWidth | ((screen >> 2) & $80)
  sta $9002	; screen width
  lda #cBg1
  sta $900f	; background colour

  cli
  jsr loader	; load next part
  bcs .
  lda #$4c
  sta exit	; quit the effect
waitexit:
  jmp waitexit	; wait for the effect to complete
  jmp (nextpart); jump to next part

irq:
  irqsync

orgx = . + 1
  lda #FIRST_COLUMN
  sta $9000	; screen origin x
  lda #twidth | ((screen >> 2) & $80)
  sta $9002	; screen width
  lda #3
  sta $9003	; screen height
grbuf = . + 1
  lda #((screen >> 6) & $f0) | (buff1 >> 10) | $88
  sta $9005	; graphics buffer pointers
  lda #cBg2
  sta $900f
  lda $9114	; acknowledge the interrupt
  cli
jsrcall:
  jsr vector	; calculate next frame
  jmp $eb18	; return to main program

nmi:
  pha
  txa
  pha
  tya
  pha
  lda #FIRST_COLUMN + (LAST_COLUMN - FIRST_COLUMN - fWidth * 2) / 2
  sta $9000	; screen origin x
  lda #fWidth | ((screen >> 2) & $80)
  sta $9002	; screen width
  lda #19
  sta $9003	; screen height
dispbuf = . + 1
  lda #((screen >> 6) & $f0) | (buff1 >> 10) | $88
  sta $9005	; graphics buffer pointers

  ; hardware scroll, 1st level: switch fonts
  lda grbuf
  eor #(buff1 ^ buff2) >> 10
  sta grbuf
  eor #((screen >> 6) & $f0) | (buff1 >> 10) | $88
  bne noscroll$
  ; hardware scroll, 2nd level: toggle the screen position
  lda orgx
  eor #1
  sta orgx
  eor #FIRST_COLUMN
  bne noscroll$

; do the software scrolling
  ; 1st level software scrolling: scroll the text matrix
  ldx #0
  ldy scroll
ss1$:
  lda scroll+1,x
  sta scroll,x
  inx
  cpx #twidth+1
  bcc ss1$
  sty scroll+twidth+1

ssflag$ = . + 1
  lda #0
  eor #1
  sta ssflag$
  bne doscroll$

noscroll$:
  jmp domusic	; do music and return to main program

doscroll$:
  ; 2nd level software scrolling: write the fonts
  ; calculate a pointer to the graphics data of the rightmost characters
  lda scroll+twidth
  asl
  asl
  asl
  asl
  sta gr
  lda #(>(buff1+$200)) / 2
  rol
  sta gr+1
  lda scroll+twidth+1
  asl
  asl
  asl
  asl
  sta gr1
  lda #(>(buff1+$200)) / 2
  rol
  sta gr1+1
  lda scroll+twidth-1
  asl
  asl
  asl
  asl
  sta gr2
  lda #(>(buff2+$200)) / 2
  rol
  sta gr2+1

  lda #0
  sta font
scrollsrc$ = . + 1
  lda text	; read the scroll text
  lsr		; and calculate the font address for the character
  ror font
  lsr
  ror font
  lsr
  ror font
  lsr
  ror font
  ora #>fontstart
  sta font+1
  sta font2+1
  sta font3+1
  sta font4+1
  sta font5+1
  sta font6+1
  lda font
  sta font2
  sta font3
  sta font4
  sta font5
  sta font6

; now blit the char to the first character set
  ldy #15
  ldx #16
blit0$:
  dex
font = . + 1
  lda fontstart,x
  sta (gr1),y
  dey
  sta (gr1),y
  dey
  bpl blit0$
  ldy #15
blit1$:
  dex
font2 = . + 1
  lda fontstart,x
  sta (gr),y
  dey
  sta (gr),y
  dey
  bpl blit1$

; clc
  lda gr+1
  adc #>(buff2 - buff1)
  sta gr+1
; clc
  lda gr1+1
  adc #>(buff2 - buff1)
  sta gr1+1

; then blit to the second character set, scrolling it to the left
  ldx #16
  ldy #15
blit2$:
  dex
font3 = . + 1
  lda fontstart,x
  asl
  asl
  sta (gr1),y
  dey
  sta (gr1),y
  dey
  bpl blit2$
  ldy #15
blit3$:
  dex
font4 = . + 1
  lda fontstart,x
  asl
  asl
  sta (gr),y
  dey
; sta (gr),y	; not needed at this point
  dey
  bpl blit3$
  ldy #15
  ldx #16
blit4$:
  dex
font5 = . + 1
  lda fontstart,x
  asl
  rol
  rol
  and #3
  ora (gr),y
  sta (gr),y
  dey
  sta (gr),y
  dey
  bpl blit4$
  ldy #15
blit5$:
  dex
font6 = . + 1
  lda fontstart,x
  asl
  rol
  rol
  and #3
  ora (gr2),y
  sta (gr2),y
  dey
  sta (gr2),y
  dey
  bpl blit5$

  sec
  lda #0
  adc scrollsrc$	; update the scroll text pointer
  sta scrollsrc$
  bcc noupdhi$
  inc scrollsrc$ + 1
noupdhi$:
  tay
  lda scrollsrc$ + 1
exit = .
  ldx exit$	; placeholder for self-modifying code (quitting the effect)
  cpy #<textend2	; check for wraparound
  sbc #>textend2
  bcc notyet$

  lda #<text		; restart text from beginning
  sta scrollsrc$
  lda #>text
  sta scrollsrc$ + 1
notyet$:
  jmp domusic		; do music and return to main program

exit$:
  tax
  cpy #<textend
  sbc #>textend
  bcc notyet$

  ; The scroller is about to finish, start fading out the music

  lda volfadespd
  bne fading$
  lda #(LAST_COLUMN - FIRST_COLUMN) * 2 / 8 ; the volume setting is 8
	; fade out the volume in this many frames between volume changes
  sta volfademode
  sta volfadespd
  jmp domusic		; do music and return to main program
fading$:
  txa
  cpy #<textend2
  sbc #>textend2
  bcc notyet$

  ; The scroller has finished.  Exit the effect.

  lda #$7f
  sta $913e	; disable and
  sta $913d	; acknowledge the interrupts
  lda #0
  sta $9002	; blank screen (zero screen width)
  lda #$ad
  sta waitexit	; notify the main program
  jmp $eb18	; return to main program

domusic:	; do the music and return to main program
  jsr play
  lda $9124	;acknowledge the interrupt
  lda #cBg1
  sta $900f
  jmp $eb18	;return to main program (pla:tay:pla:tax:pla:rti)

  player	;include the player code and the music data here
  include "mus.anders"

vector:		; calculate one frame
  lda #$ad
  sta jsrcall	; disable re-entrant calls

;*** update angles
  clc
  lda sx
  adc dsx
#if angmax == 256
  clc
#else
#if angmax == 128 || angmax == 64 || angmax == 32 || angmax == 16
  and #angmax-1
  clc
#else
  cmp #angmax ;are we >= maximum angle?
  bcc cont1$
  sbc #angmax ;if so, reset
  clc
cont1$:
#endif	; angmax not a power of 2
#endif	; angmax == 256
  sta sx
  lda sy
  adc dsy
#if angmax == 256
  clc
#else
#if angmax == 128 || angmax == 64 || angmax == 32 || angmax == 16
  and #angmax-1
  clc
#else
  cmp #angmax
  bcc cont2$
  sbc #angmax ;same deal
  clc
cont2$:
#endif	; angmax not a power of 2
#endif	; angmax == 256
  sta sy
  lda sz
  adc dsz
#if angmax != 256
#if angmax == 128 || angmax == 64 || angmax == 32 || angmax == 16
  and #angmax-1
#else
  cmp #angmax
  bcc cont3$
  sbc #angmax
cont3$:
#endif	; angmax not a power of 2
#endif	; angmax != 256
  sta sz

;*** rotate coordinates

rotate:

;** first, calculate t1,t2,...,t10

;* two macros to simplify our life
#mac adda  ;add two angles together
  clc
  lda {1}
  adc {2}
#if angmax != 256
#if angmax == 128 || angmax == 64 || angmax == 32 || angmax == 16
  and #angmax-1
#else
  cmp #angmax ;is the sum > 2*pi?
  bcc done$
  sbc #angmax ;if so, subtract 2*pi
done$
#endif
#endif
#endm

#mac suba  ;subtract two angles
  sec
  lda {1}
  sbc {2}
#if angmax != 256
  bcs done$
  adc #angmax ;oops, we need to add 2*pi
done$
#endif
#endm

;* now calculate t1,t2,etc.

  suba sy,sz
  sta t1 ;t1=sy-sz
  adda sy,sz
  sta t2 ;t2=sy+sz
  adda sx,sz
  sta t3 ;t3=sx+sz
  suba sx,sz
  sta t4 ;t4=sx-sz
  adda sx,t2
  sta t5 ;t5=sx+t2
  suba sx,t1
  sta t6 ;t6=sx-t1
  adda sx,t1
  sta t7 ;t7=sx+t1
  suba t2,sx
  sta t8 ;t8=t2-sx
  suba sy,sx
  sta t9 ;t9=sy-sx
  adda sx,sy
  sta t10 ;t10=sx+sy

; et voila!

;** next, calculate a,b,c,...,i

;* another useful little macro
#mac asr ;divide a signed number contained in .A by 2
  cmp #$80
  ror
#endm

;* note that we are currently making a minor leap
;* of faith that no overflows will occur.

; calculate r11 = (cos(t1) + cos(t2)) / 2
  clc
  ldx t1
  lda cos,x
  ldx t2
  adc cos,x
  sta r11
; calculate r12 = (sin(t1) - sin(t2)) / 2
  ldx t1
  lda sin,x
  sec
  ldx t2
  sbc sin,x
  sta r12
; calculate r13 = sin(sy)
  ldx sy
  lda sin,x
  asl
  sta r13
; calculate r21 = (sin(t3)-sin(t4))/2 + (cos(t8)-cos(t7)+cos(t6)-cos(t5))/4
  clc
  ldx t5
  lda cos,x
  ldx t7
  adc cos,x
  asr
  sta r21
  clc
  ldx t8
  lda cos,x
  ldx t6
  adc cos,x
  asr
  sec
  sbc r21
  clc
  ldx t3
  adc sin,x
  sec
  ldx t4
  sbc sin,x
  sta r21
; calculate r22 = (cos(t3)+cos(t4))/2 + (sin(t5)-sin(t6)-sin(t7)-sin(t8))/4
  clc
  ldx t7
  lda sin,x
  ldx t8
  adc sin,x
  asr
  sta r22
  sec
  ldx t5
  lda sin,x
  ldx t6
  sbc sin,x
  asr
  sec
  sbc r22
  clc
  ldx t3
  adc cos,x
  clc
  ldx t4
  adc cos,x
  sta r22
; calculate r23 = (sin(t9)-sin(t10))/2
  ldx t9
  lda sin,x
  sec
  ldx t10
  sbc sin,x
  sta r23
; calculate r31 = (cos(t4)-cos(t3))/2 + (sin(t6)-sin(t8)-sin(t7)-sin(t5))/4
  ldx t7
  lda sin,x
  ldx t5
  adc sin,x
  asr
  sta r31
  ldx t6
  lda sin,x
  sec
  ldx t8
  sbc sin,x
  asr
  sec
  sbc r31
  clc
  ldx t4
  adc cos,x
  sec
  ldx t3
  sbc cos,x
  sta r31
; calculate r32=(sin(t3)+sin(t4))/2 + (cos(t6)+cos(t7)-cos(t5)-cos(t8))/4
  clc
  ldx t5
  lda cos,x
  ldx t8
  adc cos,x
  asr
  sta r32
  clc
  ldx t6
  lda cos,x
  ldx t7
  adc cos,x
  asr
  sec
  sbc r32
  clc
  ldx t3
  adc sin,x
  clc
  ldx t4
  adc sin,x
  sta r32
; calculate r33=(cos(t9)+cos(t10))/2
  clc
  ldx t9
  lda cos,x
  ldx t10
  adc cos,x
  sta r33

;*** clear buffer

bufhi = . + 1
  lda #>buff2
  sta cladrhi$
  ldx #2
  lda #0
  tay
clear$:
cladrhi$ = . + 2
  sta buff1,y
  iny
  bne clear$
  inc cladrhi$
  dex
  bne clear$

;*** next, read and draw polygons

  ldy #0
  sty index
objloop$:
  ldy index
  lda polylist,y;first, the number of points
  beq objdone$	;but if numpoints is zero then we are at the end of the list
  sta countpts
  inc index

; rotate, project and draw the polygon

doit$:
  jsr rotproj
  jmp objloop$

objdone$:
;*** swap buffers

  lda #(buff1 ^ buff2) >> 10
  eor dispbuf
  sta dispbuf	; swap display buffer
  lda #>(buff1 ^ buff2)
  eor bufhi
  sta bufhi	; and work buffer

  lda #$20
  sta jsrcall	; re-enable the subroutine call
  rts

;* Rotate, project, and store the points
;
; A set of points is read in, rotated and projected, and
; plotted into the drawing buffer (eor or normal).

#mac smult	;multiply two signed 8-bit numbers: a*y/64 -> a
  sta mult1$
  clc		;this multiply is for normal
  eor #$ff	;numbers, i.e. x=-64..64
  adc #$01
  sta mult2$
mult1$ = . + 1
  lda tmulnorm,y
  sec
mult2$ = . + 1
  sbc tmulnorm,y
#endm

#mac smultz	;multiply two signed 8-bit numbers: a*y/64 -> a
  sta mult1$
  clc		;and this multiply is specifically
  eor #$ff	;for the projection part, where
  adc #$01	;numbers are -110..110 and 0..40
  sta mult2$
mult1$ = . + 1
  lda tmulspec,y
  sec
mult2$ = . + 1
  sbc tmulspec,y
#endm

#mac rotate	;the actual rotation routine
;The routine takes the point {1} {2} {3}, rotates it,
;and stores the result in {1} {2} {3}.

  ldy {1} ;multiply first rotation column
  lda r11
  smult
  sta p1t
  lda r21
  smult
  sta p2t
  lda r31
  smult
  sta p3t
  ldy {2} ;second column
  lda r12
  smult
  clc
  adc p1t
  sta p1t
  lda r22
  smult
  clc
  adc p2t
  sta p2t
  lda r32
  smult
  clc
  adc p3t
  sta p3t
  ldy {3} ;third column
  lda r13
  smult
  clc
  adc p1t
  sta {1}
  lda r23
  smult
  clc
  adc p2t
  sta {2}
  lda r33
  smult
  clc
  adc p3t
  sta {3} ;rotated z
#endm

#mac project
  ldx {3}
  ldy zdiv,x ;table of f(z)=d/(z0-z/64)
  lda {1}
  smultz
  clc
  adc #32 ;offset the coordinate
  sta {1} ;rotated and projected

  ldy zdiv,x ;table of f(z)=d/(z0-z/64)
  lda {2}
  smultz
  clc
  adc #32 ;offset the coordinate
  sta {2} ;rotated and projected y
#endm ;all done

rotproj:
t$ = x1
  ldy index
  lda polylist,y
  sta p1x
  iny
  lda polylist,y
  sta p1y
  iny
  lda polylist,y
  sta p1z
  iny
  dec countpts
  lda polylist,y
  sta p2x
  iny
  lda polylist,y
  sta p2y
  iny
  lda polylist,y
  sta p2z
  iny
  dec countpts
  lda polylist,y
  sta p3x
  iny
  lda polylist,y
  sta p3y
  iny
  lda polylist,y
  sta p3z
  iny
  sty index
  rotate p1x,p1y,p1z
  rotate p2x,p2y,p2z
  rotate p3x,p3y,p3z

#if HIDDEN_FACES
  lda p2x ;hidden face check
  sec
  sbc p1x
  tay  ;y=(x2-x1)
  lda p3y
  sec
  sbc p2y ;a=(y3-y2)
  smult
  sta t$
  lda p3x
  sec
  sbc p2x
  tay
  lda p2y
  sec
  sbc p1y
  smult
  cmp t$
  bmi doit$	;if x1*y2 > y1*x2 then face is visible

  dec countpts	;hidden face: read in remaining points and return
  lda countpts
  asl
  adc countpts
  adc index
  sta index
  rts

doit$:
#endif
  project p1x,p1y,p1z
  project p2x,p2y,p2z
  project p3x,p3y,p3z

  lda p1x
  sta x1
  lda p1y
  sta y1
  lda p2x
  sta x2
  lda p2y
  sta y2
  jsr draw
  lda p2x
  sta x1
  lda p2y
  sta y1
  lda p3x
  sta x2
  lda p3y
  sta y2
  jsr draw

  dec countpts
  bne polyloop ;is it just a triangle?
  jmp polydone

polyloop:
  ldy index
  lda polylist,y
  sta p2x
  iny
  lda polylist,y
  sta p2y
  iny
  lda polylist,y
  sta p2z
  iny
  sty index
  rotate p2x,p2y,p2z
  project p2x,p2y,p2z

  lda p2x
  sta x1
  lda p2y
  sta y1
  lda p3x
  sta x2
  lda p3y
  sta y2
  jsr draw

  lda p2x
  sta p3x
  lda p2y
  sta p3y
  dec countpts
  beq polydone
  jmp polyloop

polydone:
  lda p1x ;close the polygon
  sta x2
  lda p1y
  sta y2
  lda p3x
  sta x1
  lda p3y
  sta y1
  jsr draw
  rts

;-------------------------------
; drawin' a line.  a fahn lahn.

;**** macro to take a step in x

#mac xstep
  ldx dx ;number of loop iterations
  txa
  lsr
xloop$:
  lsr chunk
  beq fixc$ ;update column
  sbc dy
  bcc fixy$ ;time to step in y
  dex
  bne xloop$
done$:
  lda oldx ;plot the last chunk
  eor chunk
  ora (buffer),y
  sta (buffer),y
  rts

fixc$:
  pha
  lda oldx
  ora (buffer),y	;plot
  sta (buffer),y
  lda #$ff		;update chunk
  sta oldx
  sta chunk
  lda #$3f
; sec
  adc buffer		;increment the column
  sta buffer
  bcc c2$
  inc buffer+1
c2$:
  sec
  pla
  sbc dy
  bcs cont$
  adc dx
#if {1} ;do we use iny or dey?
  iny
#else
  dey
#endif
cont$:
  dex
  bne xloop$
  jmp done$

fixy$:
  adc dx
  pha
  lda oldx
  eor chunk
  ora (buffer),y
  sta (buffer),y
  lda chunk
  sta oldx
  pla
#if {1} ;update y
  iny
#else
  dey
#endif
  dex
  bne xloop$
  rts
#endm ; xstep

;**** take a step in y

#mac ystep
  ldx dy ;number of loop iterations
  beq done$ ;if dy=0 it's just a point
  txa
  lsr
yloops$:
  sec
yloop$:
  pha
  lda oldx
  ora (buffer),y
  sta (buffer),y
  pla
#if {1}
  iny
#else
  dey
#endif
  sbc dx
  bcc fixx$
  dex
  bne yloop$
done$:
  lda oldx
  ora (buffer),y
  sta (buffer),y
  rts

fixx$:
  adc dy
  lsr oldx
  beq fixc$
  dex
  bne yloops$
  jmp done$

fixc$:
  pha
  lda #$80
  sta oldx
  lda #$3f
; sec
  adc buffer		;increment the column
  sta buffer
  bcc c2$
  inc buffer+1
c2$:
  pla
  dex
  bne yloops$
  jmp done$
#endm ; ystep

;*** initial line setup

;--------------------------------------------------------------------------
; bit table

bitp
  repeat 16
    dc.b %11111111,%01111111,%00111111,%00011111
    dc.b %00001111,%00000111,%00000011,%00000001
  repend
  pagecheck bitp

draw:
  sec  ;make sure x1<x2
  lda x2
  sbc x1
  bcs cont$
  lda y2 ;if not, swap p1 and p2
  ldy y1
  sta y1
  sty y2
  lda x1
  ldy x2
  sty x1
  sta x2

  sec
  sbc x1 ;now a=dx
cont$:
  sta dx
  ldx x1 ;put x1 into x, now we can trash x1

;find the first column for x
  lda #0
  sta buffer
  txa
  lsr
  lsr  ;there are x1/8 128 byte blocks
  lsr  ;which means x1/16 256 byte blocks
  lsr
  ror buffer
  lsr
  ror buffer
  adc bufhi
  sta buffer+1

  sec
  lda y2 ;calculate |dy|
  sbc y1
  bcs cont2$ ;is y2>y1?
  eor #$ff ;otherwise dy=y1-y2
  adc #1
cont2$:
  sta dy
  cmp dx ;who's bigger: dy or dx?
  bcc stepinx$ ;if dx, then...
  jmp stepiny$

stepinx$:
  ldy y1
  cpy y2
  lda bitp,x ;x currently contains x1
  sta oldx
  sta chunk
  bcc xincy$ ;do we step forwards or backwards in y?
  jmp xdecy$

xincy$:
  xstep 1 ;step with iny, exit with rts
xdecy$:
  xstep 0 ;step with dey, exit with rts

stepiny$:
  ldy y1
  lda bitp,x ;x=x1
  sta oldx
  lsr  ;y doesn't use chunks
  eor oldx ;so we just want the bit
  sta oldx
  cpy y2
  bcs ydecy$

  ystep 1 ;step with iny, exit with rts
ydecy$:
  ystep 0 ;step with dey, exit with rts

;-------------------------------
; set up the tables

;--------------------------------------------------------------------------
; sin and cos tables:
; f(x)=32*sin(2*pi*x/angmax)
; 1.25*angmax entries
sin:	dc.b 0,1,3,4,6,7,9,10,12,13,15,16,17,19,20,21
	dc.b 22,23,24,25,26,27,28,28,29,30,30,31,31,31,31,31
cos:	dc.b 32,31,31,31,31,31,30,30,29,28,28,27,26,25,24,23
	dc.b 22,21,20,19,17,16,15,13,12,10,9,7,6,4,3,1
	dc.b 0,-1,-3,-4,-6,-7,-9,-10,-12,-13,-15,-16,-17,-19,-20,-21
	dc.b -22,-23,-24,-25,-26,-27,-28,-28,-29,-30,-30,-31,-31,-31,-31,-31
	dc.b -32,-31,-31,-31,-31,-31,-30,-30,-29,-28,-28,-27,-26,-25,-24,-23
	dc.b -22,-21,-20,-19,-17,-16,-15,-13,-12,-10,-9,-7,-6,-4,-3,-1
#pagecheck sin
	dc.b 0,1,3,4,6,7,9,10,12,13,15,16,17,19,20,21
	dc.b 22,23,24,25,26,27,28,28,29,30,30,31,31,31,31,31
#pagecheck cos

#if . - sin != 5 * angmax / 4
  echo "sinus table size mismatch"
  err
#endif

polylist:
;--------------------------------------------------------------------------
; the object: a space ship
 
; bottom plates
	dc.b 4,-15,30,-12,-15,-30,-12,-57,-30,-3,-51,-6,-3
	dc.b 4,15,30,-12,51,-6,-3,57,-30,-3,15,-30,-12
	dc.b 4,15,30,-12,15,-30,-12,-15,-30,-12,-15,30,-12
; back plate
	dc.b 7,15,-30,-12,57,-30,-3,33,-30,6,0,-30,12,-33,-30,6,-57,-30,-3
	dc.b -15,-30,-12
; small engines (invisible when hidden lines enabled)
;	dc.b 3,33,-30,0,33,-30,-4,39,-30,-2
;	dc.b 3,-33,-30,0,-39,-30,-2,-33,-30,-4
; bigger engines
	dc.b 4,6,-30,3,6,-30,-6,18,-30,-4,18,-30,1
	dc.b 4,-6,-30,3,-18,-30,1,-18,-30,-4,-6,-30,-6
; right wingtip
	dc.b 3,-57,-30,-3,-33,-30,6,-51,-12,-3
; right wing
	dc.b 3,-33,-30,6,-15,30,-12,-51,-12,-3
; right side of cabin
	dc.b 3,-15,30,-12,-33,-30,6,0,6,12
; right back plate
	dc.b 3,0,6,12,-33,-30,6,0,-30,12
; front plate
	dc.b 3,15,30,-12,-15,30,-12,0,6,12
; left back plate
	dc.b 3,0,-30,12,33,-30,6,0,6,12
; left side of cabin
	dc.b 3,0,6,12,33,-30,6,15,30,-12
; left wing
	dc.b 3,15,30,-12,33,-30,6,51,-12,-3
; left wingtip
	dc.b 3,33,-30,6,57,-30,-3,51,-12,-3
; end of object
	dc.b 0

;--------------------------------------------------------------------------
; division table

#align 256	;the division table must not be page aligned,
		;but it's faster so

zdiv:	; table of f(z)=d/(z0-z/64)
d = 170
z0 = 5

z set 0
#repeat 256
q set (1 + 2 * 64 * d / (70 * z0 - z)) / 2
; this was scaled a bit  ^^ (was originally 64)
q set (q < 127 ? q - 127) + 127
q set (q > -127 ? q + 127) - 127
	dc.b q
z set (z == 127 ? 0-128 - z - 1) + z + 1
#repend

; multiplication tables

tmulspec:	; table of f(x)=x*x/256 normalized for -107<x<150
z set 0
#repeat 256
x set (z > 150 ? 256-z-z) + z
	dc.b (2*x*x/256+1)/2
z set z + 1
#repend
  assert_page tmulspec
		; another copy of the table
z set 0
#repeat 256
x set (z > 150 ? 256-z-z) + z
	dc.b (2*x*x/256+1)/2
z set z + 1
#repend

tmulnorm:	; table of f(x)=x*x/256 normalized for -128<x<128
z set 0
#repeat 256
x set (z > 127 ? 256-z-z) + z
	dc.b (2*x*x/256+1)/2
z set z + 1
#repend
  assert_page tmulnorm
		; another copy of the table
z set 0
#repeat 256
x set (z > 127 ? 256-z-z) + z
	dc.b (2*x*x/256+1)/2
z set z + 1
#repend

text:
; The scrolltext. The following ASCII text is converted to char codes:
;;;
; VECTORS! THE ROUTINES AND THIS OBJECT WERE TAKEN FROM STEPHEN JUDD'S
; AND GEORGE TAYLOR'S ARTICLE IN THE C=HACKING NET MAGAZINE ISSUE
; #10. CONVERSION, OPTIMIZATION AND OTHER CODE BY MARKO MÄKELÄ(f).
; CHARACTER SET AND MUSIC BY ANDERS CARLSSON(s). MUSIC ROUTINE BY
; ASGER ALSTRUP. WOULD YOU BELIEVE THAT THIS IS POSSIBLE ON THE
; VIC-20, WITH A 1 MHZ 6502, WITHOUT SUPPORT FOR RASTER INTERRUPTS,
; GRAPHICS OR SCROLLING AND WITH 13.5K OF RAM? NEXT YOU'LL SEE ANOTHER
; POPULAR EFFECT ON THIS LITTLE SUPERCOMPUTER...
;;;
; Conversion filter:
;; cut -c3-|tr @A-ZÄÖÅfsÆØÜÉ\\012 '\0-\037\044-\046\042 ' \
;; | hexdump -e '"  hex " 32/1 "%02x" "\n"'
  hex 160503140f1213212014080520120f1514090e051320010e042014080913200f
  hex 020a05031420170512052014010b050e2006120f0d201314051008050e200a15
  hex 0404271320010e042007050f120705201401190c0f1227132001121409030c05
  hex 20090e2014080520033d0801030b090e07200e0514200d0107011a090e052009
  hex 13131505202331302e20030f0e16051213090f0e2c200f1014090d091a011409
  hex 0f0e20010e04200f1408051220030f0405200219200d01120b0f200d1b0b050c
  hex 1b281e292e200308011201031405122013051420010e04200d15130903200219
  hex 20010e04051213200301120c13130f0e281f292e200d1513090320120f151409
  hex 0e0520021920011307051220010c13141215102e20170f150c0420190f152002
  hex 050c090516052014080114201408091320091320100f131309020c05200f0e20
  hex 140805201609032d32302c201709140820012031200d081a20363530322c2017
  hex 0914080f151420131510100f121420060f122012011314051220090e14051212
  hex 151014132c200712011008090313200f12201303120f0c0c090e0720010e0420
  hex 170914082031332e350b200f062012010d3f200e05181420190f15270c0c2013
  hex 050520010e0f1408051220100f10150c011220050606050314200f0e20140809
  hex 13200c0914140c05201315100512030f0d10151405122e2e2e
textend:
; blank padding at end of text (I am lazy)
#repeat twidth/2+2
  dc.b " "
#repend
textend2:

#if . > plasma
  echo "Vector part is", . - plasma, "bytes too long!"
  err
#endif
endcode:
#rend
relend:
