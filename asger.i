;Sound routine for the Vic-20

;© Copyright 1996, Asger Alstrup
;Modified by Marko Mäkelä and Anders Carlsson

;Copyleft by the GPL. See COPYING for details.

;Objective: Provide good sound with low worst-case time.


;Bresenham's algorithm for lines:

;void bresenham(x1, x2, frames) {
;  dx = x2-x1;
;  udx = abs(dx);
;  sdx = udx/dx;
;  y=0;
;  a=(udx+frames)/2;

;  do 
;   print x1,y
;   y++
;   a = a - udx
;   if (a<0) {
;     a += frames
;     x1+= sdx
;   }
;  while y=<frames;

;Memory constants
;----------------

sndbase = $900a
volumereg = $900e

zp ds.w 1

;Control codes
;-------------

silencecode = 39
bassdrumcode = 36
highhatcode = 38
snaredrumcode = 37

slidecode = 59
setvolcode = 60		;volume
volfadecode = 61	;delay for decrease in frames
instrcode = 62		;instrument #
gotocode = 63		;absolute address

;Constants
;---------

slidemask = $80
vibmask = $40

;Macros
;------

;Prepate note from notenumber
#mac prepnote
  dc.b {1},{2}
#endm

#mac c ;octave duration
  prepnote {1}*12,{2}
#endm

#mac c# 
  prepnote 1+{1}*12,{2}
#endm

#mac d 
  prepnote 2+{1}*12,{2}
#endm

#mac d#
  prepnote 3+{1}*12,{2}
#endm

#mac e 
  prepnote 4+{1}*12,{2}
#endm

#mac f 
  prepnote 5+{1}*12,{2}
#endm

#mac f#
  prepnote 6+{1}*12,{2}
#endm

#mac g 
  prepnote 7+{1}*12,{2}
#endm

#mac g#
  prepnote 8+{1}*12,{2}
#endm

#mac a 
  prepnote 9+{1}*12,{2}
#endm

#mac a#
  prepnote 10+{1}*12,{2}
#endm

#mac h 
  prepnote 11+{1}*12,{2}
#endm

;VSD (very simple drums) emulation
#mac bd ;duration in frames
  dc.b bassdrumcode,{1}
#endm

#mac hh ;duration in frames
  dc.b highhatcode,{1}
#endm

#mac sd ;duration in frames
  dc.b snaredrumcode,{1}
#endm

;Silence
#mac s ;duration in frames
  dc.b silencecode,{1}
#endm

;Goto
#mac go ;address
  dc.b gotocode
  dc.w {1}
#endm

;Volume fade
#mac volfade ;delay in frames between each decrement of volume
  dc.b volfadecode,{1}
#endm

;Set volume (effective from next frame)
#mac volset ;volume
  dc.b setvolcode,{1}
#endm

;Change instrument (Must be followed by a real note!)
#mac inst ;instrument offset
  dc.b instrcode,{1}-instruments
#endm

;Slide from <1> to <2> in <3> frames
#mac slide
  dc.b slidecode
  {1} {2},{5}
  {3} {4},{5}
#endm

;Code
;----

#mac player
play:
  ;Volume and volume fade control:

  lda volfademode
  beq nofade$
  dec volfademode
  bne nofade$
  dec volume
  beq fadedone$

  lda volfadespd
  dc.b $2c	;Skip next instruction (bit absolute)

fadedone$:
  lda #0
  sta volfademode

nofade$
  lda volumereg
  and #$f0
  ora volume
  sta volumereg

  ;Process each voice:

  ldx #3
nextvoice$:
  jsr notectrl
  
  lda delay,x
  cmp vibdelay,x
  bcs novib$

  jsr vibration

novib$:

  jsr doslide

  lda vibcnt,x
  cmp #$80
  ror
  cmp #$80
  ror
  clc
  adc vicfreq,x
  sta sndbase,x		;And play note

  dex
  bpl nextvoice$

  rts
;Note control
;------------

notectrl:
  dec delay,x		;Are we done with the current note?
  bne nochg$

next$:  
  ldy positionl,x
  lda positionh,x
  sta zp+1

next2$:
  lda (zp),y
  iny
  bne l1$
  inc zp+1
  inc positionh,x
l1$
; and #63
  cmp #silencecode+1	;Is it a note or silence?
  bcc realnote$
  cmp #setvolcode
  beq processsetvol$
  cmp #volfadecode
  beq processfadevol$
  cmp #instrcode
  beq processinst$
  cmp #gotocode
  beq processgoto$
  cmp #slidecode
  beq processslide$

nochg$:
  rts

  ;Process new instrument
  ;----------------------

processinst$:
  lda (zp),y
  sta temp		;Instrument offset

  iny
  bne pi1$
  inc zp+1
  inc positionh,x
pi1$

  ;Set note
  lda (zp),y
  sta note,x
    
  iny
  bne pi2$
  inc zp+1
  inc positionh,x
pi2$
  lda (zp),y
  sta delay,x

  iny
  bne pi3$
  inc positionh,x
pi3$  
  tya
  sta positionl,x

  ldy temp
  lda delay,x
  jmp setinstr

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
  lda (zp),y
  sta volume

donext$:
  iny
  bne next2$
  inc zp+1
  inc positionh,x
  bne next2$

  ;Process goto command
  ;-------------------- 

processgoto$:
  lda (zp),y
  sta positionl,x
  iny
  bne l2$
  inc zp+1
l2$
  lda (zp),y
  sta positionh,x
  jmp next$  		;and read from there

  ;Process note
  ;------------

realnote$:
  sta note,x
  lda (zp),y
  sta delay,x

  lda slidey,x
  sta slideframes,x

  iny
  bne rn1$
  inc positionh,x
rn1$  
  tya
  sta positionl,x
  rts

  ;Process slide
  ;-------------

processslide$:
  lda (zp),y
  sta note,x
  sta temp

  iny
  bne psl1$
  inc zp+1
  inc positionh,x
psl1$
  lda (zp),y
  sta delay,x
  pha

  iny
  bne psl2$
  inc zp+1
  inc positionh,x
psl2$

  lda (zp),y

  iny
  bne psl3$
  inc positionh,x
psl3$

  iny
  bne psl4$
  inc positionh,x
psl4$

  pha
  tya
  sta positionl,x
  pla

  tay
  pla
  jmp initslide

;Set new instrument and next note
;--------------------------------

;Instrument offset in .y

;Length of note in .a

setinstr:
  sta comp$+1
  lda instruments,y
  beq novib$
comp$:
  lda #0
  sec  
  sbc instruments,y	;Vibdelay/off
  sta vibdelay,x

  ldy note,x
  jmp dovib

novib$:
  sta vibmode,x
  sta vibcnt,x
  lda #0
  sta vibdelay,x
  rts

;Initialize vibration
;--------------------

;Get note to vibrate in .y

dovib:
  ;Calculate vibend: (Next note - Note) / 2
  lda notes+1,y
  sec
  sbc notes,y
  sta vibend,x

  ;Calculate vibstart: -vibend
  eor #$ff	
; sec
  adc #0
  sta vibstart,x

  ;Start from middle
  lda #0
  sta vibcnt,x

  ;And go up
  lda #128
  sta vibmode,x		;This is done in main loop
  rts

;Process vibration
;-----------------

vibration:
  ldy vibmode,x
  beq novib$
  bmi vibup$

;vibdown$:
  lda vibcnt,x
  cmp vibstart,x
  bne stilldown$

;Going up:
  inc vibmode,x		;always to 128

;Fast will use "rts" here

stillup$:
  inc vibcnt,x
novib$:
  rts

vibup$:
  lda vibcnt,x
  cmp vibend,x
  bne stillup$

;Going down:
  dec vibmode,x		;always to 127

;Fast will use "rts" here

stilldown$:
  dec vibcnt,x
  rts


;Initialize slide
;----------------

;End note in .y, first note in temp, duration in .a

initslide:
  sta slideframes,x

  lda #1
  sta slideon,x

  lda notes,y
  ldy temp
  sec
  sbc notes,y
  pha
  lda notes,y
  sta vicfreq,x

  ldy #1
  pla
  beq isl2$ 
  bpl isl1$

  eor #$ff
  clc
  adc #1
  dey

isl2$:
  dey

isl1$:
  sta slideudx,x
  tya
  sta slidesdx,x

  lda #0
  sta slidey,x

  lda slideudx,x
  clc
  adc slideframes,x
  lsr
  sta slidea,x
  rts

;Process slide
;-------------

doslide:
  lda slideon,x
  beq dsl2$

  lda slidey,x 
  cmp slideframes,x
  beq dsl3$

  inc slidey,x
  lda slidea,x
  sec
  sbc slideudx,x
  sta slidea,x
  
  bpl dsl1$
  clc
  adc slideframes,x
  sta slidea,x

  lda vicfreq,x
  clc
  adc slidesdx,x
  sta vicfreq,x

dsl1$
  rts

;Stop sliding
dsl3$

  lda #0
  sta slideon,x

;No sliding:

dsl2$
  ldy note,x
  lda notes,y
  sta vicfreq,x
  rts

;Note table based on A is 440 Hz. Tones above e2 shouldn't be used

notes:
#if SYSTEM == PAL
  dc.b 129,136,143,149,155,161,166,171,176,180,185,189
  dc.b 192,196,199,202,205,208,211,213,216,218,220,222
  dc.b 224,225,227,229,230,231,232,234,235,236,237,238
  dc.b 255,224,254,64; BassDrum, Snare, HighHat, Silence
#else ; NTSC
  dc.b 133,140,146,152,158,163,169,173,178,182,186,190
  dc.b 194,197,201,204,207,209,212,214,217,219,221,223
  dc.b 224,226,228,229,231,232,233,235,236,237,238,239
  dc.b 255,224,254,64; BassDrum, Snare, HighHat, Silence
#endif
#endm

;Initialize music
;----------------

#mac playerinit
  ldx #3
loop$:
  lda startsl,x
  sta positionl,x
  lda startsh,x
  sta positionh,x

  lda #0
  sta vibmode,x
  sta note,x
  sta sndbase,x

  sta slidey,x
  sta slideframes,x
  sta slideon,x

  ldy #0
  lda #0
  jsr setinstr

  lda #1
  sta delay,x

  dex
  bpl loop$

  lda #8
  sta volume
  lda #0
  sta volfademode	;No fading
  sta zp
#endm

#mac musicdata
;Variables for notecontrol
positionl:
  dc.b 0,0,0,0
positionh:
  dc.b 0,0,0,0
delay:
  dc.b 0,0,0,0
note:
  dc.b 0,0,0,0

;instr:			;Current instrument
  dc.b 0,0,0,0

vicfreq:
  dc.b 0,0,0,0

;Variables for vibration:

vibstart:		;non-positive value for start offset for vibration
  dc.b 0,0,0,0
vibend:			;non-negative value for end offset for vibration
  dc.b 0,0,0,0
vibcnt:			;actual offset for vibration
  dc.b 0,0,0,0
vibmode:		;stop (0), down (127) or up (128) vibration
  dc.b 0,0,0,0
vibdelay:		;delay until vibration begins in frames
  dc.b 0,0,0,0		

;Variables for slide:

slideframes:
  dc.b 0,0,0,0
slideudx:
  dc.b 0,0,0,0
slidesdx:
  dc.b 0,0,0,0
slidea:
  dc.b 0,0,0,0
slidey:
  dc.b 0,0,0,0
slideon:
  dc.b 0,0,0,0
slidex2:
  dc.b 0,0,0,0
  

;Other variables:

temp:
  dc.b 0

volume:
  dc.b 0
volfademode:
  dc.b 0
volfadespd:
  dc.b 0
#endm
