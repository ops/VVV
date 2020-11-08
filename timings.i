TRUE = 1
FALSE = 0

NTSC	= 6560
PAL	= 6561

#if SYSTEM == PAL
FIRST_LINE = 28		; first visible raster line
FIRST_COLUMN = 5	; leftmost visible screen coordinate
LAST_COLUMN = 65	; leftmost right border coordinate that is not visible
LINES = 312
CYCLES_PER_LINE = 71
#endif
#if SYSTEM == NTSC
FIRST_LINE = 28		; first visible raster line (not 100% sure)
FIRST_COLUMN = 1	; leftmost visible screen coordinate (not 100% sure)
LAST_COLUMN = 55	; leftmost right border coordinate that is not visible
LINES = 261
CYCLES_PER_LINE = 65
#endif

VIA_RELOAD_TIME = 2	; reloading a VIA timer takes 2 bus clock cycles
TIMER_VALUE = LINES * CYCLES_PER_LINE - VIA_RELOAD_TIME

; Useful macros

#mac pagecheck
  if ((. - 1) >> 8) - >{1}
    echo "warning: area at",{1},"splits accross page boundaries, performance affected"
  endif
#endm

#mac wantsamepage
  if >. - >{1}
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm

#mac assert_page
  if >. - >{1} - (. - {1}) / 256
    echo "fatal: area is not page aligned"
    err
  endif
#endm

; screensync: wait for a raster line (exact synchronization)
; ----------
; Parameters:
;   {1} raster line number (plus 9 times 2), as read from $9004
;
; Registers affected:
;   .A .X .Y .P

#mac screensync
  lda #$7f
  sta $913e	; disable and acknowledge interrupts and NMIs
  sta $913d

  ldx #{1}
init$:		; ensure that we are not in the middle of the raster line
  cpx $9004
  beq init$

coarse$:
  cpx $9004
  bne coarse$	; at this stage, the inaccuracy is 7 clock cycles
		; the processor is in this place 2 to 9 cycles
		; after $9004 has changed
  ldy #9
  bit $24
fine$:
  ldx $9003
  txa
  bit $24
#if SYSTEM == PAL
  ldx #9
  nop
  nop
#endif
#if SYSTEM == NTSC
  bit $24
  ldx #8
#endif
delay$:
  dex
  bne delay$	; first spend some time (so that the whole
  if >. - >delay$
    echo "fatal: page boundary crossed in screensync at",.
    err
  endif

  cmp $9003	; loop will be 1 raster line)
  beq . + 2	; save one cycle if $9003 changed too late
  dey
  bne fine$
		; now it is fully synchronized
		; 6 cycles have passed since last $9004 change
#endm

; setirq: set the timer interrupt on specified line
; ------
; Parameters:
;   {1} raster line number (times 2 plus 9), as read from $9004
;   {2} horizontal raster position (5 cycles granularity)
;   {3} interrupt routine address
;   {4} flag: should the Restore key be enabled?
;
; Registers affected:
;   .A .X .Y .P

#mac setirq
;synchronize with the screen
  screensync {1}

;initialize the timers
  lda #$40	; enable Timer A free run on VIA 1
  sta $912b

  lda #<TIMER_VALUE
  ldx #>TIMER_VALUE
  sta $9126	; load the timer low byte latch

#if SYSTEM == PAL
  ldy #{2}+1	; make a little delay to get the raster effect to the
delay1$:	; right place
  dey
  bne delay1$
  if >. - >delay1$
    echo "fatal: page boundary crossed in setirq at",.
    err
  endif
  nop
  nop
#endif
#if SYSTEM == NTSC
  ldy #{2}+6
delay1$:
  dey
  bne delay1$
  if >. - >delay1$
    echo "fatal: page boundary crossed in setirq at",.
    err
  endif
#endif

  stx $9125	; start Timer A
		; NTSC: 25+5*{2} cycles from $9004 change
		; PAL:  37+5*{2} cycles from $9004 change

pointers:
  lda #<{3}	; set the raster IRQ routine pointer
  sta $314
  lda #>{3}
  sta $315
  lda #$c0
  sta $912e	; enable Timer A underflow interrupts
#if {4}
  lda #$82
  sta $911e	; enable Restore key
#endif
#endm

; irqsync: synchronize a raster interrupt
; -------
; Parameters:
;   none
;
; Registers affected:
;   .A .P

#mac irqsync
; irq (event)	; > 7 + at least 2 cycles of last instruction (9 to 16 total)
; pha		; 3
; txa		; 2
; pha		; 3
; tya		; 2
; pha		; 3
; tsx		; 2
; lda $0104,x	; 4
; and #xx	; 2
; beq 		; 3
; jmp ($314)	; 5
		; ---
		; 38 to 45 cycles delay at this stage
  lda $9124	; get the Timer A value
  sec		; (42 to 49 cycles delay at this stage)
#if 0 ; new delay function, idea by Asger Alstrup
  sbc #<TIMER_VALUE - 49 + VIA_RELOAD_TIME - 9  ; Here is a bug.
  eor #$ff
  sta skipcycles$
skipcycles$ = . + 1
  bpl .
  dc.b $a9,$a9,$a9,$a9,$a9,$a9,$a9,$24,$ea
  if >. - >(skipcycles$ + 1)
    echo "fatal: page boundary crossed in irqsync at",.
    err
  endif
  ; Time elapsed since IRQ: approx. 65 cycles (one less if on zero page)
#else
  sbc #<TIMER_VALUE - 49 + VIA_RELOAD_TIME
  cmp #8	; are we more than 7 cycles ahead of time?
  bcc save8$
  pha		; yes, spend 8 extra cycles
  pla
  and #7	; and reset the high bit
save8$:
  cmp #4
  bcc save4$
  bit $24	; waste 4 cycles
  and #3
save4$:
  cmp #2	; spend the rest of the cycles
  bcs . + 2
  bcs . + 2
  lsr
  bcs . + 2	; now it has taken a fixed amount of cycles from the
		; beginning of the IRQ
  ; Time elapsed since IRQ: approx. 78 cycles
#endif

#endm
