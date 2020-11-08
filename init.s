CURTAIN_SPEED = 1	; screen blanking speed in lines per frame
cBg = $2a		; colour used to blank the screen

  processor 6502
  seg code

  include "global.i"
  include "timings.i"
  include "basic.i"

  basicline $1201, 1996 ; create the BASIC SYS line (>=8k expansion)

intro:
  ; determine if PAL or NTSC (what's the biggest raster line)
  sei
waitnz$		; wait for a nonzero line first
  lda $9004
  beq waitnz$
waitz$
  tax
  lda $9004
  bne waitz$
  cli
  cpx #(LINES - 1) / 2
  bne wrongsystem$
  jmp doit
wrongsystem$:
  lda #<sorry$
  ldy #>sorry$
  jmp $cb1e

sorry$:
  dc.b "THIS DEMO WAS COMPILED"
#if SYSTEM==PAL
  dc.b "FOR THE 6561-101",13
  dc.b "(71*312 CYCLES/FRAME, "
  dc.b "PAL-B)."
#else
  dc.b "FOR THE 6560-101",13
  dc.b "(65*261 CYCLES/FRAME, "
  dc.b "NTSC-M)."
#endif
  dc.b 13,13
  dc.b "GET THE VERSION FOR",13
  dc.b "YOUR SYSTEM FROM",13
  dc.b "HTTP://WWW.FUNET.FI/",13
  dc.b "PUB/CBM/VIC20/DEMOS/!",0

doit:
  ; initialize the stack pointer
  ldx #$ff
  txs
  ; install the fastloader
  jsr init

  ldx #rlength	; move the routines to the destination area
reloc$:
  lda routines-1,x
  dex
  sta therest,x
  bne reloc$

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
  ldy $9002	; determine screen start address
  lda $9005
  lsr
  cmp #$40
  ror
  and #%10011100
  eor #$80
  cpy #$80
  adc #1
  and #$fe
  sta copy1$
  ora #1
  sta copy2$

  ldx #0	; copy the screen memory
copy$:
copy1$ = . + 2
  lda $1e00,x
  sta $1e00,x
copy2$ = . + 2
  lda $1f00,x
  sta $1f00,x
  inx
  bne copy$

  cpy #$80
  bcs done$
copyc$:
  lda $9400,x
  sta $9600,x
  lda $9500,x
  sta $9700,x
  inx
  bne copyc$

done$:
  stx $900e	; disable sound output

  tya
  ora #$80
  sta orig9002	; store screen width
  tay
  eor #$80
  sta blank9002
  lda $9005
  ora #$f0
  sta $9005
  sty $9002

  lda #cBg & 7	; create a text page with the next part's colour
fill$:
  sta $9400,x
  sta $9500,x
  inx
  bne fill$

  lda #$c0
  sta $913e	; enable the timer interrupts, IRQ and NMI.

  jmp therest	; do the rest

routines:
#rorg $100
therest:
  jsr loader	; load the next part with the fast loader
  bcs .		; loop if a disk error occurred

waitexit:
  bne waitexit	; wait for the effect to complete
  jmp (nextpart)

curtains:	; IRQ routine (top of the curtain)
  lda #cBg	; set the background color of the next part
  sta $900f
blank9002 = . + 1
  lda #22	; placeholder for self-modifying code
  sta $9002	; blank the screen
  lda $9124	; acknowledge the interrupt
  jmp $eb18	; return to main program (pla:tay:pla:tax:pla:rti)

curtaine:	; NMI routine (bottom of the curtain)
  pha
  lda $9004	; check if we are at end of screen
  cmp #LINES / 2 - 1
  bcc unfinished$
  lda #$7f	; effect done, stop the interrupts
  sta $913e
  lda #$a9
  sta waitexit	; modify the main loop
  pla
  rti		; exit

unfinished$:
color = . + 1
  lda #0	; placeholder for self-modifying code
  sta $900f	; restore the screen color
orig9002 = . + 1
  lda #$80 + 22 ; placeholder for self-modifying code
  sta $9002	; unblank the screen
  lda $9114	; acknowledge the NMI
  pla
  rti
#rend
rlength = . - routines

; The synchronous 1540/1541/1570/1571 fast loader, computer's part

irqload:
#rorg loader

; I/O constants and the variables

iecport1 = $912c
dato = 32
clko = 2
iecport2 = $911f
atno = 128
clki = 1
dati = 2

; variable definition

  lda iecport1
  and #255 - dato - clko
  sta iec1d1a$	; CLK=1, DATA=1
  sta iec1d1b$
  eor #clko
  sta iec0d1a$	; CLK=0, DATA=1
  sta iec0d1b$

name$ = . + 1
  lda #"0"	; increment the file name's first character
  sec
  adc #0
  sta name$
  jsr putbyt$	; send the file name's first character
  lda #"-"
  jsr putbyt$	; send the file name's second character

  jsr getbyt$	; get the start address
  tay
  jsr getbyt$
  sta adrhi$

  sty nextpart	; store the address as a jump vector for the main program
  sta nextpart + 1

loadloop$:
  jsr getbyt$	; get next file byte, exit on completion
adrhi$ = . + 2
  sta $100,y	; store it
  iny
  bne loadloop$
  inc adrhi$
  jmp loadloop$

;---------------------------------------
; getbyt$: get a byte, interpret the escape codes

getbyt$:
  jsr getbits$
  cmp #ESCBYTE
  bne getdone$
  jsr getbits$	; escape char fetched, get another byte
  cmp #ESCBYTE	; another escape char: it is a literal
  beq getdone$
  cmp #1	; Transfer finished. 0=ok, nonzero=error.
  pla		; Set the C flag accordingly.
  pla		; discard the return address
getdone$:
  rts

; getbits$: get a byte

getbits$:
  ldx #8	; counter: get 8 bits
getbit$:
  lda iecport2
  and #dati | clki
  eor #dati | clki
  beq getbit$	; wait for CLK==low || DATA==low

#if dati == 128
  asl		; Carry = DATA==low
#else
#if dati < clki
  and #dati
#endif
  cmp #dati
#endif

iec0d1a$ = . + 1
  lda #255 - dato
  bcs gskip$
  eor #dato | clko
gskip$:
  sta iecport1	; acknowledge the bit
  ror store$	; store the data

  lda #dati | clki
wgetack$:
  bit iecport2
  beq wgetack$	; wait for CLK==high || DATA==high

iec1d1a$ = . + 1
  lda #255 - clko - dato
  sta iecport1	; raise CLK and DATA
  dex
  bne getbit$	; loop until all bits are received
store$ = . + 1
  lda #0
  rts

; putbyt$ puts a byte

putbyt$:
  sta store$
  ldx #8	; counter: send all 8 bits
putbit$:
  lsr store$	; read a bit
iec0d1b$ = . + 1
  lda #255 - dato
  bcc pskip$
  eor #dato | clko
pskip$:
  sta iecport1	; send the data

  lda #dati | clki
wputack1$:
  bit iecport2
  bne wputack1$	; wait for CLK==DATA==low

iec1d1b$ = . + 1
  lda #255 - clko - dato
  sta iecport1	; set DATA=CLK=high
wputack2$:
  lda iecport2
  and #dati | clki
  eor #dati | clki
  bne wputack2$	; wait for CLK==DATA==high
  dex
  bne putbit$	; loop until all bits are sent
  rts

loaderend:
#rend
loadersize = . - irqload

; Initializations and the disk drive's part of the fastloader.

; KERNAL definitions

secnd	= $ff93	; send secondary address for LISTEN
ciout	= $ffa8	; write serial data
unlsn	= $ffae	; send UNLISTEN command
listn	= $ffb1	; send LISTEN command
fa	= $ba	; Current Device Number

AMOUNT = $20	; amount of data bytes to transfer with one M-W command
ESCBYTE = $ac	; the escape char used in the transfers
RETRIES = 50	; amount of retries in reading a block

LEDFLASH = 3	; LED flashing level:
		; 0 = normal (LED constantly on while loading a file)
		; 1 = LED glows on and off while waiting for a command
		; 2 = LED on only while reading sectors
		; 3 = 1 + 2
;DRIVE = 1581	; 1581 or any other for 154x/157x-compatible

DEFAULT_DEVICE = 8	; Default device number

; the initialization code

init:
; transfer the fast loader to its execution address
  ldx #<loadersize
#if loadersize > 256
#echo "irq loader too long!"
#err
#endif
xferloop$:
  lda irqload - 1,x
  dex
  sta loader,x
  bne xferloop$

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
;  ldx #lmecmd$ - 1
;sendcmd$:
;  lda mecmd$,x
;  jsr ciout
;  dex
;  bpl sendcmd$
; perform the command
  lda #"U"
  jsr ciout
  jsr c1581wait
  lda #"3"
  jsr ciout
  jsr c1581wait
  jsr unlsn
  jmp c1581wait

; subroutine: make the current drive listen

inidev$:
  lda fa	; get the device number
  bne nodef$	; if not set, then use the default device number
  lda #DEFAULT_DEVICE
nodef$:
  sta fasave	; save the device number
  jsr listn
  lda #$6f
  jmp secnd

; the m-w command backwards

mwcmd$:
  dc.b AMOUNT,>drive,<drive,"W-M"
lmwcmd$ = . - mwcmd$

; the m-e command backwards

;mecmd$:
;  dc.b >drive,<drive,"E-M"
;lmecmd$ = . - mecmd$

c1581wait:
  ldy #50
  dey
  bne *-1
  rts

;---------------------------------------
; the drive code

drvcode:

#rorg $500

;---------------------------------------
; The fastload routine for the drive
;---------------------------------------
; The 1581-code and some optimization by Pasi Ojala, albert@cs.tut.fi

PARANOID = 1	; The 1581 docs say that track 40, 0 contains
		; a pointer to the first directory block.
		; 1=use it, 0=make a guess (track 40, sector 3)
;---------------------------------------
; FOR 1581
;---------------------------------------
;acsbf8	= $03 ;job for buffer 1 (not used directly)
trkbf8	= $0d ;track for job 1
sctbf8	= $0e ;sector for job 1

;ciapa	= $4000	; (not used directly)
ciapb	= $4001
ledon	= $cbcc	; activity led on: $cbcc, off: $cbc3
ledoff	= $cbc3	; or using the job queue:  on $94 / off $96

;---------------------------------------
; FOR 1540/41/70/71
;---------------------------------------
acsbf	= $01 ;access to buffer 1
trkbf	= $08 ;track of buffer 1
sctbf	= $09 ;sector of buffer 1
iddrv0	= $12 ;id of drive 0
id	= $16 ;id

via1pb	= $1800
via2pb	= $1c00

;---------------------------------------
; For both 154x/7x and 1581
;---------------------------------------
buf	= $0400	;sector buffer (for job 1)
datbf	= $14	;databuffer - temporary (on 1581 sector for job 4)


drive:
  cld
  lda $ff54
  and #$df
  eor #$4c	; jmp abs or jmp (abs)?
  beq drive1581
  jmp drive1571

drive1581:	; interrupts always enabled
#if !(LEDFLASH & 1)
  jsr ledoff
#endif

  jsr recv$	; get the file name, first char
  sta name1$+1
  jsr recv$	; get the file name, second char
  sta name2$+1
#if !(LEDFLASH & 2)
  jsr ledon
#endif

#if PARANOID
  ldx #40	; get the root block
  ldy #0
  jsr readsect$	; read the sector
  bcs errquit$	; quit if it could not be read

  ldx buf	; read the disk directory (track 40, sector 3 (usually))
  ldy buf+1
#else
  ldx #40
  ldy #3
#endif
dirloop$:
  jsr readsect$	; read the sector
  bcs errquit$	; quit if it could not be read

  ldy #2
nextfile$:
  lda buf,y	; check file type
  and #$87
  cmp #$82	; must be PRG
  bne notfound$

  lda buf+3,y	; check the first two characters
name1$:
  cmp #"1"
  bne notfound$

  lda buf+4,y
name2$:
  cmp #"-"
  beq found$

notfound$:
  tya
  clc
  adc #$20
  tay
  bcc nextfile$

  ldy buf+1	; get next sector
  ldx buf	; and track
  bne dirloop$	; keep trying until the last directory block has been searched

  ; file not found: fall through
errquit$:
  ldx #ESCBYTE	; send the escape byte followed by 1 to notify the computer
  jsr send$
  ldx #1
  jsr send$

  ; Error led flash on
  lda #$98
  ldx #1
  jsr $ff54	; execute command for queue 1, return value in A also
  jmp drive1581

found$:
  ldx buf+1,y	; get the track and sector numbers
  lda buf+2,y
  tay
nextsect$:
  jsr readsect$
  bcs errquit$	; quit if the sector could not be read
  ldy #255
  lda buf
  bne notlast$	; if the track is nonzero, this wasn't the last sector

  ldy buf+1	; last sector: get sector length
notlast$:
  sty numlast$+1

  ldy #1	; skip the track and sector when sending the buffer
sendbuf$:	; send the buffer contents to the computer
  ldx buf+1,y
  cpx #ESCBYTE
  bne noesc$

  jsr send$	; escape the escape character
  ldx #ESCBYTE
noesc$:
  jsr send$
  iny
numlast$:
  cpy #255	; were all bytes of the block sent?
  bne sendbuf$

  ldy buf+1	; the track and sector of next block
  ldx buf
  bne nextsect$	; loop until all sectors are loaded

finish$:
  ldx #ESCBYTE	; send the escape byte followed by 0 to notify the computer
  jsr send$
  ldx #0
  jsr send$
  jmp drive1581

;---------------------------------------
; readsect$: read a sector

readsect$:
  stx trkbf8
  sty sctbf8
#if LEDFLASH & 2
  jsr ledon
#endif
		; TODO: set head knock and retries
  lda #$80
  ldx #1
  jsr $ff54	; execute command $80 for queue 1, return value in A also
  cmp #2	; 0 and 1 -> (0-2 and 1-2) clears carry, errors set carry
#if LEDFLASH & 2
  jsr ledoff	; led off (does not affect carry)
#endif
  rts

; send$ sends the X register contents. datbf is used as temporary storage.

send$:
  stx datbf
  ldx #8	; send 8 bits
; sendbit$ sends a bit
sendbit$:
  lsr datbf	; read next bit
  lda #2	; prepare for CLK=high, DATA=low (was 2)
  bcs sskip$
  lda #8	; prepare for CLK=low, DATA=high (was 8)
sskip$:
  sta ciapb	; send the data

sgetack$:
  lda ciapb	; wait for CLK==DATA==low
  and #5
  eor #5
  bne sgetack$
  sta ciapb	; set CLK=DATA=high

  lda #5
swait$:
  bit ciapb
  bne swait$	; wait for CLK==DATA==high

  dex
  bne sendbit$	; loop until all bits have been sent
  rts

;---------------------------------------
; recv$ receives a byte to A. datbf is used as temporary storage.

recv$:
#if LEDFLASH & 1
  ldy #0	; LED brightness (0=dim, 255=lit)
  tsx
fincr$:
  jsr doflash$
  ldy datbf
  iny
  bne fincr$
fdecr$:
  dey
  jsr doflash$
  ldy datbf
  bne fdecr$
  beq fincr$

doflash$:
  sty datbf	; store the counter for LED flashing
  jsr ledoff
  jsr fdelay$	; perform the delay
  jsr ledon
  lda datbf
  eor #$ff
  tay		; fall through

fdelay$:
  lda #$85
  and ciapb	; wait for any signal from the bus
  bne flashdone$
  iny
  bne fdelay$
  rts

flashdone$:
  jsr ledoff
  txs		; discard the return address
#endif

  ldx #8	; counter: receive 8 bits
recvbit$:
  lda #$85
  and ciapb	; wait for CLK==low || DATA==low
  bmi gotatn$	; quit if ATN was asserted
  beq recvbit$
  lsr		; read the data bit
  lda #2	; prepare for CLK=high, DATA=low
  bcc rskip$
  lda #8	; prepare for CLK=low, DATA=high
rskip$:
  sta ciapb	; acknowledge the bit received
  ror datbf	; and store it
rwait$:
  lda ciapb	; wait for CLK==high || DATA==high
  and #5
  eor #5
  beq rwait$
  lda #0
  sta ciapb	; set CLK=DATA=high

  dex
  bne recvbit$	; loop until all bits have been received
  lda datbf	; read the data to A
  rts

gotatn$:
  pla		; If ATN gets asserted, exit to the operating system.
  pla		; Discard the return address.
  cli		; Enable the interrupts.
  rts

;edrvcode:


;---------------------------------------
; FOR 1540/41/70/71
;---------------------------------------


drive1571:
  cld
  cli		; interrupts enabled until first sector read
#if !(LEDFLASH & 1)
  lda via2pb
  and #$f7	; led off
  sta via2pb
#endif

  jsr recv$
  sta name1$+1	; get the file name, first char
  jsr recv$
  sta name2$+1	; second char
#if !(LEDFLASH & 2)
  lda #8
  ora via2pb
  sta via2pb	; led on
#endif

  ldx #18
  ldy #1	; read the disk directory (track 18, sector 1)
dirloop$:
  jsr readsect$	; read the sector
  bcs errquit$	; quit if it could not be read

  ldy #$02
nextfile$:
  lda buf,y	; check file type
  and #$83
  cmp #$82	; must be PRG
  bne notfound$

  lda buf+3,y	; check the first two characters
name1$:
  cmp #0
  bne notfound$

  lda buf+4,y
name2$:
  cmp #0
  beq found$

notfound$:
  tya
  clc
  adc #$20
  tay
  bcc nextfile$

  ldy buf+1	; get next sector
  ldx buf	; and track
  bne dirloop$	; keep trying until the last directory block has been searched

  ; file not found: fall through

errquit$:
  ldx #ESCBYTE	; send the escape byte followed by 1 to notify the computer
  jsr send$
  ldx #1
  jsr send$
  jmp drive1571

found$:
  ldx buf+1,y	; get the track and sector numbers
  lda buf+2,y
  tay

nextsect$:
  jsr readsect$
  bcs errquit$	; quit if the sector could not be read
  ldy #255
  lda buf
  bne notlast$	; if the track is nonzero, this wasn't the last sector

  ldy buf+1	; last sector: get sector length
notlast$:
  sty numlast$+1

  ldy #1	; skip the track and sector when sending the buffer
sendbuf$:	; send the buffer contents to the computer
  ldx buf+1,y
  cpx #ESCBYTE
  bne noesc$

  jsr send$	; escape the escape character
  ldx #ESCBYTE

noesc$:
  jsr send$
  iny
numlast$:
  cpy #255	; were all bytes of the block sent?
  bne sendbuf$

  ldy buf+1	; store the track and sector of next block
  ldx buf
  bne nextsect$	; loop until all sectors are loaded

finish$:
  ldx #ESCBYTE	; send the escape byte followed by 0 to notify the computer
  jsr send$
  ldx #0
  jsr send$
  jmp drive1571

;---------------------------------------
; readsect$: read a sector

readsect$:
  stx trkbf
  sty sctbf
#if LEDFLASH & 2
  lda #8
  ora via2pb
  sta via2pb	; turn the LED on
#endif
  ldy #RETRIES	; load the retry count
  cli		; enable interrupts, so that the command will be executed
retry$:
  lda #$80
  sta acsbf	; code for reading the sector
poll1$:
  lda acsbf	; wait for the command to complete
  bmi poll1$

  cmp #1
  bne noexit$
#if LEDFLASH & 2
  lda #$f7
  and via2pb
  sta via2pb	; turn the LED off
#endif
  clc
  sei		; disable interrupts again to make the program faster
  rts		; success: exit the loop

noexit$:
  dey		; decrement the retry count
  bmi error$	; quit if there were too many retries

  cpy #RETRIES / 2
  bne skipcode$

  lda #$c0
  sta acsbf	; half the retries left: knock the head (seek track 1)

skipcode$:
  lda id	; tolerate disk id changes
  sta iddrv0
  lda id+1
  sta iddrv0+1

poll2$:
  lda acsbf	; wait for the command to complete
  bmi poll2$
  bpl retry$	; branch always

error$:
#if LEDFLASH & 2
  lda #$f7
  and via2pb
  sta via2pb	; turn the LED off
#endif
  sec
  sei
  rts

; send$ sends the X register contents. datbf is used as temporary storage.

send$:
  stx datbf
  ldx #8	; send 8 bits
; sendbit$ sends a bit
sendbit$:
  lsr datbf	; read next bit
  lda #2	; prepare for CLK=high, DATA=low
  bcs sskip$
  lda #8	; prepare for CLK=low, DATA=high
sskip$:
  sta via1pb	; send the data

sgetack$:
  lda via1pb	; wait for CLK==DATA==low
  and #5
  eor #5
  bne sgetack$
  sta via1pb	; set CLK=DATA=high

  lda #5
swait$:
  bit via1pb
  bne swait$	; wait for CLK==DATA==high

  dex
  bne sendbit$	; loop until all bits have been sent
  rts

;---------------------------------------
; recv$ receives a byte to A. datbf is used as temporary storage.

recv$:
#if LEDFLASH & 1
  ldy #0	; LED brightness (0=dim, 255=lit)
  tsx
fincr$:
  jsr doflash$
  ldy datbf
  iny
  bne fincr$
fdecr$:
  dey
  jsr doflash$
  ldy datbf
  bne fdecr$
  beq fincr$

doflash$:
  sty datbf	; store the counter for LED flashing
  lda #$f7
  and via2pb
  sta via2pb	; turn the LED off
  jsr fdelay$	; perform the delay
  lda #8
  ora via2pb
  sta via2pb	; turn the LED on
  lda datbf
  eor #$ff
  tay		; fall through

fdelay$:
  lda #$85
  and via1pb	; wait for any signal from the bus
  bne flashdone$
  iny
  bne fdelay$
  rts

flashdone$:
  lda #$f7
  and via2pb
  sta via2pb	; turn the LED off
  txs		; discard the return address
#endif

  ldx #8	; counter: receive 8 bits
recvbit$:
  lda #$85
  and via1pb	; wait for CLK==low || DATA==low
  bmi gotatn$	; quit if ATN was asserted
  beq recvbit$
  lsr		; read the data bit
  lda #2	; prepare for CLK=high, DATA=low
  bcc rskip$
  lda #8	; prepare for CLK=low, DATA=high
rskip$:
  sta via1pb	; acknowledge the bit received
  ror datbf	; and store it
rwait$:
  lda via1pb	; wait for CLK==high || DATA==high
  and #5
  eor #5
  beq rwait$
  lda #0
  sta via1pb	; set CLK=DATA=high

  dex
  bne recvbit$	; loop until all bits have been received
  lda datbf	; read the data to A
  rts

gotatn$:
  pla		; If ATN gets asserted, exit to the operating system.
  pla		; Discard the return address.
  cli		; Enable the interrupts.
  rts

edrvcode:
;#endif

#rend
