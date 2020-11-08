; The BASIC line

; basicline: generate a BASIC SYS line
; ---------
; Parameters:
;   {1} program origin
;   {2} line number
; Registers affected:
;   none

#mac basicline
  org {1}
  word 0$	; link to next line
  word {2}	; line number
  byte $9E	; SYS token

; SYS digits

  if (* + 8) / 10000
    byte $30 + (* + 8) / 10000
  endif
  if (* + 7) / 1000
    byte $30 + (* + 7) % 10000 / 1000
  endif
  if (* + 6) / 100
    byte $30 + (* + 6) % 1000 / 100
  endif
  if (* + 5) / 10
    byte $30 + (* + 5) % 100 / 10
  endif
  byte $30 + (* + 4) % 10
  byte 0	; end of BASIC line
0$:
  word 0	; end of BASIC program
#endm
