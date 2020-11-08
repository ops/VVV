;Control codes
;-------------

silencecode = 127

setvolcode = 126	;volume
volfadecode = 125	;delay for decrease in frames
gotocode = 124		;absolute address

firstcode = 124		;first code

;Macros
;------
#if SYSTEM==PAL
  mac c0
    dc.b 129
  endm
  mac c#0
    dc.b 136
  endm
  mac d0
    dc.b 143
  endm
  mac d#0
    dc.b 149
  endm
  mac e0
    dc.b 155
  endm
  mac f0
    dc.b 161
  endm
  mac f#0
    dc.b 166
  endm
  mac g0
    dc.b 171
  endm
  mac g#0
    dc.b 176
  endm
  mac a0
    dc.b 180
  endm
  mac a#0
    dc.b 185
  endm
  mac h0
    dc.b 189
  endm
  mac c1
    dc.b 192
  endm
  mac c#1
    dc.b 196
  endm
  mac d1
    dc.b 199
  endm
  mac d#1
    dc.b 202
  endm
  mac e1
    dc.b 205
  endm
  mac f1
    dc.b 208
  endm
  mac f#1
    dc.b 211
  endm
  mac g1
    dc.b 213
  endm
  mac g#1
    dc.b 216
  endm
  mac a1
    dc.b 218
  endm
  mac a#1
    dc.b 220
  endm
  mac h1
    dc.b 222
  endm
  mac c2
    dc.b 224
  endm
  mac c#2
    dc.b 225
  endm
  mac d2
    dc.b 227
  endm
  mac d#2
    dc.b 229
  endm
  mac e2
    dc.b 230
  endm
#else
  mac c0
    dc.b 133
  endm
  mac c#0
    dc.b 140
  endm
  mac d0
    dc.b 146
  endm
  mac d#0
    dc.b 152
  endm
  mac e0
    dc.b 158
  endm
  mac f0
    dc.b 163
  endm
  mac f#0
    dc.b 169
  endm
  mac g0
    dc.b 173
  endm
  mac g#0
    dc.b 178
  endm
  mac a0
    dc.b 182
  endm
  mac a#0
    dc.b 186
  endm
  mac h0
    dc.b 190
  endm
  mac c1
    dc.b 194
  endm
  mac c#1
    dc.b 197
  endm
  mac d1
    dc.b 201
  endm
  mac d#1
    dc.b 204
  endm
  mac e1
    dc.b 207
  endm
  mac f1
    dc.b 209
  endm
  mac f#1
    dc.b 212
  endm
  mac g1
    dc.b 214
  endm
  mac g#1
    dc.b 217
  endm
  mac a1
    dc.b 219
  endm
  mac a#1
    dc.b 221
  endm
  mac h1
    dc.b 223
  endm
  mac c2
    dc.b 224
  endm
  mac c#2
    dc.b 226
  endm
  mac d2
    dc.b 228
  endm
  mac d#2
    dc.b 229
  endm
  mac e2
    dc.b 231
  endm
#endif

;Silence
#mac s
  dc.b silencecode
#endm

;Duration
#mac dur
#if SYSTEM==PAL
d$ = {1}
#else
d$ = {1} * NTSC_M / NTSC_D
; The correct ratio would be 6 / 5, but not all durations in all our songs
; are multiples of 5.
#endif

  if d$ >= firstcode
    echo"Invalid duration",d$,", orig",{1}
    err
  endif

  dc.b d$
#endm

;Goto
#mac go ;address
  dc.b gotocode
  dc.b {1}-chanstart
  if {1}-chanstart > 255
    echo "too long track at",{1}
    err
  endif
#endm

;Volume fade
#mac volfade ;delay in frames between each decrement of volume
  dc.b volfadecode,{1}
#endm

;Set volume (effective from next frame)
#mac volset ;volume
  dc.b setvolcode,{1}
#endm
