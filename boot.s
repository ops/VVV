;;;
;;; VVV boot program
;;;

        .import __GIJ_HOSTCODE_LOAD__
        .import __GIJ_HOSTCODE_RUN__
        .import __GIJ_HOSTCODE_SIZE__

        .import __LOADERCODE_LOAD__
        .import __LOADERCODE_RUN__
        .import __LOADERCODE_SIZE__

        .import gij_load_init
        .import gij_load

        .segment "CODE"

        jsr     gij_load_init

        ldx     #$00
:       lda     __GIJ_HOSTCODE_LOAD__,x
        sta     __GIJ_HOSTCODE_RUN__,x
        inx
        cpx     #< __GIJ_HOSTCODE_SIZE__
        bne    :-

        ldx     #$00
:       lda     __LOADERCODE_LOAD__,x
        sta     __LOADERCODE_RUN__,x
        inx
        cpx     #< __LOADERCODE_SIZE__
        bne    :-

        ldy     #$80
:       inx
        bne     :-
        dey
        bne     :-

        lda     #>(4621-1)
        pha
        lda     #<(4621-1)
        pha

        ldx     #'0'
        ldy     #'0'
        jmp     gij_load

        .segment "LOADERCODE"

        ldx     #'0'
name:   ldy     #'1'
        inc    name+1
        jmp    gij_load
