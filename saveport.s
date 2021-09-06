; BeamRacer * https://beamracer.net
; Video and Display List coprocessor board for the Commodore 64
; Copyright (C)2019-2020 Mad Hackers Lab
;
; https://github.com/madhackerslab/beamracer-examples
;
store_page = $ff00
offset_page = $fe00

        .include "vlib/vasyl.s"

        jsr knock_knock

; Fill bank 1 with hi-byte decoding data.
        lda #1
        sta VREG_CONTROL

        lda #0
        sta VREG_ADR0
        sta VREG_ADR0+1
        lda #1
        sta VREG_STEP0

        ldx #0
        ldy #0
@loop:  sty VREG_PORT0
        dex
        bne @loop
        iny
        bne @loop

; Fill a single page in bank 2 with lo-byte decoding data.
        lda #2
        sta VREG_CONTROL
        lda #<store_page
        sta VREG_ADR0
        lda #>store_page
        sta VREG_ADR0+1

        ldy #0
@loop2: sty VREG_PORT0
        iny
        bne @loop2


; Fill a single page in the main bank (0) with offset decoding data.
        lda #0
        sta VREG_CONTROL

        lda #<offset_page
        sta VREG_ADR0
        lda #>offset_page
        sta VREG_ADR0+1

        ldy #$80
@loop3: sty VREG_PORT0
        iny
        cpy #$80
        bne @loop3


        jsr copy_and_activate_dlist
        rts

        .include "vlib/vlib.s"

        .segment "VASYL"
store:
        .word 0
dl_start:
        WAIT    44, 20
        MOV     $D020,1

        ; Put something intended for future preservation into ADR1.
        MOV     VREG_ADR1, <$face
        MOV     VREG_ADR1+1, >$face

        ; Jump to preservation subroutine.
        MOV     VREG_DLIST2L, <saver
        MOV     VREG_DLIST2H, >saver
        MOV     VREG_DLISTL, <return_adr
        MOV     VREG_DLISTH, >return_adr
        MOV     VREG_DL2STROBE, 0
return_adr:
        ; Restore the initial entry point.
        MOV     VREG_DLISTL, <dl_start
        MOV     VREG_DLISTH, >dl_start
        MOV     $D020,14
        END

saver:
        MOV     $D020,2
        ; We will need to read twice from the address pointed to by ADR1. Hence, the step is 0.
        MOV     VREG_STEP1, 0
        ; The first read is from bank 1, and it produces the page number.
        MOV     VREG_CONTROL, (1 << CONTROL_DLIST_ON_BIT) | (1 << CONTROL_PORT_READ_ENABLE_BIT) | 1
        ; We use STEP1 as a temporary register to store said page number (hi-byte)
        XFER    VREG_STEP1, (1)

        ; Since we have already preserved the hi-byte, we can overwrite it. We will repoint PORT1 to
        ; a page where every byte contains its sequence number (i.e. 0, 1, 2, ..., 255).
        MOV     VREG_ADR1+1, >store_page
        ; This special page is in bank 2, but it could also be located in the main (0) bank.
        MOV     VREG_CONTROL, (1 << CONTROL_DLIST_ON_BIT) | (1 << CONTROL_PORT_READ_ENABLE_BIT) | 2
        ; The lo-byte goes to STEP0.
        XFER    VREG_STEP0, (1)

        ; Let's switch back to bank 0.
        MOV     VREG_CONTROL, (1 << CONTROL_DLIST_ON_BIT) | (1 << CONTROL_PORT_READ_ENABLE_BIT) | 0

        ; Now we need to preserve the contents of STEPx registers at the location of our choosing.
        ; Another LUT will help - 256 bytes in a following sequence (128, 129, 130, ... 255, 0, 1, ..., 127).
        ;
        ; We start at the midpoint (0). The SECOND access will thus read the value of the offset.
        MOV     VREG_ADR1, <(offset_page+128)
        MOV     VREG_ADR1+1, >(offset_page+128)
        ; This is where we want to store the result (hi-byte).
        MOV     VREG_ADR0, <(store + 1)
        MOV     VREG_ADR0+1, >(store + 1)
        ; First (dummy) read. It does ADR1 <- ADR1 + STEP1, and then overwrites STEP1 (which we no longer need).
        XFER    VREG_STEP1, (1)
        ; Read the value of interest and send it to PORT0 (i.e. to store + 1)
        XFER    VREG_PORT0, (1)

        ; Now the same thing for the lo-byte
        MOV     VREG_ADR0, <(offset_page+128)
        MOV     VREG_ADR0+1, >(offset_page+128)
        MOV     VREG_ADR1, <(store + 0)
        MOV     VREG_ADR1+1, >(store + 0)
        XFER    VREG_STEP0, (0)
        XFER    VREG_PORT1, (0)
        
        ; Visual marker and return from the subroutine.
        MOV     $D020,6
        MOV     VREG_DLSTROBE, 0


