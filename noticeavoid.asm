	processor 6502          ; Ex. 1 Collision Detection - Bouncing Ball
                                ;
	include	 "vcs.h"	; this example uses the bounce/reflect method used in the game COMBAT
	include  "macro.h"	;  
                                
	; Subscribe to 8Blit for more videos for programming the ATARI 2600.
	; https://www.youtube.com/8blit
	;
	; Please consider supporting the channel by becoming a patron.
	; Your support is greatly appreciated! https://www.patreon.com/8blit
	;
	; Visit our website
	; https://www.8blit.com

	include "xmacro.h"
; define constants
THREE_COPIES    equ %011
PFFG	equ $0F
PFBG	equ $00

	seg.u	vars		; uninitialized segment
	org	$80             ; origin set at base of ram
temp1	ds 1 ; temporary variable
loopCount ds 1; a better name
XPos .byte
YPos .byte

	seg	Code    	; start of main segment
	org $F000

Reset:	
	CLEAN_START

NAFrame:
	VERTICAL_SYNC
	TIMER_SETUP 37 ; V-Blank
	lda #75
	sta loopCount	; scanline counter
	lda #$00
	sta COLUBK	; background color
	lda #$0E
	sta COLUP0	; show how players alternate
	lda #$0E
	sta COLUP1	; by having different colors
	lda #THREE_COPIES
	sta NUSIZ0
	sta NUSIZ1	; both players have 3 copies
	sta WSYNC
	SLEEP 20
	sta RESP0	; position 1st player
	sta RESP1	; ...and 2nd player
	lda #$10
	sta HMP1	; 1 pixel to the left
	sta WSYNC
	sta HMOVE	; apply HMOVE
	SLEEP 24	; sleep 24 cycles
	sta HMCLR	; clear HMOVE registers
	lda #1
	sta VDELP0	; we need the VDEL registers
	sta VDELP1	; so we can do our 4-store trick
	TIMER_WAIT ; actually wait the v-sync


	TIMER_SETUP 20 ; skip 20 lines for positioning
	TIMER_WAIT

	SLEEP 40
; dump out our player photo
PhotoLoop:
	ldy loopCount                   ; 3     (3) 
	lda photo_0,y                   ; 4     (7)
	sta GRP0                        ; 3     (10) 0 -> [GRP0]
	lda photo_1,y                   ; 4     (14)
	sta GRP1                        ; 3     (17) 1 -> [GRP1] ; 0 -> GRP0
	sta WSYNC                       ; 3     (20)
	lda photo_2,y                   ; 4     (24)
	sta GRP0                        ; 3     (27*) 2 -> [GRP0] ; 1 -> GRP1
	lda photo_5,y                   ; 4     (31) 4 -> X
	sta temp1                       ; 3     (34)
	ldx photo_4,y                   ; 4     (36)
	lda photo_3,y                   ; 4     (40) 3 -> A
	ldy temp1                       ; 3     (43) 5 -> Y
	sta GRP1                        ; 3     (46) 3 -> [GRP1] ; 2 -> GRP0
	stx GRP0                        ; 3     (49) 4 -> [GRP0] ; 3 -> GRP1
	sty GRP1                        ; 3     (52) 5 -> [GRP1] ; 4 -> GRP0
	sta GRP0                        ; 3     (55) 5 -> GRP1 
	dec loopCount                   ; 5     (60)
	bpl PhotoLoop                   ; 3     (63)

	TIMER_SETUP 20 ; skip 20 more lines for positioning
	TIMER_WAIT

; dump in the mean streets
	lda #PFBG
	sta COLUBK
	lda #PFFG
	sta COLUPF
	ldx #8
StreetsOuter:
	ldy #7
StreetsLoop:
	sta WSYNC	; wait for next scanline
	lda PFData0,y
	sta PF0		; set the PF1 playfield pattern register
	lda PFData1,y
	sta PF1		; set the PF1 playfield pattern register
	lda PFData2,y
	sta PF2		; set the PF2 playfield pattern register
	dey
	bne StreetsLoop
	dex
	bne StreetsOuter
; clear out the playfield so it doesn't display when we don't want it.
	sta WSYNC
	lda #0
	sta PF0
	sta PF1
	sta PF2

; now wait the rest of the screen
	TIMER_SETUP 32
    TIMER_WAIT

; now wait the overscan
	TIMER_SETUP 30
    TIMER_WAIT
	jmp NAFrame

; sum of all waits = 3 + 37 + 192 + 30



MoveJoystick
; Move vertically
; (up and down are actually reversed since ypos starts at bottom)
	ldx YPos
	lda #%00100000	;Up?
	bit SWCHA
	bne SkipMoveUp
    cpx #2
    bcc SkipMoveUp
	dex
SkipMoveUp
	lda #%00010000	;Down?
	bit SWCHA 
	bne SkipMoveDown
    cpx #183
    bcs SkipMoveDown
    inx
SkipMoveDown
	stx YPos
; Move horizontally
    ldx XPos
	lda #%01000000	;Left?
	bit SWCHA
	bne SkipMoveLeft
    cpx #16
    bcc SkipMoveLeft
    dex
SkipMoveLeft
	lda #%10000000	;Right?
	bit SWCHA 
	bne SkipMoveRight
    cpx #153
    bcs SkipMoveRight
    inx
SkipMoveRight
	stx XPos
	rts

; https://www.flickr.com/photos/tokyodrifter/4132540774/in/photolist-7ibmp1-7kwDQr-6drtzM-7i7rqD-fVoZL2-283iFVj-7i7rtP-jYc5Bt-B84WBv-7ibmpY-bZTLow-dEY7Uh-qgio3b-ezNHdC-7i7rrr-7i7TLn-2j4x2vd-nfcDxV-4n8mWY-oMAYTH-dEZXBE-uW2BtA-2i4aDit-nYsFmQ-vo5JMz-5T3mhX-NQZLh3-bBAki7-eZwFDo-cm8FiA-2k3G9o1-qCxgBM-edxNEE-69tfxm-4VoNiK-d3aAQh-oZFiyw-nNrGQY-r63Zzb-BCmnBS-f5sW2P-2d3dWDA-agRe5T-a6uZq8-aNupzD-dRJQVC-6b3nTH-D9as7H-5p8iUR-h1bFAk
	align $100
photo_0
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %10000000
	BYTE %10000000
	BYTE %10000000
	BYTE %11000000
	BYTE %11100000
	BYTE %11110000
	BYTE %11111000
	BYTE %11111110
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %00000000

	align $100

photo_1
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %10000000
	BYTE %11100000
	BYTE %11110000
	BYTE %11111000
	BYTE %11111100
	BYTE %11111100
	BYTE %11111110
	BYTE %11111110
	BYTE %11111110
	BYTE %11111110
	BYTE %11111110
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %00000000

	align $100
photo_2
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %10000000
	BYTE %10000000
	BYTE %10000000
	BYTE %10001000
	BYTE %11000001
	BYTE %11000001
	BYTE %11000000
	BYTE %11100001
	BYTE %11110001
	BYTE %11100000
	BYTE %11110000
	BYTE %11110000
	BYTE %11110010
	BYTE %11110000
	BYTE %11111000
	BYTE %11111000
	BYTE %11111000
	BYTE %11111000
	BYTE %11111000
	BYTE %11111100
	BYTE %11111000
	BYTE %11111100
	BYTE %11111101
	BYTE %11111110
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %00000000

	align $100
photo_3
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00001000
	BYTE %00001000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000100
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000001
	BYTE %00000001
	BYTE %00000001
	BYTE %00000011
	BYTE %00000001
	BYTE %00000001
	BYTE %00000001
	BYTE %00000001
	BYTE %00000001
	BYTE %00000001
	BYTE %00000001
	BYTE %01000001
	BYTE %00000001
	BYTE %00000000
	BYTE %00000000
	BYTE %00001000
	BYTE %00001000
	BYTE %00001000
	BYTE %00000000
	BYTE %00000110
	BYTE %00011100
	BYTE %00110100
	BYTE %00100100
	BYTE %00000010
	BYTE %00000000
	BYTE %00000010
	BYTE %00000000
	BYTE %00000010
	BYTE %00000000
	BYTE %00000001
	BYTE %00000100
	BYTE %10111000
	BYTE %00100000
	BYTE %00000000
	BYTE %00000000
	BYTE %11000000
	BYTE %11000000
	BYTE %11100000
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %00000000

	align $100
photo_4
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00100000
	BYTE %11110000
	BYTE %11110000
	BYTE %11111000
	BYTE %11111000
	BYTE %11111000
	BYTE %11110000
	BYTE %11000100
	BYTE %11111100
	BYTE %11111100
	BYTE %11111110
	BYTE %11111010
	BYTE %11111110
	BYTE %11111110
	BYTE %11111110
	BYTE %11111110
	BYTE %11111110
	BYTE %11111110
	BYTE %11100110
	BYTE %01100111
	BYTE %01100011
	BYTE %00010011
	BYTE %00110111
	BYTE %00111111
	BYTE %00111111
	BYTE %00111111
	BYTE %00111111
	BYTE %00011111
	BYTE %00011110
	BYTE %00011100
	BYTE %00001000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000000
	BYTE %00000011
	BYTE %11111111
	BYTE %11111111
	BYTE %00000000

	align $100
photo_5
	BYTE %00000000
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %01111111
	BYTE %00111111
	BYTE %00111111
	BYTE %00111111
	BYTE %00111111
	BYTE %00111111
	BYTE %00111111
	BYTE %00111111
	BYTE %00011111
	BYTE %00011111
	BYTE %00011111
	BYTE %00011111
	BYTE %00011111
	BYTE %00011111
	BYTE %00011111
	BYTE %00011111
	BYTE %00011111
	BYTE %00001111
	BYTE %00001111
	BYTE %00001111
	BYTE %00001111
	BYTE %00001111
	BYTE %00001111
	BYTE %00001111
	BYTE %00001111
	BYTE %00001111
	BYTE %00001111
	BYTE %00001111
	BYTE %10001111
	BYTE %10001111
	BYTE %10001111
	BYTE %10001111
	BYTE %10001111
	BYTE %10001111
	BYTE %10001111
	BYTE %10001111
	BYTE %10001111
	BYTE %00001111
	BYTE %00001111
	BYTE %00001111
	BYTE %00001111
	BYTE %00001111
	BYTE %00011111
	BYTE %00011111
	BYTE %00011111
	BYTE %00011111
	BYTE %00111111
	BYTE %00111111
	BYTE %01111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %11111111
	BYTE %00000000




	align 100
PFData0
        .byte #%11110000
        .byte #%11110000
        .byte #%11110000
        .byte #%11110000
        .byte #%11110000
        .byte #%11110000
        .byte #%11110000
        .byte #%11110000
PFData1
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
PFData2
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111


	ORG $FFFA

InterruptVectors:
	.word Reset          ; NMI
	.word Reset          ; RESET
	.word Reset          ; IRQ