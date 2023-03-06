    processor 6502          ; Notice Avoid https://github.com/grondak/noticeavoid
    include	 "vcs.h"
    include  "macro.h"
    include "xmacro.h"
;------------------------------------------------
; Variables - some made at https://alienbill.com/2600/playerpalnext.html
;------------------------------------------------
    seg.u	vars		; uninitialized segment
    org	$80             ; origin set at base of ram

; for the photo
photoTemp	        ds 1 ; temporary variable
photoLines          ds 1; a better name
; for the joysticks
XPos                ds 0
YPos                ds 0
; for the Lookie Loos Animation
spriteYPosition		ds 1	; 192 is at the top of the screen, the constant VALUE_OF_Y_AT_SCREEN_BOTTOM gives us the bottom.
currentSpriteLine	ds 1	; (0 &lt;= currentSpriteLine &lt; SPRITE_HEIGHT) for each frame
hPosition			ds 1
hPositionFrac		ds 1
playerBuffer		ds 1
spriteMoving		ds 1	; Boolean. We use this to see if we stopped moving
animFrameLineCtr	ds 1
faceDelay			ds 1
spriteLineColor		ds 1
hPositionIndex		ds 1
faceDuration		ds 1

;------------------------------------------------
; Constants - some made at https://alienbill.com/2600/playerpalnext.html
;------------------------------------------------
; for the photo
THREE_COPIES  = %011        ; draw three copies of the sprite for the photo
; for the streets
PFFG = $0F                  ; the streets are a concrete jungle
PFBG = $00                  ; everything else is a shadow
; for the Lookie Loos
FACE_DURATION = 4			; Number of frames each face lasts on screen. Decrease to speed up, increase to slow down.
FACE_COLOR = $0F            ; bright white
SLO_MO_FACE_DURATION = 30	; Same as above, applicable when "slo-mo" is activated (i.e. player holds fire button).
SPRITE_HEIGHT = 8			; Native number of pixels tall the sprite is (before being stretched by a 2LK or whatever).
NUM_ANIMATION_FACES = 9		; Number of faces of animation. (!)Corresponds with number of color tables(!)
MIRROR = 1					; If true, sprite mirrors when moved left.
X_LK = 1					; set to 1 for 1LK, 2 for 2LK, etc.
SPRITE_WIDTH = 1			; set to 1, 2, or 4, anything else is right out
BG_COLOR = $00				; background color
VALUE_OF_Y_AT_SCREEN_BOTTOM = 192-192/X_LK
VERTICAL_CENTER_OF_SCREEN = 192-(192-VALUE_OF_Y_AT_SCREEN_BOTTOM)/2


;------------------------------------------------
; Macros (hey, my first macros!) - made at https://alienbill.com/2600/playerpalnext.html
;------------------------------------------------

    MAC KERNAL
        REPEAT X_LK
            sta WSYNC
        REPEND
    ENDM
    
    MAC UP_DIST_MACRO
        inc spriteYPosition
        
        IF X_LK = 1
            inc spriteYPosition		; we move a little extra to speed up vertical motion in 1LK
        ENDIF
    ENDM

    MAC DOWN_DIST_MACRO
        dec spriteYPosition
        
        IF X_LK = 1
            dec spriteYPosition		; we move a little extra to speed up vertical motion in 1LK
        ENDIF
    ENDM

    seg	Code    	; start of main segment
    org $F000
reset:	
    CLEAN_START
naFrame: ; draw one Notice Avoid Frame
    VERTICAL_SYNC
    TIMER_SETUP 37 ; V-Blank
    jsr photoDraw ; draw the photo first
; skip 20 more lines for positioning
    TIMER_SETUP 20 
    TIMER_WAIT
    jsr streetsDraw ; draw the streets one line at a time
; now wait the rest of the screen
    TIMER_SETUP 32
    TIMER_WAIT
; now wait the overscan
    TIMER_SETUP 30
    TIMER_WAIT
    jsr readJoysticks
    jmp naFrame



photoDraw:
    lda #75
    sta photoLines	; scanline counter
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
photoLoop:
    ldy photoLines                  ; 3     (3) 
    lda photo_0,y                   ; 4     (7)
    sta GRP0                        ; 3     (10) 0 -> [GRP0]
    lda photo_1,y                   ; 4     (14)
    sta GRP1                        ; 3     (17) 1 -> [GRP1] ; 0 -> GRP0
    sta WSYNC                       ; 3     (20)
    lda photo_2,y                   ; 4     (24)
    sta GRP0                        ; 3     (27*) 2 -> [GRP0] ; 1 -> GRP1
    lda photo_5,y                   ; 4     (31) 4 -> X
    sta photoTemp                   ; 3     (34)
    ldx photo_4,y                   ; 4     (36)
    lda photo_3,y                   ; 4     (40) 3 -> A
    ldy photoTemp                   ; 3     (43) 5 -> Y
    sta GRP1                        ; 3     (46) 3 -> [GRP1] ; 2 -> GRP0
    stx GRP0                        ; 3     (49) 4 -> [GRP0] ; 3 -> GRP1
    sty GRP1                        ; 3     (52) 5 -> [GRP1] ; 4 -> GRP0
    sta GRP0                        ; 3     (55) 5 -> GRP1 
    dec photoLines                  ; 5     (60)
    bpl photoLoop                   ; 3     (63)
    rts


streetsDraw:
; dump in the mean streets
    lda #PFBG
    sta COLUBK
    lda #PFFG
    sta COLUPF
    ldx #8
streetsOuter:
    ldy #7
streetsInner:
    sta WSYNC	; wait for next scanline
    lda pfData0,y
    sta PF0		; set the PF1 playfield pattern register
    lda pfData1,y
    sta PF1		; set the PF1 playfield pattern register
    lda pfData2,y
    sta PF2		; set the PF2 playfield pattern register
    jsr tempLoosDraw
    dey
    bne streetsInner
    dex
    bne streetsOuter
; clear out the playfield so it doesn't display when we don't want it.
    sta WSYNC
    lda #0
    sta PF0
    sta PF1
    sta PF2
    rts



; Draw some LookieLoos
tempLoosDraw:
    rts

readJoysticks:
; Move vertically
; (up and down are actually reversed since ypos starts at bottom)
    ldx YPos
    lda #%00100000	;Up?
    bit SWCHA
    bne skipMoveUp
    cpx #2
    bcc skipMoveUp
    dex
skipMoveUp:
    lda #%00010000	;Down?
    bit SWCHA 
    bne skipMoveDown
    cpx #183
    bcs skipMoveDown
    inx
skipMoveDown:
    stx YPos
; Move horizontally
    ldx XPos
    lda #%01000000	;Left?
    bit SWCHA
    bne skipMoveLeft
    cpx #16
    bcc skipMoveLeft
    dex
skipMoveLeft:
    lda #%10000000	;Right?
    bit SWCHA 
    bne skipMoveRight
    cpx #153
    bcs skipMoveRight
    inx
skipMoveRight:
    stx XPos
    rts

; https://www.flickr.com/photos/tokyodrifter/4132540774/in/photolist-7ibmp1-7kwDQr-6drtzM-7i7rqD-fVoZL2-283iFVj-7i7rtP-jYc5Bt-B84WBv-7ibmpY-bZTLow-dEY7Uh-qgio3b-ezNHdC-7i7rrr-7i7TLn-2j4x2vd-nfcDxV-4n8mWY-oMAYTH-dEZXBE-uW2BtA-2i4aDit-nYsFmQ-vo5JMz-5T3mhX-NQZLh3-bBAki7-eZwFDo-cm8FiA-2k3G9o1-qCxgBM-edxNEE-69tfxm-4VoNiK-d3aAQh-oZFiyw-nNrGQY-r63Zzb-BCmnBS-f5sW2P-2d3dWDA-agRe5T-a6uZq8-aNupzD-dRJQVC-6b3nTH-D9as7H-5p8iUR-h1bFAk
    align $100
photo_0:
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

photo_1:
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
photo_2:
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
photo_3:
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
photo_4:
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
photo_5:
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

;---Lookie Loos Graphics Data from PlayerPal 2600--- - https://alienbill.com/2600/playerpalnext.html

llNeutral:
        .byte #%00000000;--
        .byte #%00111100;--
        .byte #%00000000;--
        .byte #%00011000;--
        .byte #%00000000;--
        .byte #%11101110;--
        .byte #%10101010;--
        .byte #%11101110;--
llUp:
        .byte #%00000000;--
        .byte #%00111100;--
        .byte #%00000000;--
        .byte #%00011000;--
        .byte #%00000000;--
        .byte #%11101110;--
        .byte #%10101010;--
        .byte #%10101010;--
llUpRight:
        .byte #%00000000;--
        .byte #%00111100;--
        .byte #%00000000;--
        .byte #%00011000;--
        .byte #%00000000;--
        .byte #%11101110;--
        .byte #%10101010;--
        .byte #%11001100;--
llRight:
        .byte #%00000000;--
        .byte #%00111100;--
        .byte #%00000000;--
        .byte #%00011000;--
        .byte #%00000000;--
        .byte #%11101110;--
        .byte #%10001000;--
        .byte #%11101110;--
llDownRight:
        .byte #%00000000;--
        .byte #%00111100;--
        .byte #%00000000;--
        .byte #%00011000;--
        .byte #%00000000;--
        .byte #%11001100;--
        .byte #%10101010;--
        .byte #%11101110;--
llDown:
        .byte #%00000000;--
        .byte #%00111100;--
        .byte #%00000000;--
        .byte #%00011000;--
        .byte #%00000000;--
        .byte #%10101010;--
        .byte #%10101010;--
        .byte #%11101110;--
llDownLeft:
        .byte #%00000000;--
        .byte #%00111100;--
        .byte #%00000000;--
        .byte #%00011000;--
        .byte #%00000000;--
        .byte #%01100110;--
        .byte #%10101010;--
        .byte #%11101110;--
llLeft:
        .byte #%00000000;--
        .byte #%00111100;--
        .byte #%00000000;--
        .byte #%00011000;--
        .byte #%00000000;--
        .byte #%11101110;--
        .byte #%00100010;--
        .byte #%11101110;--
llUpLeft:
        .byte #%00000000;--
        .byte #%00111100;--
        .byte #%00000000;--
        .byte #%00011000;--
        .byte #%00000000;--
        .byte #%11101110;--
        .byte #%10101010;--
        .byte #%01100110;--
;---End Lookie Loos Graphics Data---


    align 100
pfData0:
        .byte #%11110000
        .byte #%11110000
        .byte #%11110000
        .byte #%11110000
        .byte #%11110000
        .byte #%11110000
        .byte #%11110000
        .byte #%11110000
pfData1:
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
pfData2:
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111

    ORG $FFFA
interruptVectors:
    .word reset          ; NMI
    .word reset          ; RESET
    .word reset          ; IRQ
