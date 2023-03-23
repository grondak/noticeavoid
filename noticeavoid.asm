    processor 6502          ; Notice Avoid https://github.com/grondak/noticeavoid
    include "vcs.h"
    include "macro.h"
    include "xmacro.h"
;------------------------------------------------
; Variables - some made at https://alienbill.com/2600/playerpalnext.html
;------------------------------------------------
    seg.u	vars		; uninitialized segment
    org	$80             ; origin set at base of ram

; for the photo
photoTemp           ds 1 ; temporary variable
photoLines          ds 1; a better name; for the Lookie Loos Animation
tempX               ds 1
looDirection0       ds 1    ; #%00000001 for down for first loo
spriteYPosition0    ds 1	; 192 is at the top of the screen, the constant VALUE_OF_Y_AT_SCREEN_BOTTOM gives us the bottom.
currentSpriteLine0  ds 1	; (0 &lt;= currentSpriteLine0 &lt; SPRITE_HEIGHT) for each frame
hPosition0          ds 1
playerBuffer0       ds 1
spriteMoving0       ds 1	; Boolean. We use this to see if we stopped moving
animFrameLineCtr0   ds 1
faceDelay0          ds 1
spriteLineColor0    ds 1
hPositionIndex0     ds 1
faceDuration0       ds 1
looDirection1       ds 1
spriteYPosition1    ds 1	; 192 is at the top of the screen, the constant VALUE_OF_Y_AT_SCREEN_BOTTOM gives us the bottom.
currentSpriteLine1  ds 1	; (0 &lt;= currentSpriteLine1 &lt; SPRITE_HEIGHT) for each frame
hPosition1          ds 1
playerBuffer1       ds 1
spriteMoving1       ds 1	; Boolean. We use this to see if we stopped moving
animFrameLineCtr1   ds 1
faceDelay1          ds 1
spriteLineColor1    ds 1
hPositionIndex1     ds 1
faceDuration1       ds 1
ballYPosition       ds 1
ballHPosition       ds 1
ballHPositionIndex  ds 1
ballColor           ds 1
temp                ds 1
loopCount           ds 1; counts scanline when drawing

; Pointers to bitmap for each digit
digit0		.word
digit1		.word
digit2		.word
digit3		.word
digit4		.word
digit5		.word

BCDDeliveryScore    hex 000000
BCDAnxietyScore     hex 000000



;------------------------------------------------
; Constants - some made at https://alienbill.com/2600/playerpalnext.html
;------------------------------------------------
; for the photo
THREE_COPIES = %011        ; draw three copies of the sprite for the photo
; for the streets
PFFG = $0F                  ; the streets are a concrete jungle
PFBG = $00                  ; everything else is a shadow
; for the Lookie Loos
FACE_COLOR = $0F            ; bright white
FACE_DURATION = 72
SLO_MO_FACE_DURATION = 30	; Same as above, applicable when "slo-mo" is activated (i.e. player holds fire button).
SPRITE_HEIGHT = 8			; Native number of pixels tall the sprite is (before being stretched by a 2LK or whatever).
NUM_ANIMATION_FACES = 9		; Number of faces of animation. (!)Corresponds with number of color tables(!)
MIRROR = 1					; If true, sprite mirrors when moved left.
X_LK = 1					; set to 1 for 1LK, 2 for 2LK, etc.
BG_COLOR = $00				; background color
VALUE_OF_Y_AT_SCREEN_BOTTOM = 192-192/X_LK
VERTICAL_CENTER_OF_SCREEN = 192-(192-VALUE_OF_Y_AT_SCREEN_BOTTOM)/2
LOO_DIRECTION_0 = %00000001
LOO_DIRECTION_1 = %00000010
ballStartingYPosition = 15
ballStartingHPosition = 15
trk0idx = $e0	; offset into tracks for channel 0 / integrated from https://8bitworkshop.com/v3.10.0/?platform=vcs&file=examples%2Fmusicplayer.a#
trk1idx	= $e1	; offset into tracks for channel 1 / with improvements to allow for rests! WHAT?
pat0idx	= $e2	; offset into patterns for channel 0
pat1idx	= $e3	; offset into patterns for channel 1
chan0dur = $e4	; current note duration channel 0
chan1dur = $e5	; current note duration channel 1
chan0note = $e6	; current note pitch channel 0
chan1note = $e7	; current note pitch channel 1



;------------------------------------------------
; Macros (hey, my first macros!) - made at https://alienbill.com/2600/playerpalnext.html
;------------------------------------------------

    MAC KERNAL
        REPEAT X_LK
            sta WSYNC
        REPEND
    ENDM
    
    MAC UP_DIST_MACRO
        inc spriteYPosition0
                
        IF X_LK = 1
            inc spriteYPosition0		; we move a little extra to speed up vertical motion in 1LK
        ENDIF
    ENDM

    MAC DOWN_DIST_MACRO
        dec spriteYPosition0
        
        IF X_LK = 1
            dec spriteYPosition0		; we move a little extra to speed up vertical motion in 1LK
        ENDIF
    ENDM
; Usage: NOTE pitch duration
; Plays a note in a pattern.
; pitch = 0-31
; duration = 1-7, uses durFrames lookup table
    MAC NOTE
.pitch	SET {1}
.durat	SET {2}
    .byte (.pitch+(.durat<<5))
    ENDM

; Usage: TONE tone
; Changes the tone in a pattern.
; tone = 1-15
    MAC TONE
.tone	SET {1}
    .byte .tone
    ENDM

; Usage: PATTERN address
; Plays a pattern in a track.
    MAC PATTERN
.addr	SET {1}
    .byte (.addr-patterns)
    ENDM
        
; Usage: ENDTRACK
; Marks the end of a track.
    MAC ENDTRACK
    .byte 0
    ENDM


    seg	Code    	; start of main segment
    org $F000
; restart and when reset switch is pulled
reset:	
    CLEAN_START
    lda #$56
    sta BCDDeliveryScore
    lda #$34
    sta BCDDeliveryScore+1
    lda #$12
    sta BCDDeliveryScore+2
    lda #FACE_DURATION
    sta faceDelay0
    sta faceDelay1
    jsr resetTrack
    lda #80
    sta hPositionIndex0	; initial x pos for loo0
    lda #28
    sta spriteYPosition0	; initial y pos for loo0
    lda #LOO_DIRECTION_0
    sta looDirection0       ; initial direction for loo0
    lda #20
    sta hPositionIndex1	; initial x pos for loo0
    lda #14
    sta spriteYPosition1	; initial y pos for loo0
    lda #LOO_DIRECTION_1
    sta looDirection1       ; initial direction for loo0
    lda #ballStartingYPosition
    sta ballYPosition
    lda ballStartingHPosition
    sta ballHPositionIndex
    sta ballHPositionIndex
    lda #PFBG
    sta COLUBK
    lda #PFFG
    sta COLUPF
    lda #1
    sta CTRLPF
    lda #2
    sta ENABL
    sta WSYNC
 ; draw one Notice Avoid Frame - 2 line kernel
naFrame:
    VERTICAL_SYNC  ;3 lines total 3
    lda #0
    sta VSYNC		; turn off VSYNC by clearing it

    
    TIMER_SETUP 37 ; V-Blank 37 lines total 40
    ldx #0
    jsr musicFrame
    ldx #1
    jsr musicFrame
    jsr digitsSetup
    jsr getDigitPtrs	; get pointers
    jsr loosMovement0
    jsr loosMovement1
    TIMER_WAIT ;the V-Blank wait
    sta VBLANK      ; that too
; skip 20 lines for positioning
    TIMER_SETUP 20 ; 20 lines total 60   
; draw the photo first ; 75 lines total 135
    jsr drawDigits		; draw digits
    jsr photoDraw 
; skip 19 more lines for positioning
    TIMER_SETUP 17 ; 17 lines total 152
; set up the player's ball
    jsr ballSetup
; set up the Lookie Loos (1x)
    jsr smartLoosSetup

    TIMER_WAIT
    jsr streetsDraw ; draw the streets one line at a time
                    ; 60 lines total 212
; now wait the rest of the screen
    TIMER_SETUP 5 ; 20 lines total 232
    TIMER_WAIT
; now wait the overscan
    lda #%00000010				;2
    sta VBLANK           		;3 end of screen - enter blanking
    TIMER_SETUP 30 ; 30 lines total 262
    jsr readJoysticks
    jsr animateLoos
    TIMER_WAIT
    jmp naFrame

digitsSetup:
    lda #$06
    sta COLUP0
    lda #$31
    sta COLUP1
    lda #THREE_COPIES
    sta NUSIZ0
    sta NUSIZ1
; set horizontal position of player objects
    sta WSYNC
    SLEEP 25
    sta RESP0
    ;SLEEP 5
    sta RESP1

    lda #$F0
    sta HMP0
    lda #00
    sta HMP1
    sta WSYNC
    sta HMOVE
    SLEEP 24	; wait 24 cycles between write to HMOVE and HMxxx
    sta HMCLR
    lda #1
    sta VDELP0
    sta VDELP1
    rts

; Set up to draw our current photo
photoDraw:
    lda #75
    sta photoLines	; scanline counter
    lda #PFBG
    sta COLUBK	; background color
    lda #PFFG
    sta COLUP0	; show how players alternate
    lda #PFFG
    sta COLUP1	; by having different colors
    lda #THREE_COPIES
    sta NUSIZ0
    sta NUSIZ1	; both players have 3 copies
    sta WSYNC
    SLEEP 20
    sta RESP0	; position 1st player
    sta RESP1	; ...and 2nd player
    sta HMP0
    lda #$10
    sta HMP1	; 1 pixel to the left
    sta WSYNC
    sta HMOVE	; apply HMOVE
    SLEEP 24	; sleep 24 cycles
    sta HMCLR	; clear HMOVE registers
    lda #1
    sta VDELP0	; we need the VDEL registers
    sta VDELP1	; so we can do our 4-store trick
    TIMER_WAIT ; actually wait the remaining lines
    SLEEP 40

; dump out our player photo line by line
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


; dump in the mean streets
streetsDraw:
    ldy #28     ; 56 total scan lines because we run WSYNC twice per Y
    ldx #4      ; groups of 8 playfield lines
    lda #$FF
    sta PF0		; set the PF1 playfield pattern register
    sta PF1		; set the PF1 playfield pattern register
    sta PF2		; set he PF2 playfield pattern register
    lda roomStyle0,x
    sta WSYNC
    sta PF0
    sta PF1
    sta PF2
    nop
    nop
    nop
    lda roomStyle1,x
    sta PF0
    sta PF1
    sta PF2
    lda #1
streetsInner:
    stx tempX
; Draw loo0
loosDraw0:
    ; Load Player sprite and color. (10~)
    ;sta WSYNC
    lda playerBuffer0			;2
    sta GRP0					;3	GRP0 = playerBuffer0
    lda playerBuffer1			;2
    sta GRP1	
    lda spriteLineColor0			;2
    sta COLUP0					;3	COLUP0 = spriteLineColor0
				;3	GRP1 = playerBuffer1
    lda spriteLineColor1			;2
    sta COLUP1	
    ; Clear the playerBuffer0. (5~)
    lda #0						;2
    sta playerBuffer0			;3	playerBuffer0 = 0    	        sta playerBuffer1			;3	playerBuffer1 = 0    	
    sta playerBuffer1
    ; See if this is the line where we start drawing the sprite. (Y:10~, N:6~)
    cpy spriteYPosition0			;3
    bne skipActivateLoo0		;2+	if (y != spriteYPosition0) goto SkipActivatePlayer

    lda #SPRITE_HEIGHT-1		;2	else
    sta currentSpriteLine0		;3	currentSpriteLine0 = SPRITE_HEIGHT-1
skipActivateLoo0:
    ; See if we are drawing sprite data on this line. (Y:5~, N:6~)
    lda currentSpriteLine0		;3
    bmi loosDraw1			;2+	if (currentSpriteLine0 &lt; 0) goto endFaceStuff

    ; Load sprite graphic and color buffers. (20~)
    ldx animFrameLineCtr0		;3
    lda llGraphicTable,x	    ;4
    sta playerBuffer0			;3	playerBuffer0 = SpriteGraphicTable[animFrameLineCtr0]
    ; Decrement our counters. (10~)
    dec currentSpriteLine0		;5 currentSpriteLine0 -= 1
    dec animFrameLineCtr0		;5
loosDraw1:
    ; Load Player sprite and color. (10~)
    ; See if this is the line where we start drawing the sprite. (Y:10~, N:6~)
    cpy spriteYPosition1			;3
    bne skipActivateLoo1		;2+	if (y != spriteYPosition1) goto SkipActivatePlayer

    lda #SPRITE_HEIGHT-1		;2	else
    sta currentSpriteLine1		;3	currentSpriteLine1 = SPRITE_HEIGHT-1
skipActivateLoo1:
    ; See if we are drawing sprite data on this line. (Y:5~, N:6~)
    lda currentSpriteLine1		;3
    bmi ballDraw			;2+	if (currentSpriteLine1 &lt; 0) goto endFaceStuff

    ; Load sprite graphic and color buffers. (20~)
    ldx animFrameLineCtr1		;3
    lda llGraphicTable,x	    ;4
    sta playerBuffer1			;3	playerBuffer1 = SpriteGraphicTable[animFrameLineCtr0]
    ; Decrement our counters. (10~)
    dec currentSpriteLine1		;5 currentSpriteLine1 -= 1
    dec animFrameLineCtr1		;5
    ; Manage the frame delay between face animations. 
ballDraw:
    lda #0
    sta ENABL
    cpy ballYPosition
    bne skipBallDraw
    lda #2
    sta ENABL
skipBallDraw:
    ldx tempX
    sta WSYNC
    dey
    beq streetsDone
    lda roomStyle0,x
    sta PF0
    sta PF1
    sta PF2
    SLEEP 18
    
    lda roomStyle1,x
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    dex
    bne streetsInner
    ldx #4
    jmp streetsInner
streetsDone:
    lda #$FF
    sta PF0		; set the PF1 playfield pattern register
    sta PF1		; set the PF1 playfield pattern register
    sta PF2		; set he PF2 playfield pattern register
    lda #0
    sta WSYNC
; clear out the playfield so it doesn't display when we don't want it.
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    rts



; prepare to draw all LookieLoos
smartLoosSetup:
    lda #$25
    sta COLUP0
    sta spriteLineColor0
    lda #$42
    sta COLUP1
    sta spriteLineColor1
    lda #0					; set to single
    sta NUSIZ0
    sta VDELP0
    sta NUSIZ1
    sta VDELP1

    ldx hPositionIndex0		;3	|
    lda hPositionTable,x	;4	|
    sta hPosition0			;3	| hPosition0 = hPositionTable[hPositionIndex0]
    and #$0F				;2	|
    tax						;2	| x = (hPosition0 & $0F) (coarse position)
    sta WSYNC
position0:
    dex						;2	| Position Sprite Horizontally (coarse adj.)
    bne position0			;2+	|
    sta RESP0				;3	|

    ldx hPositionIndex1		;3	|
    lda hPositionTable,x	;4	|
    sta hPosition1			;3	| hPosition0 = hPositionTable[hPositionIndex0]
    and #$0F				;2	|
    tax						;2	| x = (hPosition0 & $0F) (coarse position)
    sta WSYNC
position1:
    dex						;2	| Position Sprite Horizontally (coarse adj.)
    bne position1			;2+	|
    sta RESP1				;3	|
    lda hPosition0
    and #$F0				;2	| clear coarse nybble
    sta HMP0
    lda hPosition1			;3	|
    and #$F0				;2	| clear coarse nybble
    sta HMP1
    sta WSYNC
    sta HMOVE
    lda #1                  ; 0 for no animation, 1 for animation
    sta spriteMoving0       ; override sprite moving
    sta spriteMoving1
    lda spriteMoving0
    bne spriteManMoving		;	if (spriteMoving0 != false) goto SpriteManMoving

    lda #SPRITE_HEIGHT-1	;	// Sprite is idle
    sta animFrameLineCtr0	;	animFrameLineCtr0 = SPRITE_HEIGHT - 1
    sta animFrameLineCtr1
    jmp endAnimationChecks	;	goto EndAnimationChecks

spriteManMoving:			
    lda animFrameLineCtr0	; Sprite is moving
    cmp #SPRITE_HEIGHT*#NUM_ANIMATION_FACES
    bcs resetFace			; if (animFrameLineCtr0 &gt;= height*numFaces) goto ResetFace

    jmp endAnimationChecks	; else goto EndAnimationChecks

resetFace:
    lda #SPRITE_HEIGHT*#NUM_ANIMATION_FACES-1
    sta animFrameLineCtr0	; animFrameLineCtr0 = (SPRITE_HEIGHT * NUM_ANIMATION_FACES) - 1
    sta animFrameLineCtr1
endAnimationChecks:
    rts




ballSetup:
    lda #%00010000
    sta CTRLPF
    ldx ballHPositionIndex  ;3	|
    lda hPositionTable,x	;4	|
    sta ballHPosition		;3	| ballHPosition = hPositionTable[ballHPositionIndex]
    and #$0F				;2	|
    tax						;2	| x = (hPosition0 & $0F) (coarse position)
    sta WSYNC
positionBall:
    dex						;2	| Position Sprite Horizontally (coarse adj.)
    bne positionBall		;2+	|
    sta RESBL
    lda ballHPosition
    and #$F0
    sta HMBL
    rts


loosMovement0: ; down to right to up to left to....
    lda looDirection0
    cmp #%00000001
    beq moveDownLoo0            ; moveDown
    lda looDirection0
    cmp #%00000010
    beq moveUpLoo0              ; moveUp
    lda looDirection0
    cmp #%00000100
    beq moveLeftLoo0            ; moveLeft
; now we handle the move right case
    ldx hPositionIndex0
    cpx #152
    bcs changeToUp0
    inx
    stx hPositionIndex0
    rts
changeToUp0:
    lda #%00000010
    sta looDirection0
    rts
moveDownLoo0:
    ldx spriteYPosition0
    cpx #10
    bcc changeToRight0
    dex
    stx spriteYPosition0
    rts
changeToRight0: 
    lda #%00001000
    sta looDirection0
    rts
moveUpLoo0:
    ldx spriteYPosition0
    cpx #28
    bcs changeToLeft0
    inx
    stx spriteYPosition0
    rts
changeToLeft0:
    lda #%00000100
    sta looDirection0
    rts
moveLeftLoo0:
    ldx hPositionIndex0
    cpx #1
    bcc changeToDown0
    dex
    stx hPositionIndex0
    rts
changeToDown0:
    lda #%00000001
    sta looDirection0
    rts

loosMovement1: ; down to right to up to left to....
    lda looDirection1
    cmp #%00000001
    beq moveDownLoo1            ; moveDown
    lda looDirection1
    cmp #%00000010
    beq moveUpLoo1              ; moveUp
    lda looDirection1
    cmp #%00000100
    beq moveLeftLoo1            ; moveLeft
; now we handle the move right case
    ldx hPositionIndex1
    cpx #152
    bcs changeToUp1
    inx
    stx hPositionIndex1
    rts
changeToUp1:
    lda #%00000010
    sta looDirection1
    rts
moveDownLoo1:
    ldx spriteYPosition1
    cpx #10
    bcc changeToRight1
    dex
    stx spriteYPosition1
    rts
changeToRight1: 
    lda #%00001000
    sta looDirection1
    rts
moveUpLoo1:
    ldx spriteYPosition1
    cpx #28
    bcs changeToLeft1
    inx
    stx spriteYPosition1
    rts
changeToLeft1:
    lda #%00000100
    sta looDirection1
    rts
moveLeftLoo1:
    ldx hPositionIndex1
    cpx #3
    bcc changeToDown1
    dex
    stx hPositionIndex1
    rts
changeToDown1:
    lda #%00000001
    sta looDirection1
    rts



readJoysticks:
; Move vertically
    ldx ballYPosition
    lda #%00100000	;Down?
    bit SWCHA
    bne skipMoveDown
    cpx #3
    bcc skipMoveDown
    dex
skipMoveDown:
    lda #%00010000	;Up?
    bit SWCHA 
    bne skipMoveUp
    cpx #28
    bcs skipMoveUp
    inx
skipMoveUp:
    stx ballYPosition
; Move horizontally
    ldx ballHPositionIndex
    lda #%01000000	;Left?
    bit SWCHA
    bne skipMoveLeft
    cpx #2
    bcc skipMoveLeft
    dex
skipMoveLeft:
    lda #%10000000	;Right?
    bit SWCHA 
    bne skipMoveRight
    cpx #159
    bcs skipMoveRight
    inx
skipMoveRight:
    stx ballHPositionIndex
    rts

; Manage the frame delay between face animations for loo0
;
animateLoos:
    dec faceDelay0			;	faceDelay -= 1
    lda faceDelay0			;
    beq resetFaceDelay0		;	if (faceDelay == 0) then goto ResetFaceDelay		lda animFrameLineCtr0	;	&lt;-else force another frame of the current face
    lda animFrameLineCtr0	;	&lt;-else force another frame of the current face
    clc						;	by bringing the animFrameLineCtr where
    adc #SPRITE_HEIGHT		;	it was at the start of this frame.
    sta animFrameLineCtr0	;	(i.e. add SPRITE_HEIGHT to it)
    jmp animateLoo1
resetFaceDelay0:
    lda #FACE_DURATION
    sta faceDelay0
    ;	faceDelay = faceDuration
; Manage the frame delay between face animations for loo1
;
animateLoo1:
    dec faceDelay1			;	faceDelay -= 1
    lda faceDelay1			;
    beq resetFaceDelay1		;	if (faceDelay == 0) then goto ResetFaceDelay
    lda animFrameLineCtr1	;	&lt;-else force another frame of the current face
    clc						;	by bringing the animFrameLineCtr where
    adc #SPRITE_HEIGHT		;	it was at the start of this frame.
    sta animFrameLineCtr1	;	(i.e. add SPRITE_HEIGHT to it)
    jmp endFaceStuff
resetFaceDelay1:
    lda #FACE_DURATION
    sta faceDelay1			;	faceDelay = faceDuration
endFaceStuff:
    rts

resetTrack:
    lda #0
    sta trk0idx
    sta pat0idx
    sta pat1idx
    sta chan0dur
    sta chan1dur
    lda #track1-track0
    sta trk1idx
nextPattern:
    ldy trk0idx,x
    lda track0,y
    beq resetTrack
    sta pat0idx,x
    inc trk0idx,x
musicFrame:
    dec chan0dur,x		; decrement note duration
    bpl playNote		; only load if duration < 0
tryAgain:
    ldy pat0idx,x		; load index into pattern table
    lda patterns,y		; load pattern code
    beq nextPattern		; end of pattern?
    inc pat0idx,x		; increment pattern index for next time
    pha			; save A for later
    clc			; clear carry for ROL
    rol
    rol
    rol
    rol			; rotate A left by 4 (same as ROR by 5)
    and #7			; only take top 3 bits
    beq noteTone		; duration zero? tone instruction
    tay			; Y = duration
    lda durFrames,y		; look up in duration table
    sta chan0dur,x		; save note duration
    pla			; pop saved value into A
    and #$1f		; extract first 5 bits
    sta chan0note,x		; store as note value
playNote:
    lda chan0note,x		; get note pitch for channel
    sta AUDF0,x		; store frequency register
    lda chan0dur,x		; get note duration remaining
    clc
    ror			; divide by 2
    cmp #16
    bcc noHighVol
    lda #15			; make sure no greater than 15 (max)
noHighVol:
    sta AUDV0,x		; store volume register
    lda chan0note,X
    bne itWasNotARest
    sta AUDV0,x
itWasNotARest:
    rts
; This routine is called for duration 0 (TONE) codes
noteTone:
    pla
    and #$f
    beq nextPattern
    sta AUDC0,x
    jmp tryAgain


; Adds value to 6-BCD-digit score.
; A = 1st BCD digit
; X = 2nd BCD digit
; Y = 3rd BCD digit
addDeliveryScore subroutine
    sed	; enter BCD mode
    clc	; clear carry
    sta temp
    lda BCDDeliveryScore
    adc temp
    sta BCDDeliveryScore
    stx temp
    lda BCDDeliveryScore+1
    adc temp
    sta BCDDeliveryScore+1
    sty temp
    lda BCDDeliveryScore+2
    adc temp
    sta BCDDeliveryScore+2
    cld	; exit BCD mode
    rts

getDigitPtrs subroutine
    ldx #0	; leftmost bitmap
    ldy #2	; start from most-sigificant BCD value
.loop
    lda BCDDeliveryScore,y	; get BCD value
    and #$f0	; isolate high nibble (* 16)
    lsr		; shift right 1 bit (* 8)
    sta digit0,x	; store pointer lo byte
    lda #>fontTable
    sta digit0+1,x	; store pointer hi byte
    inx
    inx		; next bitmap pointer
    lda BCDDeliveryScore,y	; get BCD value (again)
    and #$f		; isolate low nibble
    asl
    asl
    asl		; * 8
    sta digit0,x	; store pointer lo byte
    lda #>fontTable
    sta digit0+1,x	; store pointer hi byte
    inx
    inx		; next bitmap pointer
    dey		; next BCD value
    bpl .loop	; repeat until < 0
    rts

; Display the resulting 48x8 bitmap
; using the Digit0-5 pointers.
drawDigits subroutine
    sta WSYNC
    SLEEP 40	; start near end of scanline
    lda #7
    sta loopCount
bigLoop
    ldy loopCount	; counts backwards
    lda (digit0),y	; load B0 (1st sprite byte)
    sta GRP0	; B0 -> [GRP0]
    lda (digit1),y	; load B1 -> A
    sta GRP1	; B1 -> [GRP1], B0 -> GRP0
    sta WSYNC	; sync to next scanline
    lda (digit2),y	; load B2 -> A
    sta GRP0	; B2 -> [GRP0], B1 -> GRP1
    lda (digit5),y	; load B5 -> A
    sta temp	; B5 -> temp
    lda (digit4),y	; load B4
    tax		; -> X
    lda (digit3),y	; load B3 -> A
    ldy temp	; load B5 -> Y
    sta GRP1	; B3 -> [GRP1]; B2 -> GRP0
    stx GRP0	; B4 -> [GRP0]; B3 -> GRP1
    sty GRP1	; B5 -> [GRP1]; B4 -> GRP0
    sta GRP0	; ?? -> [GRP0]; B5 -> GRP1
    dec loopCount	; go to next line
    bpl bigLoop	; repeat until < 0    
    lda #0		; clear the sprite registers
    sta GRP0
    sta GRP1
    sta GRP0
    sta GRP1
    rts

; https://www.flickr.com/photos/tokyodrifter/4132540774/in/photolist-7ibmp1-7kwDQr-6drtzM-7i7rqD-fVoZL2-283iFVj-7i7rtP-jYc5Bt-B84WBv-7ibmpY-bZTLow-dEY7Uh-qgio3b-ezNHdC-7i7rrr-7i7TLn-2j4x2vd-nfcDxV-4n8mWY-oMAYTH-dEZXBE-uW2BtA-2i4aDit-nYsFmQ-vo5JMz-5T3mhX-NQZLh3-bBAki7-eZwFDo-cm8FiA-2k3G9o1-qCxgBM-edxNEE-69tfxm-4VoNiK-d3aAQh-oZFiyw-nNrGQY-r63Zzb-BCmnBS-f5sW2P-2d3dWDA-agRe5T-a6uZq8-aNupzD-dRJQVC-6b3nTH-D9as7H-5p8iUR-h1bFAk
    align $100
photo_0:
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %11000000
    .byte %11100000
    .byte %11110000
    .byte %11111000
    .byte %11111110
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %00000000

    align $100

photo_1:
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %10000000
    .byte %11100000
    .byte %11110000
    .byte %11111000
    .byte %11111100
    .byte %11111100
    .byte %11111110
    .byte %11111110
    .byte %11111110
    .byte %11111110
    .byte %11111110
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %00000000

    align $100
photo_2:
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10001000
    .byte %11000001
    .byte %11000001
    .byte %11000000
    .byte %11100001
    .byte %11110001
    .byte %11100000
    .byte %11110000
    .byte %11110000
    .byte %11110010
    .byte %11110000
    .byte %11111000
    .byte %11111000
    .byte %11111000
    .byte %11111000
    .byte %11111000
    .byte %11111100
    .byte %11111000
    .byte %11111100
    .byte %11111101
    .byte %11111110
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %00000000

    align $100
photo_3:
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00001000
    .byte %00001000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000100
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000001
    .byte %00000001
    .byte %00000001
    .byte %00000011
    .byte %00000001
    .byte %00000001
    .byte %00000001
    .byte %00000001
    .byte %00000001
    .byte %00000001
    .byte %00000001
    .byte %01000001
    .byte %00000001
    .byte %00000000
    .byte %00000000
    .byte %00001000
    .byte %00001000
    .byte %00001000
    .byte %00000000
    .byte %00000110
    .byte %00011100
    .byte %00110100
    .byte %00100100
    .byte %00000010
    .byte %00000000
    .byte %00000010
    .byte %00000000
    .byte %00000010
    .byte %00000000
    .byte %00000001
    .byte %00000100
    .byte %10111000
    .byte %00100000
    .byte %00000000
    .byte %00000000
    .byte %11000000
    .byte %11000000
    .byte %11100000
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %00000000

    align $100
photo_4:
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00100000
    .byte %11110000
    .byte %11110000
    .byte %11111000
    .byte %11111000
    .byte %11111000
    .byte %11110000
    .byte %11000100
    .byte %11111100
    .byte %11111100
    .byte %11111110
    .byte %11111010
    .byte %11111110
    .byte %11111110
    .byte %11111110
    .byte %11111110
    .byte %11111110
    .byte %11111110
    .byte %11100110
    .byte %01100111
    .byte %01100011
    .byte %00110011
    .byte %00110111
    .byte %00111111
    .byte %00111111
    .byte %00111111
    .byte %00111111
    .byte %00011111
    .byte %00011110
    .byte %00011100
    .byte %00001000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000011
    .byte %11111111
    .byte %11111111
    .byte %00000000

    align $100
photo_5:
    .byte %00000000
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %01111111
    .byte %00111111
    .byte %00111111
    .byte %00111111
    .byte %00111111
    .byte %00111111
    .byte %00111111
    .byte %00111111
    .byte %00011111
    .byte %00011111
    .byte %00011111
    .byte %00011111
    .byte %00011111
    .byte %00011111
    .byte %00011111
    .byte %00011111
    .byte %00011111
    .byte %00001111
    .byte %00001111
    .byte %00001111
    .byte %00001111
    .byte %00001111
    .byte %00001111
    .byte %00001111
    .byte %00001111
    .byte %00001111
    .byte %00001111
    .byte %00001111
    .byte %10001111
    .byte %10001111
    .byte %10001111
    .byte %10001111
    .byte %10001111
    .byte %10001111
    .byte %10001111
    .byte %10001111
    .byte %10001111
    .byte %00001111
    .byte %00001111
    .byte %00001111
    .byte %00001111
    .byte %00001111
    .byte %00011111
    .byte %00011111
    .byte %00011111
    .byte %00011111
    .byte %00111111
    .byte %00111111
    .byte %01111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %00000000

;---Lookie Loos Graphics Data from PlayerPal 2600--- - https://alienbill.com/2600/playerpalnext.html
llGraphicTable:
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

hPositionTable:
    .byte 				  $34,$24,$14,$04,$F4,$E4,$D4,$C4,$B4,$A4,$94	; 0-10
    .byte $75,$65,$55,$45,$35,$25,$15,$05,$F5,$E5,$D5,$C5,$B5,$A5,$95	; 11-25
    .byte $76,$66,$56,$46,$36,$26,$16,$06,$F6,$E6,$D6,$C6,$B6,$A6,$96	; 26-40
    .byte $77,$67,$57,$47,$37,$27,$17,$07,$F7,$E7,$D7,$C7,$B7,$A7,$97	; 41-55
    .byte $78,$68,$58,$48,$38,$28,$18,$08,$F8,$E8,$D8,$C8,$B8,$A8,$98	; 56-70
    .byte $79,$69,$59,$49,$39,$29,$19,$09,$F9,$E9,$D9,$C9,$B9,$A9,$99	; 71-85
    .byte $7A,$6A,$5A,$4A,$3A,$2A,$1A,$0A,$FA,$EA,$DA,$CA,$BA,$AA,$9A	; 86-100
    .byte $7B,$6B,$5B,$4B,$3B,$2B,$1B,$0B,$FB,$EB,$DB,$CB,$BB,$AB,$9B	; 101-115
    .byte $7C,$6C,$5C,$4C,$3C,$2C,$1C,$0C,$FC,$EC,$DC,$CC,$BC,$AC,$9C	; 116-130
    .byte $7D,$6D,$5D,$4D,$3D,$2D,$1D,$0D,$FD,$ED,$DD,$CD,$BD,$AD,$9D	; 131-145
    .byte $7E,$6E,$5E,$4E,$3E,$2E,$1E,$0E,$FE,$EE,$DE,$CE,$BE,$AE		; 146-159


patterns:
    TONE 0

pattern00:
    TONE 3
    NOTE 16,4
    NOTE 2,4
    NOTE 0,4
    NOTE 0,4
    NOTE 0,4
    NOTE 4,4
    NOTE 30,4
    NOTE 16,4
    TONE 0

pattern10:
    TONE 6
    NOTE 6,4
    TONE 12
    NOTE 16,4
    NOTE 18,4
    NOTE 19,4
    NOTE 22,4
    NOTE 23,4
    NOTE 26,4
    NOTE 23,4
    NOTE 26,4
    NOTE 23,4
    NOTE 26,4
    NOTE 23,4
    NOTE 22,6
    TONE 0
pattern11:
    TONE 6
    NOTE 6,6
    NOTE 6,4
    TONE 12
    NOTE 16,4
    NOTE 18,4
    NOTE 19,4
    NOTE 22,4
    NOTE 23,4
    NOTE 26,4
    NOTE 23,4
    NOTE 26,4
    NOTE 26,4
    NOTE 22,7
    TONE 11
    NOTE 0,7
    NOTE 0,2
    TONE 0
pattern12:
    TONE 11
    NOTE 0,5
    TONE 12
    NOTE 18,5
    NOTE 18,3
    NOTE 18,5
    NOTE 16,6
    NOTE 19,5
    NOTE 22,3
    TONE 6
    NOTE 4,5
    NOTE 4,4
    TONE 12
    NOTE 11,5
    NOTE 11,3
    NOTE 11,5
    NOTE 10,6
    NOTE 10,4
    NOTE 17,3
    NOTE 17,5
    NOTE 16,5
    TONE 0
patternT1:
    TONE 1
    NOTE 0,1
    NOTE 29,2
    NOTE 0,2
    NOTE 29,2
    NOTE 0,4
    NOTE 25,3
    NOTE 0,2
    NOTE 29,4
    NOTE 0,2
    NOTE 29,4
    NOTE 0,2
    NOTE 29,3
    NOTE 0,2
    NOTE 25,3
    NOTE 0,4
    TONE 0

patternT2:
    TONE 1
    NOTE 0,1
    NOTE 29,2
    NOTE 0,2
    NOTE 29,2
    NOTE 0,4
    NOTE 25,3
    NOTE 0,2
    NOTE 29,4
    NOTE 0,2
    NOTE 25,4
    NOTE 0,2
    NOTE 25,3
    NOTE 0,2
    NOTE 25,3
    NOTE 0,4
    TONE 0

patternT3:
    TONE 8
    NOTE 0,4
    NOTE 29,3
    NOTE 0,4
    NOTE 27,7
    NOTE 0,4
    NOTE 26,7
    NOTE 0,4
    NOTE 25,7
    TONE 0

track0:
    PATTERN patternT1
    PATTERN patternT1
    PATTERN patternT1
    PATTERN patternT2
    ENDTRACK
track1:
    PATTERN patternT1
    PATTERN patternT1
    PATTERN patternT1
    PATTERN patternT2
    ENDTRACK
track2:
    PATTERN patternT3
    PATTERN patternT3
    PATTERN patternT3
    PATTERN patternT3
    ENDTRACK

durFrames:
	.byte 0,4,8,12,16,24,32,48
    align $100
fontTable:
    hex 003c6666766e663c007e181818381818
    hex 007e60300c06663c003c66061c06663c
    hex 0006067f661e0e06003c6606067c607e
    hex 003c66667c60663c00181818180c667e
    hex 003c66663c66663c003c66063e66663c

roomStyles:
roomStyle0:
    .byte %00000000
    .byte %01010101
    .byte %10101010
    .byte %11111111
roomStyle1:
    .byte %11011011
    .byte %00000000
    .byte %00000000
    .byte %01000010


    ORG $FFFA
interruptVectors:
    .word reset          ; NMI
    .word reset          ; RESET
    .word reset          ; IRQ
