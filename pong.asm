;TODO: write good documentation with nice format.
.INCLUDE "header.inc"

.INCLUDE "InitSNES.asm"

.MACRO LoadPalette
    lda #\2
    sta $2121       ; Start at START color
    lda #:\1        ; Using : before the parameter gets its bank.
    ldx #\1         ; Not using : gets the offset address.
    ldy #(\3 * 2)   ; 2 bytes for every color
    jsr DMAPalette
.ENDM


.MACRO LoadBlockToVRAM
    ldx #\2         ; DEST
    stx $2116       ; $2116: Word address for accessing VRAM.
    lda #:\1        ; SRCBANK
    ldx #\1         ; SRCOFFSET
    ldy #\3         ; SIZE
    jsr LoadVRAM
.ENDM


.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"

Start:
    InitSNES    ; Clear registers, etc.

    ; Load Palette for our tiles
    LoadPalette SprPal, 128, 4
    
    ; Load Palette for background tiles
    LoadPalette BackPal, 0, 4
 
    LoadBlockToVRAM BackOne, $1000, $40         ; 2 tukes 4bpp, 8x8 = 20 hex
 
    ; Load Tile data to VRAM
    LoadBlockToVRAM Sprites, $0000, $0800	; 2 tiles, 4bpp, 32x32 = 200(hex) = 320 wrong math i need much more (32x32 is more than 8x8)
    
    jsr SpriteInit
    
;player bat sprite
    lda #(256 - 255)
    sta $0000       ; sprite x coordination

    lda #(224/2 -16) 
    sta $0001       ; sprite y coordination

    stz $0002

    lda #%01110000
    sta $0003
    
    lda #%01010000
    sta $0200
   
    
;ball sprite    
    lda #(256/2 -16)
    sta $0004       ; sprite x coordination

    lda #(224/2 -16) 
    sta $0005       ; sprite y coordination

    lda #$04
    sta $0006

    lda #%01110000
    sta $0007
   
   
    ; Setup Video modes and other stuff, then turn on the screen
    jsr SetupBGOne
    jsr SetupVideo
    

    lda #$81
    sta $4200       ; enable nmi

    lda #$80
    sta $4212       ; poll controller on vblank
    
    ; Ball start direction
    lda #%11000100
    sta $0400

Infinity:
    rep #$30
    jsr BallControll
    jsr ControllerOne
    jmp Infinity    ; bwa hahahahaha

SetupBGOne:   
    lda #$80       ;increment on 2119 (16 bit writes)
    sta $2115
    ldx #$0480     ;ram location 800(starting thus tile0 of bg1) plus 4 lines 
    stx $2116
    .rept 32
        ldx #$0002 ; one line
        stx $2118 
    .endr
    ldx #$06E0     ; 20 lines down to make a centered field
    stx $2116
    .rept 32
        ldx #$0002 ;again one line
        stx $2118
    .endr
    rts    

SetupVideo:
    php
    
    rep #$10
    sep #$20
    stz $2105               ; backgrounds to 8x8 mode 
    stz $2105
    lda #%00000100          ; background 1 = 32x32 in size and starts at 2k
    sta $2107
    lda #$01
    sta $210b
    
    ;*********transfer sprite data

	stz $2102		; set OAM address to 0
	stz $2103

	LDY #$0400
	STY $4300		; CPU -> PPU, auto increment, write 1 reg, $2104 (OAM Write)
	stz $4302
	stz $4303		; source offset
	LDY #$0220
	STY $4305		; number of bytes to transfer
	LDA #$7E
	STA $4304		; bank address = $7E  (work RAM)
	LDA #$01
	STA $420B		;start DMA transfer
	
	lda #%10100000
    sta $2101

    lda #%00010001            ; Enable BG1 and sprites
    sta $212C
    
    lda #$0F
    sta $2100           ; Turn on screen, full Brightness

    plp
    rts

LoadVRAM:
    stx $4302   ; Store Data offset into DMA source offset
    sta $4304   ; Store data Bank into DMA source bank
    sty $4305   ; Store size of data block

    lda #$01
    sta $4300   ; Set DMA mode (word, normal increment)
    lda #$18    ; Set the destination register (VRAM write register)
    sta $4301
    lda #$01    ; Initiate DMA transfer (channel 1)
    sta $420B

    rts         ; return

DMAPalette:
    stx $4302   ; Store data offset into DMA source offset
    sta $4304   ; Store data bank into DMA source bank
    sty $4305   ; Store size of data block

    stz $4300   ; Set DMA Mode (byte, normal increment)
    lda #$22    ; Set destination register ($2122 - CGRAM Write)
    sta $4301
    lda #$01    ; Initiate DMA transfer
    sta $420B

    rts         ; return from subroutine

SpriteInit:
    php	

    rep	#$30	;16bit mem/A, 16 bit X/Y
	
    ldx #$0000
    lda #$0001
_setoffscr:
    sta $0000,X
    inx
    inx
    inx
    inx
    cpx #$0200
    bne _setoffscr

    ldx #$0000
    lda #$5555
_clr:
    sta $0200, X   ; Initialize all sprites to be off the screen
    inx
    inx
    cpx #$0020
    bne _clr

    plp
    rts

NewFrameRender:

    LDY #$0400
    STY $4300		; CPU -> PPU, auto increment, write 1 reg, $2104 (OAM Write)
    stz $4302
    stz $4303		; source offset
    LDY #$0220
    STY $4305		; number of bytes to transfer
    LDA #$7E
    STA $4304		; bank address = $7E  (work RAM)
    LDA #$01
    STA $420B		;start DMA transfer

    rts

ControllerOne:

    lda $4212
    and #$01
    bne ControllerOne  ; if controller input is not ready do not continue
    lda $4219
    sta $0300          ; record the buttons pressed

    and #%00001000     ; is up pressed?
    beq +           
    lda $0001          ; is it the sprite equal to the top of the (pong)board?
    cmp #$00
    beq +          

    dec $0420          ; if not decrement 3 times
    dec $0420
    dec $0420
+
    lda $4219
    and #%00000100     ; is down pressed?
    beq +

    lda $0001          ; is the sprite equal to the bottom of the (pong)board?
    cmp #$D4
    beq +
   
    inc $0420          ; if not increment 3 times
    inc $0420
    inc $0420
+   
    lda $0421          ; 
    cmp #$02           ; this is true if FF +1 happends to 0420 
    bne +
    inc $0001          ; go down by one
    stz $0421          ; reset to 0
+
    lda $0421
    cmp #$FE           ; this is true if 0420 = 0 and then -1
    bne +
    dec $0001          ; go up by one
    stz $0421          ; reset to 0
+
    lda $4219
    cmp #$00           ; check if no buttons are pressed
    bne +              ; if not skip

    stz $0420          ; store 0 into $0420 to prevent bugmode
    stz $0421          ; store 0 into $0421 to prevent bugmode
+
    rts
;TODO fix issue where ram becomes x5, possibly hardcode the flips for all flips :(
FlipBall:
    rep #$20
    lda #$0000         ; going into 8 bit mode 
    sep #$20
    
    lda $0005          ; load current location on vertical of ball
    cmp #$00           ; is the maximum horizontal limit found?
    bne +              ; if not skip the flip
     
    lda #%01000000     ; on maximum height flip direction to going down
    trb $0400          ; trb is used to clear bit
+
    lda $0005
    cmp #$D4
    bne +

    lda #%01000000     ; on minimum height flip direction to going up
    tsb $0400          ; tsb is used to set bit
+
    lda $0004
    cmp #$00           ; on maximum vertical left?
    bne +              ; if not not skip the flip
     
    lda #%10000000
    trb $0400          ; if on maximum left flip to right 
+
    lda $0004          ; on maximum vertical right?
    cmp #$F0           
    bne +              ; if not skip the flip
    
    lda #%10000000     ; if on maximum right flip to left
    tsb $0400
+
    rep #$20           ; back to 16 bit a
    rts


BallControll:
;TODO create sequinces for different directions (---xxxxx) this one is only for 00000100
    jsr FlipBall 
   
                       ; not on maximum hight or max to the left go up and left  
    lda $0400
    cmp #%11000100     ; going up and left?
    bne +              ; if not next

    dec $0410          
    dec $0410
    dec $0410
    dec $0415
    dec $0415   
    dec $0415
    jmp BallRender

+
    lda $0400
    and #%10000000     ; going down and left?
    beq +              ; if not next

    dec $0410          
    dec $0410
    dec $0410
    inc $0415
    inc $0415   
    inc $0415
    jmp BallRender  
+    
    lda $0400
    and #%01000000     ; going down and right?
    beq +              ; if not next

    inc $0410          
    inc $0410
    inc $0410
    dec $0415
    dec $0415   
    dec $0415
    jmp BallRender
    
+                      ; that leaves going down right
    inc $0410          
    inc $0410
    inc $0410
    inc $0415
    inc $0415   
    inc $0415
    jmp BallRender    


BallRender:
    lda $0411         ; if $0410 goes over FF $0411 will go up with 1
    cmp #$02          ; check if $0411 is 2
    bne +             ; if not skip
    inc $0004         ; if move ball one left
    stz $0411         ; reset $0411
+
    lda $0411         
    cmp #$FE          ; check if $0411 is FE (-2)
    bne +             ; if not skip
    dec $0004         ; if move ball one right
    stz $0411         ; reset $0411
+
    lda $0416         ; if $0415 goes over FF $0416 will go up with 1
    cmp #$02          ; check if 0416 is 2
    bne +             ; if not skip

    inc $0005         ; if ball move down
    stz $0416         ; reset $0416
+
    lda $0416         
    cmp #$FE          ; check if $0416 is FE (-2)
    bne +             ; if not skip

    dec $0005         ; if ball move up
    stz $0416         ; reset $0416
+
    rts
 
VBlank:
    rep #$30 
    jsr NewFrameRender
    rti

.ENDS    
    
    
    
;============================================================================
; Character Data
;============================================================================
.BANK 1 SLOT 0
.ORG 0
.SECTION "CharacterData"
SprPal:
    .INCBIN "pong.clr"
Sprites:
    .INCBIN "pong.pic"
BackOne:
    .INCBIN "back1.pic"
BackPal:
    .INCBIN "back1.clr"
.ENDS
    
