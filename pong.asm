; Memory mapping data
; 0 - 200 used for sprites

; 300 - 400 used for controller input

;============================================================================
; Includes
;============================================================================

;== Include MemoryMap, Vector Table, and HeaderInfo ==
.INCLUDE "header.inc"

;== Include SNES Initialization routines ==
.INCLUDE "InitSNES.asm"

;============================================================================
; Macros
;============================================================================
;============================================================================
;LoadPalette - Macro that loads palette information into CGRAM
;----------------------------------------------------------------------------
; In: SRC_ADDR -- 24 bit address of source data, 
;     START -- Color # to start on, 
;     SIZE -- # of COLORS to copy
;---------------------------------------------------------------------------- 
; Out: None
;----------------------------------------------------------------------------
; Modifies: A,X
; Requires: mem/A = 8 bit, X/Y = 16 bit
;----------------------------------------------------------------------------
.MACRO LoadPalette
    lda #\2
    sta $2121       ; Start at START color
    lda #:\1        ; Using : before the parameter gets its bank.
    ldx #\1         ; Not using : gets the offset address.
    ldy #(\3 * 2)   ; 2 bytes for every color
    jsr DMAPalette
.ENDM
;============================================================================
; LoadBlockToVRAM -- Macro that simplifies calling LoadVRAM to copy data to VRAM
;----------------------------------------------------------------------------
; In: SRC_ADDR -- 24 bit address of source data
;     DEST -- VRAM address to write to (WORD address!!)
;     SIZE -- number of BYTEs to copy
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: A, X, Y
;----------------------------------------------------------------------------

;LoadBlockToVRAM SRC_ADDRESS, DEST, SIZE
;   requires:  mem/A = 8 bit, X/Y = 16 bit
.MACRO LoadBlockToVRAM
    ldx #\2         ; DEST
    stx $2116       ; $2116: Word address for accessing VRAM.
    lda #:\1        ; SRCBANK
    ldx #\1         ; SRCOFFSET
    ldy #\3         ; SIZE
    jsr LoadVRAM
.ENDM



;============================================================================
; Main Code
;============================================================================

.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"

Start:
    InitSNES    ; Clear registers, etc.

    ; Load Palette for our tiles
    LoadPalette SprPal, 128, 4
 
    ; Load Tile data to VRAM
    LoadBlockToVRAM Sprites, $0000, $0200	; 2 tiles, 4bpp, 32x32 = 200(hex) = 320
    
    jsr SpriteInit
    
;player bat sprite
    lda #(256 - 250)
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

    lda #$0B
    sta $0006

    lda #%01110000
    sta $0007
   
   
    ; Setup Video modes and other stuff, then turn on the screen
    jsr SetupVideo

    lda #$81
    sta $4200       ; enable nmi

    lda #$80
    sta $4212       ; poll controller on vblank
    
    ;ball start speed
    lda #%11000100
    sta $0400

Infinity:
    rep #$30
    jsr BallControll
    jsr ControllerOne
    jmp Infinity    ; bwa hahahahaha


;============================================================================
; SetupVideo -- Sets up the video mode and tile-related registers
;----------------------------------------------------------------------------
; In: None
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
SetupVideo:
    php
    
    rep #$10
    sep #$20
    
    ;stz $2102
    ;stz $2103
    
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

    lda #%00010000            ; Enable BG1
    sta $212C
    
    lda #$0F
    sta $2100           ; Turn on screen, full Brightness

    plp
    rts

;============================================================================

;============================================================================
; LoadVRAM -- Load data into VRAM
;----------------------------------------------------------------------------
; In: A:X  -- points to the data
;     Y     -- Number of bytes to copy (0 to 65535)  (assumes 16-bit index)
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: none
;----------------------------------------------------------------------------
; Notes:  Assumes VRAM address has been previously set!!
;----------------------------------------------------------------------------
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
;============================================================================

;============================================================================
; DMAPalette -- Load entire palette using DMA
;----------------------------------------------------------------------------
; In: A:X  -- points to the data
;      Y   -- Size of data
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: none
;----------------------------------------------------------------------------
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
;TODO: create a dma to do frame rendering for objects    
    lda $0000
    sta $2104
    lda $0001
    sta $2104
    lda $0002
    sta $2104
    lda $0003
    sta $2104
    lda $0004
    sta $2104
    lda $0005
    sta $2104

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
    rts
    
BallControll:
;    and #%00000001
;    beq +
;    rti
;+
;    and #%00000010
;    beq +
;    rti
;+
-
    ;lda $0400
    ;and #%00000100
    ;beq +++

    lda $0400
    cmp #%11000100     ; going up and left?
    bne ++             ; if not next

    lda $0004
    cmp #$00           ; on maximum vertical left?
    bne +              ; if not not skip the flip
 
    lda #%10000000
    trb $0400          ; if on maximum left flip to right
    jmp BallControll
+
    lda $0005          ; load current location on vertical of ball
    cmp #$00           ; is the maximum horizontal limit found?
    bne +              ; if not skip the flip
     
    lda #%10000100     ; on maximum height flip direction to going down
    sta $0400
    jmp BallControll   ; recursive function of yourself       
+
                       ; not on maximum hight or max to the left go up and left  
    dec $0410          
    dec $0410
    dec $0410
    dec $0415
    dec $0415   
    dec $0415
    jmp BallRender

++
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
;+++
;rts
    
;++
;    rts
;    and #%00001000
;    beq +
;    rti
;+
;    and #%00010000
;    beq +
;    rti
;+

BallRender:
    lda $0411
    cmp #$02
    bne +
    inc $0004
    stz $0411
+
    lda $0411
    cmp #$FE
    bne +
    dec $0004
    stz $0411
+
    lda $0416
    cmp #$02
    bne +
    inc $0005
    stz $0416
+
    lda $0416
    cmp #$FE
    bne +
    dec $0005
    stz $0416
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
.ENDS
    
