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
    
    lda #(256 - 250)
    sta $0000       ; sprite x coordination

    ;lda #(224/2 -16)
    lda #30
    sta $0001       ; sprite y coordination

    stz $0002

    lda #%01110000
    sta $0003
    
    lda #%01010100
    sta $0200

    
   
    ; Setup Video modes and other stuff, then turn on the screen
    jsr SetupVideo

    lda #$80
    sta $4200       ; enable nmi

    lda #$81
    sta $4212       ; poll controller on vblank

Infinity:
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
    
    stz $2102
    stz $2103
    
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

    stz $4300  ; Set DMA Mode (byte, normal increment)
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


VBlank:

    sep #$10
    rep #$20

    lda $4212
    and #$01            
    bne VBlank      ; if controller input is not ready do not continue

    ldy #$0001      ; memory position in oam for sprite 1

    lda $4219
    sta $0300       ; record the buttons pressed

    and #%00001000  ; is up pressed?
    beq +           ; if not go to +

    lda $0001
    clc
    adc #$01
    sta $0001      ; store position back to right ram position
    
    sty $2102
    stz $2103      ; set oam write to 0001 the y of main
    sta $2104      ; write new y coordinate to position
+
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
    
