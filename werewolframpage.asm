.segment "ZEROPAGE"
	.struct Entity
		id     		.byte
		xposlo 		.byte
		xposhi 		.byte
		ypos   		.byte
		spritePtr 	.byte
	.endstruct

	temp:		 .res 1
	tempx:		 .res 1
	tempy:		 .res 1
	currentRoom:   .res 1
	keyRoom:   .res 1

	MAXENTITIES = 6
	entities: .res .sizeof(Entity) * MAXENTITIES
	TOTALENTITIES = .sizeof(Entity) * MAXENTITIES
	FURNATURESTRIDE = 4*5
	TIMERSPEED = 126
	PTIME = 9
	WWTIME = 6
	
	borderTop: .res 1
	borderBot: .res 1
	borderLeft: .res 1
	borderRight: .res 1
	
	timer: .res 1
	timer2: .res 1
	keyDropped: .res 1
	mode: .res 1
	walkCycle: .res 1

.segment "CODE"
	JMP @start
	
.segment "CODE2"
	.byte $02, $5A, $0, $98 , $30
	.byte $03, $18, $01, $8C , $32
	.byte $04, $7C, $0, $96 , $31
	.byte $05, $32, $00, $8C , $2F
	
	.byte $02, $32, $0, $98 , $30
	.byte $03, $18, $01, $96 , $32
	.byte $04, $7D, $0, $96 , $31
	.byte $05, $C8, $00, $8C , $2F
	
	.byte $02, $32, $0, $8C , $2F
	.byte $03, $18, $01, $9A , $31
	.byte $04, $7D, $0, $9C , $32
	.byte $05, $C8, $00, $98 , $2F
	
	.byte $02, $5A, $0, $98 , $30
	.byte $03, $18, $01, $8C , $31
	.byte $04, $7C, $0, $96 , $32
	.byte $05, $32, $00, $8C , $32
	
@start:
		;setup
		lda #$00
		sta $c6
		sta keyDropped
		lda #$01
		sta currentRoom
		lda #$7D
		sta borderTop
		lda #$C8
		sta borderBot
		lda #$18
		sta borderLeft
		lda #$40
		sta borderRight
		lda #$00
		sta mode
		lda #TIMERSPEED
		sta timer
		lda #PTIME
		sta timer2
		
		
		lda #$00
		sta entities+Entity::id ;id		
		lda #$80
		sta entities+Entity::xposlo ;set player pos x
		lda #$00
		sta entities+Entity::xposhi ;set enemy pos x
		lda #$78
		sta entities+Entity::ypos ;set player pos y
		lda #$29
		sta entities+Entity::spritePtr ;human sprite
		
		LDX #.sizeof(Entity)
		
		lda #$01
		sta entities+Entity::id,x ;id		
		lda #$80
		sta entities+Entity::xposlo,x ;set player pos x
		lda #$00
		sta entities+Entity::xposhi,x ;set enemy pos x
		lda #$78
		sta entities+Entity::ypos,x ;set player pos y
		lda #$28
		sta entities+Entity::spritePtr,x ;human sprite
		
		JSR @EnterRoom

		JSR @DrawBG
        ; set to 25 line text mode and turn on the screen
        lda #$1B
        sta $D011

        ; disable SHIFT-Commodore
        lda #$80
        sta $0291

        ; set screen memory ($0400) and charset bitmap offset ($2000)
        lda #$18
        sta $D018

        ; set border color
		lda #$0B
        sta $D020
        
        ; set background color
        lda #$00
        sta $D021
		
		; set sprite multicolors
        lda #$02
        sta $d025
        lda #$06
        sta $d026
		
		; colorize sprites
		lda #$02
		sta $d027
		lda #$07
		sta $d028
		lda #$03
		sta $d029
		lda #$0F
		sta $d02A
		lda #$0E
		sta $d02B
		lda #$0F
		sta $d02C
		lda #$08
		sta $d02D
		lda #$08
		sta $d02E

        ; expand sprites
        lda #$00
        sta $d01d
        lda #$C0
        sta $d017
		
        ; wall sprites
        lda #$33
		sta $07FE
        lda #$33
		sta $07FF
		
		; positioning wall sprites
		lda #$48
		sta $D00C	; #0. sprite X low byte
		lda #$A2
		sta $D00D	; #0. sprite Y
		
		lda #$10
		sta $D00E	; #1. sprite X low byte
		lda #$A2
		sta $D00F	; #1. sprite Y
		
        ; set multicolor flags
        lda #$00
        sta $d01c

        ; set screen-sprite priority flags
        lda #$00
        sta $d01b

;;;;;;;;;;;;;;;;;;;;;;;;MAIN LOOP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@mainloop:
		JSR @UpdateScreen
		DEC timer
		BNE @continueMainLoop
		JSR @TickTimer
@continueMainLoop:	
		JSR @readjoysitck
		LDA temp
		CMP #$01
		BEQ @continueMainLoop1
		LDA mode	;;Btn up
		CMP #$01
		BNE @collisionCheckDone 
		LDA #$02
		STA mode
		jmp @continueMainLoop2
@continueMainLoop1:
		LDA mode
		CMP #$02
		BNE @continueMainLoop2
		JSR @TransToWerewolf		
@continueMainLoop2:	
		JSR @collisionCheck
		CPX #TOTALENTITIES
		BEQ @collisionCheckDone
		JSR @dropKey
@collisionCheckDone:
		JSR @bounceTop
        LDA #$ff
@wait_raster:
        CMP $d012
        BNE @wait_raster
		JSR @SpriteStart
		JMP @mainloop
		
;;;;;;;;;;;;;;;;;;;;;;;;END MAIN LOOP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;SUB ROUTINES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@checkKey:
		LDY #.sizeof(Entity)
		LDA entities+Entity::xposlo,x
		CMP entities+Entity::xposlo,Y
		BNE @checkKeyFail
		LDA entities+Entity::xposhi,x
		CMP entities+Entity::xposhi,Y
		BNE @checkKeyFail
		LDA entities+Entity::ypos,x
		CMP entities+Entity::ypos,Y
		BNE @checkKeyFail
		LDA currentRoom
		CMP keyRoom
		BNE @checkKeyFail
		JMP @win
	@checkKeyFail:
		RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@dropKey:
		LDA mode
		CMP #$03
		Bne @continueDrop
		JSR @checkKey
		RTS
	@continueDrop:
		LDY #.sizeof(Entity)
		LDA entities+Entity::xposlo,x
		STA entities+Entity::xposlo,Y
		LDA entities+Entity::xposhi,x
		STA entities+Entity::xposhi,Y
		LDA entities+Entity::ypos,x
		STA entities+Entity::ypos,Y
		LDA currentRoom
		STA keyRoom
		LDA #$01
		STA mode
		STA keyDropped
		RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@DrawBG:
; draw screen
		
        lda #$00
        sta $fb
        sta $fd
        sta $f7

        lda #$28        ; $2800 in $FB + $FC - source of screen data
        sta $fc

        lda #$04        ; $0400 in $FD + $FE - Screen memory
        sta $fe

        lda #$e8        ; $2be8 in $F9 + $FA - Source of color data
        sta $f9
        lda #$2b
        sta $fa

        lda #$d8        ; $d800 in $F7 + $F8 - Color memory
        sta $f8

        ldx #$00
@loop:
        ldy #$00
@innerloop:
        lda ($fb),y
        sta ($fd),y
        lda ($f9),y
        sta ($f7),y
        iny
        bne @innerloop

        inc $fc
        inc $fe
        inc $fa
        inc $f8
        inx
		CPX #$04
		BNE @loop
		RTS
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@UpdateScreen:
		LDA timer2
		CLC
		ADC #$30
		STA $7aa	
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@TransToWerewolf:
		LDA mode
		CMP #$03
		BNE @ContinueTrans
		JMP @lose
	@ContinueTrans:
		LDA keyDropped
		CMP #$01
		BNE @win
		LDA #$03
		STA mode
		lda #$2C
		sta entities+Entity::spritePtr		
		lda #$08
		sta $d027
		lda #WWTIME
		sta timer2
		rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@TickTimer:
		lda TIMERSPEED
		sta timer
		dec timer2
		bne @continuetick
		JSR @TransToWerewolf
	@continuetick:
		RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@win:

	LDA #$10 ;p
	STA $7AC
	
	LDA #$32
	STA $7AD ;1
	
	LDA #$17
	STA $7AF ;W
	
	LDA #$9
	STA $7B0 ;i
	
	LDA #$E
	STA $7B1 ;n
	
	
	; wait for keypress
	lda $c6
	beq *-2
	jmp @start
	
	
@lose:

	LDA #$10 ;p
	STA $7AC
	
	LDA #$31
	STA $7AD ;2
	
	LDA #$17
	STA $7AF ;W
	
	LDA #$9
	STA $7B0 ;i
	
	LDA #$E
	STA $7B1 ;n
	
	; wait for keypress
	lda $c6
	beq *-2
	jmp @start


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@bounceTop:
		lda entities+Entity::ypos
		CMP borderTop
		BCS @bounceBot
		inc entities+Entity::ypos
@bounceBot:
		lda entities+Entity::ypos
		CMP borderBot
		BCC @bounceLeft
		dec entities+Entity::ypos
@bounceLeft:
		lda entities+Entity::xposhi
		bne @bounceRight	
		lda entities+Entity::xposlo
		CMP borderLeft
		bcs @bounceRight		
		inc entities+Entity::xposlo
		lda currentRoom
		CMP #$00
		BEQ @bounceRight
		dec currentRoom
		LDA #$3C
		STA entities+Entity::xposlo
		LDA #$01
		STA entities+Entity::xposhi
		JSR @EnterRoom
@bounceRight:
		lda entities+Entity::xposhi
		beq @doneBounce	
		lda entities+Entity::xposlo
		CMP borderRight
		bcc @doneBounce	
        
		dec entities+Entity::xposlo
        ldy entities+Entity::xposlo
        cpy #$ff
        bne @doorCheck
		LDA #$01
		STA entities+Entity::xposhi
	@doorCheck:
		lda currentRoom
		CMP #$03
		BEQ @doneBounce
		inc currentRoom
		LDA #$1A
		STA entities+Entity::xposlo
		LDA #$00
		STA entities+Entity::xposhi
		JSR @EnterRoom
@doneBounce:
		rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@EnterRoom:
		LDX #$00
		LDY #$00
		LDA currentRoom
		CMP #$00
		BEQ @EndYLoad
	@StartYLoad:
		TYA
		CLC
		ADC #FURNATURESTRIDE 
		TAY
		INX
		CPX currentRoom
		BNE @StartYLoad
	@EndYLoad:
		LDX #.sizeof(Entity) *2
	@RoomELoop:
		lda $00C000,y
		sta entities+Entity::id,x ;id	
		INY
		lda $00C000,y
		sta entities+Entity::xposlo,x ;set enemy pos x
		INY
		lda $00C000,y
		sta entities+Entity::xposhi,x ;set enemy pos x
		INY
		lda $00C000,y
		sta entities+Entity::ypos,x ;set enemy pos y
		INY
		lda $00C000,y
		sta entities+Entity::spritePtr,x ;human sprite
		INY
		TXA
		CLC
		ADC #.sizeof(Entity)
		TAX
		CPX #TOTALENTITIES
		BNE @RoomELoop
		LDA currentRoom
		CMP #$00
		BEQ @MakeLeftDoor
		LDA currentRoom
		CMP #$03
		BEQ @MakeRightDoor
		jmp @MakeNoDoor
	@MakeLeftDoor:
        lda #$BD
        sta $d015
		RTS
	@MakeRightDoor:
        lda #$7D
        sta $d015
		RTS
	@MakeNoDoor:
        lda #$3D
        sta $d015
		RTS
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;				
		
@readjoysitck:
		CLV 
		lda #00
		sta temp
		lda #00
		sta tempx
        lda 56320
@up:
        lsr a
        bcs @down
        dec entities+Entity::ypos
		inc tempx
@down:
        lsr a
        bcs @left
        inc entities+Entity::ypos
		inc tempx
@left:
        lsr a
        bcs @right
        dec entities+Entity::xposlo
		inc tempx
        ldy entities+Entity::xposlo
        cpy #$ff
        bne @right
        dec entities+Entity::xposhi
@right:
        lsr a
        bcs @fire
		inc tempx
        inc entities+Entity::xposlo
        bne @fire   
        inc entities+Entity::xposhi
@fire:
        lsr a
		bcs @doneInput
		lda #$01
		sta temp
@doneInput:   
		lda tempx
		BEQ @doneInput2
		LDA walkCycle
		adc #$05
		STA walkCycle
		CMP #$3C
		BCC @doneInput2
		LDA #$00
		STA walkCycle
		inc entities+Entity::spritePtr
		LDA mode
		CMP #$03
		Beq @LoopWWAnimation
		
	@LoopPlayerAnimation:
		LDA entities+Entity::spritePtr
		CMP #$2C
		BCS @rollbackAnim
		jmp @doneInput2
	@LoopWWAnimation:
		LDA entities+Entity::spritePtr
		CMP #$2F
		BCS @rollbackAnim
		jmp @doneInput2
	
	@rollbackAnim:
		DEC entities+Entity::spritePtr
		DEC entities+Entity::spritePtr
		DEC entities+Entity::spritePtr
@doneInput2:
		RTS
		
		
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
		
@collisionCheck:
		ldx #.sizeof(Entity)*2
@checktop:
		lda entities+Entity::ypos
		clc
		ADC #$10
		CMP entities+Entity::ypos,x
		Bcs @checkbot
		jmp @colldone
@checkbot:
		lda entities+Entity::ypos
		clc
		SBC #$10
		CMP entities+Entity::ypos,x
		Bcc @checkright
		jmp @colldone
@checkright:
		lda entities+Entity::xposhi,x
		CMP entities+Entity::xposhi
		bne @colldone
		lda entities+Entity::xposlo
		clc
		ADC #$10
		CMP entities+Entity::xposlo,x
		Bcs @checkleft
		jmp @colldone
@checkleft:
		lda entities+Entity::xposhi,x
		CMP entities+Entity::xposhi
		bne @colldone
		lda entities+Entity::xposlo
		clc
		SBC #$10
		CMP entities+Entity::xposlo,x
		Bcc @collisionhit
		jmp @colldone
@collisionhit:	
        ; set background color
        TXA
		rts
@colldone:
		CPX #TOTALENTITIES
        beq @collreturn
		TXA
		CLC
		ADC #.sizeof(Entity)
		TAX
        jmp @checktop
@collreturn:
		rts
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;END Subroutines;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;;;;;;;;;;;;;;;;;;;;;SPRITE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
		
@SpriteStart:		
        ; set sprite pointers
		LDX #$00
		LDY #$00
	@ptrs:
		LDA entities+Entity::spritePtr,x
        STA $07F8,y
		INY
		CPX #TOTALENTITIES - .sizeof(Entity)
        beq @ptrsDone
		TXA
		CLC
		ADC #.sizeof(Entity)
		TAX
        jmp @ptrs
	@ptrsDone:
		LDX #$00
		LDY #$00
        lda #$40
        sta $D010
        lda #$00
		sta temp
		sta tempx
		sta tempy
		LDA $07F8
	
@spritePosloop:
		LDA entities+Entity::xposhi,x
		CMP #$02
		BCC @tempfixDone
		LDA #$01
		STA entities+Entity::xposhi,x
	@tempfixDone:
		STA tempy
		LDA #0
		STA tempx
	@rotate:
		LDA tempx
		CMP entities+Entity::id,x
		Beq @doneRotate
		LDA tempy
		ROL  A
		STA tempy
		LDA tempx
		CLC
		ADC #$01
		STA tempx
		jmp @rotate
	@doneRotate:
		LDA $D010
		ORA tempy
		STA $D010
	@xloy:		
        lda entities+Entity::xposlo,x
        sta $d000,y
		iny
        lda entities+Entity::ypos,x
        sta $d000,y
		iny
		CPX #TOTALENTITIES - .sizeof(Entity)
        beq @SpritesDone
        TXA
		CLC
		ADC #.sizeof(Entity)
		TAX
        jmp @spritePosloop
@SpritesDone:
		RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END SPRITE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "SPRITE";
	; Key Placeholder
	.byte $00, $00, $00
	.byte $00, $00, $00
	.byte $00, $00, $00
	.byte $00, $00, $00
	.byte $00, $78, $00
	.byte $00, $FC, $00
	.byte $01, $86, $00
	.byte $01, $86, $00
	.byte $01, $86, $00
	.byte $01, $86, $00
	.byte $00, $FC, $00
	.byte $00, $78, $00
	.byte $00, $30, $00
	.byte $00, $30, $00
	.byte $00, $70, $00
	.byte $00, $70, $00
	.byte $00, $30, $00
	.byte $00, $F0, $00
	.byte $00, $F0, $00
	.byte $00, $00, $00
	.byte $00, $00, $00
	.byte 0


	; person
    .byte $00, $38, $00
    .byte $00, $7C, $00
    .byte $00, $D6, $00
    .byte $00, $D6, $00
    .byte $00, $7C, $00
    .byte $00, $38, $00
    .byte $00, $38, $00
    .byte $03, $FF, $C0
    .byte $07, $FF, $E0
    .byte $06, $FF, $60
    .byte $06, $FF, $60
    .byte $06, $FF, $60
    .byte $0E, $FF, $70
    .byte $06, $FF, $60
    .byte $00, $E7, $00
    .byte $00, $C3, $00
    .byte $00, $C3, $00
    .byte $00, $C3, $00
    .byte $00, $C3, $00
    .byte $00, $C3, $00
    .byte $00, $F3, $C0
	.byte 0
	;person walk 1
	.byte $00, $00, $00
	.byte $00, $3C, $00
	.byte $00, $7E, $00
	.byte $00, $DB, $00
	.byte $00, $DB, $00
	.byte $00, $7E, $00
	.byte $00, $18, $00
	.byte $00, $18, $00
	.byte $03, $FF, $C0
	.byte $07, $FF, $E0
	.byte $06, $FF, $60
	.byte $06, $FF, $60
	.byte $06, $FF, $70
	.byte $06, $FF, $60
	.byte $0E, $FF, $00
	.byte $06, $E7, $00
	.byte $00, $C3, $00
	.byte $01, $83, $00
	.byte $01, $81, $80
	.byte $01, $81, $E0
	.byte $01, $E0, $00
	.byte 0

	;person walk 2
	.byte $00, $3C, $00
	.byte $00, $7E, $00
	.byte $00, $DB, $00
	.byte $00, $DB, $00
	.byte $00, $7E, $00
	.byte $00, $18, $00
	.byte $00, $18, $00
	.byte $03, $FF, $C0
	.byte $07, $FF, $E0
	.byte $06, $FF, $60
	.byte $06, $FF, $60
	.byte $06, $FF, $60
	.byte $0E, $FF, $60
	.byte $06, $FF, $70
	.byte $00, $E7, $60
	.byte $00, $C3, $00
	.byte $00, $C3, $00
	.byte $07, $C3, $00
	.byte $07, $81, $80
	.byte $04, $01, $80
	.byte $04, $01, $E0
	.byte 0
	
	;Werewolf
	.byte $00, $1F, $80
	.byte $00, $3A, $80
	.byte $00, $3A, $FE
	.byte $00, $1F, $55
	.byte $00, $1F, $00
	.byte $00, $7F, $AA
	.byte $00, $FC, $FE
	.byte $00, $FE, $00
	.byte $00, $7F, $00
	.byte $00, $7F, $10
	.byte $00, $FF, $E8
	.byte $00, $FF, $E0
	.byte $00, $FF, $38
	.byte $01, $FF, $00
	.byte $03, $FF, $00
	.byte $37, $E7, $00
	.byte $7E, $63, $00
	.byte $7E, $63, $00
	.byte $3C, $63, $00
	.byte $18, $73, $80
	.byte $00, $73, $80
	.byte 0
	
	;Werewolf walk 1
	.byte $00, $00, $00
	.byte $00, $1F, $80
	.byte $00, $3A, $80
	.byte $00, $3A, $FE
	.byte $00, $1F, $55
	.byte $00, $1F, $00
	.byte $00, $7F, $AA
	.byte $00, $FC, $FE
	.byte $00, $FE, $00
	.byte $00, $7F, $00
	.byte $00, $7F, $00
	.byte $00, $FF, $90
	.byte $70, $FF, $E8
	.byte $78, $FF, $60
	.byte $FF, $FF, $38
	.byte $3F, $E7, $00
	.byte $0F, $E7, $00
	.byte $00, $63, $00
	.byte $00, $C3, $80
	.byte $00, $E3, $80
	.byte $00, $E0, $00
	.byte 0

	;Werewolf walk 2
	.byte $00, $1F, $80
	.byte $00, $3A, $80
	.byte $00, $3A, $FE
	.byte $00, $1F, $55
	.byte $00, $1F, $00
	.byte $00, $7F, $AA
	.byte $00, $FC, $FE
	.byte $00, $FE, $00
	.byte $20, $7F, $00
	.byte $70, $7F, $10
	.byte $78, $FF, $E8
	.byte $7C, $FF, $E0
	.byte $7F, $FF, $38
	.byte $3F, $FF, $00
	.byte $0F, $FF, $00
	.byte $00, $67, $00
	.byte $00, $63, $00
	.byte $00, $63, $00
	.byte $07, $C3, $80
	.byte $07, $81, $C0
	.byte $06, $01, $C0
	.byte 0
	
; Furnature start
    .byte $00, $00, $00
    .byte $00, $18, $00
    .byte $00, $66, $00
    .byte $00, $A5, $00
    .byte $01, $24, $80
    .byte $01, $24, $80
    .byte $01, $FF, $80
    .byte $01, $66, $80
    .byte $01, $24, $80
    .byte $01, $3C, $80
    .byte $01, $FF, $80
    .byte $01, $66, $80
    .byte $01, $24, $80
    .byte $03, $FF, $C0
    .byte $03, $FF, $C0
    .byte $03, $FF, $C0
    .byte $02, $42, $40
    .byte $02, $42, $40
    .byte $02, $42, $40
    .byte $02, $42, $40
    .byte $02, $00, $40
	.byte 0

; Sprite #10
; Single color mode, BG color: 6, Sprite color: 9
    .byte $00, $FF, $00
    .byte $03, $00, $C0
    .byte $06, $FF, $60
    .byte $0D, $E7, $B0
    .byte $0B, $E7, $D0
    .byte $0B, $E7, $D0
    .byte $0D, $E7, $B0
    .byte $05, $E7, $A0
    .byte $05, $E7, $A0
    .byte $05, $E7, $A0
    .byte $05, $E7, $A0
    .byte $05, $E7, $A0
    .byte $05, $E7, $A0
    .byte $05, $E7, $A0
    .byte $05, $FF, $A0
    .byte $04, $00, $20
    .byte $07, $FF, $E0
    .byte $07, $FF, $E0
    .byte $04, $24, $20
    .byte $04, $24, $20
    .byte $04, $00, $20
	.byte 0

; Sprite #11
; Single color mode, BG color: 6, Sprite color: 9
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $3F, $FF, $FC
    .byte $20, $00, $04
    .byte $3F, $FF, $FC
    .byte $3F, $FF, $FC
    .byte $33, $81, $CC
    .byte $21, $00, $84
    .byte $21, $00, $84
    .byte $21, $00, $84
    .byte $21, $00, $84
    .byte $21, $00, $84
    .byte $20, $00, $04
	.byte 0

; Sprite #12
; Single color mode, BG color: 6, Sprite color: 9
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $00, $00, $00
    .byte $0F, $FF, $F0
    .byte $0F, $FF, $F0
    .byte $0F, $FF, $F0
    .byte $0F, $DB, $F0
    .byte $0F, $DB, $F0
    .byte $0F, $DB, $F0
    .byte $0F, $FF, $F0
    .byte $0F, $FF, $F0
    .byte $0F, $FF, $F0
    .byte $08, $81, $10
    .byte $08, $81, $10
    .byte $08, $81, $10
    .byte $08, $00, $10
	.byte 0
		
	;WALL
	.byte $00, $18, $00, $00, $18, $00, $00, $18, $00, $00, $18, $00, $00, $18, $00, $00, $18, $00, $00, $18, $00
	.byte $00, $18, $00, $00, $18, $00, $00, $18, $00, $00, $18, $00, $00, $18, $00, $00, $18, $00, $00, $18, $00
	.byte $00, $18, $00, $00, $18, $00, $00, $18, $00, $00, $18, $00, $00, $18, $00, $00, $18, $00, $00, $18, $00
	.byte 0
	
	
.segment "CLRDATA"
	.byte	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $0E, $0E
	.byte	$08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08
	.byte	$08, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $09, $09, $09, $09, $07, $07, $07, $07, $07, $07, $07, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $07, $07, $09, $09, $07, $09, $09, $09, $09, $08
	.byte	$08, $09, $09, $09, $09, $07, $07, $07, $07, $07, $07, $07, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $07, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08
	.byte	$08, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $0E, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $0E, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $01, $08
	.byte	$08, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $01, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $0E, $08
	.byte	$08, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $08
	.byte	$08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08
	.byte	$0E, $01, $01, $01, $0E, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $01, $01, $01, $01, $01, $01
	.byte	$01, $08, $08, $08, $08, $08, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $08, $08, $08, $08, $08, $08, $08, $08, $01
	.byte	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E


.segment "CHRDATA"
	.byte	$3C, $66, $6E, $6E, $60, $62, $3C, $00
	.byte	$18, $3C, $66, $7E, $66, $66, $66, $00
	.byte	$7C, $66, $66, $7C, $66, $66, $7C, $00
	.byte	$3C, $66, $60, $60, $60, $66, $3C, $00
	.byte	$78, $6C, $66, $66, $66, $6C, $78, $00
	.byte	$7E, $60, $60, $78, $60, $60, $7E, $00
	.byte	$7E, $60, $60, $78, $60, $60, $60, $00
	.byte	$3C, $66, $60, $6E, $66, $66, $3C, $00
	.byte	$66, $66, $66, $7E, $66, $66, $66, $00
	.byte	$3C, $18, $18, $18, $18, $18, $3C, $00
	.byte	$1E, $0C, $0C, $0C, $0C, $6C, $38, $00
	.byte	$66, $6C, $78, $70, $78, $6C, $66, $00
	.byte	$60, $60, $60, $60, $60, $60, $7E, $00
	.byte	$63, $77, $7F, $6B, $63, $63, $63, $00
	.byte	$66, $76, $7E, $7E, $6E, $66, $66, $00
	.byte	$3C, $66, $66, $66, $66, $66, $3C, $00
	.byte	$7C, $66, $66, $7C, $60, $60, $60, $00
	.byte	$3C, $66, $66, $66, $66, $3C, $0E, $00
	.byte	$7C, $66, $66, $7C, $78, $6C, $66, $00
	.byte	$3C, $66, $60, $3C, $06, $66, $3C, $00
	.byte	$7E, $18, $18, $18, $18, $18, $18, $00
	.byte	$66, $66, $66, $66, $66, $66, $3C, $00
	.byte	$66, $66, $66, $66, $66, $3C, $18, $00
	.byte	$63, $63, $63, $6B, $7F, $77, $63, $00
	.byte	$66, $66, $3C, $18, $3C, $66, $66, $00
	.byte	$66, $66, $66, $3C, $18, $18, $18, $00
	.byte	$7E, $06, $0C, $18, $30, $60, $7E, $00
	.byte	$3C, $30, $30, $30, $30, $30, $3C, $00
	.byte	$0C, $12, $30, $7C, $30, $62, $FC, $00
	.byte	$3C, $0C, $0C, $0C, $0C, $0C, $3C, $00
	.byte	$00, $18, $3C, $7E, $18, $18, $18, $18
	.byte	$00, $10, $30, $7F, $7F, $30, $10, $00
	.byte	$00, $00, $00, $00, $00, $00, $00, $00
	.byte	$18, $18, $18, $18, $00, $00, $18, $00
	.byte	$66, $66, $66, $00, $00, $00, $00, $00
	.byte	$66, $66, $FF, $66, $FF, $66, $66, $00
	.byte	$18, $3E, $60, $3C, $06, $7C, $18, $00
	.byte	$62, $66, $0C, $18, $30, $66, $46, $00
	.byte	$3C, $66, $3C, $38, $67, $66, $3F, $00
	.byte	$06, $0C, $18, $00, $00, $00, $00, $00
	.byte	$0C, $18, $30, $30, $30, $18, $0C, $00
	.byte	$30, $18, $0C, $0C, $0C, $18, $30, $00
	.byte	$00, $66, $3C, $FF, $3C, $66, $00, $00
	.byte	$00, $18, $18, $7E, $18, $18, $00, $00
	.byte	$00, $00, $00, $00, $00, $18, $18, $30
	.byte	$00, $00, $00, $7E, $00, $00, $00, $00
	.byte	$00, $00, $00, $00, $00, $18, $18, $00
	.byte	$00, $03, $06, $0C, $18, $30, $60, $00
	.byte	$3C, $66, $6E, $76, $66, $66, $3C, $00
	.byte	$18, $18, $38, $18, $18, $18, $7E, $00
	.byte	$3C, $66, $06, $0C, $30, $60, $7E, $00
	.byte	$3C, $66, $06, $1C, $06, $66, $3C, $00
	.byte	$06, $0E, $1E, $66, $7F, $06, $06, $00
	.byte	$7E, $60, $7C, $06, $06, $66, $3C, $00
	.byte	$3C, $66, $60, $7C, $66, $66, $3C, $00
	.byte	$7E, $66, $0C, $18, $18, $18, $18, $00
	.byte	$3C, $66, $66, $3C, $66, $66, $3C, $00
	.byte	$3C, $66, $66, $3E, $06, $66, $3C, $00
	.byte	$00, $00, $18, $00, $00, $18, $00, $00
	.byte	$00, $00, $18, $00, $00, $18, $18, $30
	.byte	$0E, $18, $30, $60, $30, $18, $0E, $00
	.byte	$00, $00, $7E, $00, $7E, $00, $00, $00
	.byte	$70, $18, $0C, $06, $0C, $18, $70, $00
	.byte	$3C, $66, $06, $0C, $18, $00, $18, $00
	.byte	$00, $00, $00, $FF, $FF, $00, $00, $00
	.byte	$08, $1C, $3E, $7F, $7F, $1C, $3E, $00
	.byte	$18, $18, $18, $18, $18, $18, $18, $18
	.byte	$00, $00, $00, $FF, $FF, $00, $00, $00
	.byte	$00, $00, $FF, $FF, $00, $00, $00, $00
	.byte	$00, $FF, $FF, $00, $00, $00, $00, $00
	.byte	$00, $00, $00, $00, $FF, $FF, $00, $00
	.byte	$30, $30, $30, $30, $30, $30, $30, $30
	.byte	$0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C
	.byte	$00, $00, $00, $E0, $F0, $38, $18, $18
	.byte	$18, $18, $1C, $0F, $07, $00, $00, $00
	.byte	$18, $18, $38, $F0, $E0, $00, $00, $00
	.byte	$C0, $C0, $C0, $C0, $C0, $C0, $FF, $FF
	.byte	$C0, $E0, $70, $38, $1C, $0E, $07, $03
	.byte	$03, $07, $0E, $1C, $38, $70, $E0, $C0
	.byte	$FF, $FF, $C0, $C0, $C0, $C0, $C0, $C0
	.byte	$FF, $FF, $03, $03, $03, $03, $03, $03
	.byte	$00, $3C, $7E, $7E, $7E, $7E, $3C, $00
	.byte	$00, $00, $00, $00, $00, $FF, $FF, $00
	.byte	$36, $7F, $7F, $7F, $3E, $1C, $08, $00
	.byte	$60, $60, $60, $60, $60, $60, $60, $60
	.byte	$00, $00, $00, $07, $0F, $1C, $18, $18
	.byte	$C3, $E7, $7E, $3C, $3C, $7E, $E7, $C3
	.byte	$00, $3C, $7E, $66, $66, $7E, $3C, $00
	.byte	$18, $18, $66, $66, $18, $18, $3C, $00
	.byte	$06, $06, $06, $06, $06, $06, $06, $06
	.byte	$08, $1C, $3E, $7F, $3E, $1C, $08, $00
	.byte	$18, $18, $18, $FF, $FF, $18, $18, $18
	.byte	$C0, $C0, $30, $30, $C0, $C0, $30, $30
	.byte	$18, $18, $18, $18, $18, $18, $18, $18
	.byte	$00, $00, $03, $3E, $76, $36, $36, $00
	.byte	$FF, $7F, $3F, $1F, $0F, $07, $03, $01
	.byte	$00, $00, $00, $00, $00, $00, $00, $00
	.byte	$F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0
	.byte	$00, $00, $00, $00, $FF, $FF, $FF, $FF
	.byte	$FF, $00, $00, $00, $00, $00, $00, $00
	.byte	$00, $00, $00, $00, $00, $00, $00, $FF
	.byte	$C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
	.byte	$CC, $CC, $33, $33, $CC, $CC, $33, $33
	.byte	$03, $03, $03, $03, $03, $03, $03, $03
	.byte	$00, $00, $00, $00, $CC, $CC, $33, $33
	.byte	$FF, $FE, $FC, $F8, $F0, $E0, $C0, $80
	.byte	$03, $03, $03, $03, $03, $03, $03, $03
	.byte	$18, $18, $18, $1F, $1F, $18, $18, $18
	.byte	$00, $00, $00, $00, $0F, $0F, $0F, $0F
	.byte	$18, $18, $18, $1F, $1F, $00, $00, $00
	.byte	$00, $00, $00, $F8, $F8, $18, $18, $18
	.byte	$00, $00, $00, $00, $00, $00, $FF, $FF
	.byte	$00, $00, $00, $1F, $1F, $18, $18, $18
	.byte	$18, $18, $18, $FF, $FF, $00, $00, $00
	.byte	$00, $00, $00, $FF, $FF, $18, $18, $18
	.byte	$18, $18, $18, $F8, $F8, $18, $18, $18
	.byte	$C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
	.byte	$E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0
	.byte	$07, $07, $07, $07, $07, $07, $07, $07
	.byte	$FF, $FF, $00, $00, $00, $00, $00, $00
	.byte	$FF, $FF, $FF, $00, $00, $00, $00, $00
	.byte	$00, $00, $00, $00, $00, $FF, $FF, $FF
	.byte	$03, $03, $03, $03, $03, $03, $FF, $FF
	.byte	$00, $00, $00, $00, $F0, $F0, $F0, $F0
	.byte	$0F, $0F, $0F, $0F, $00, $00, $00, $00
	.byte	$18, $18, $18, $F8, $F8, $00, $00, $00
	.byte	$F0, $F0, $F0, $F0, $00, $00, $00, $00
	.byte	$F0, $F0, $F0, $F0, $0F, $0F, $0F, $0F
	.byte	$C3, $99, $91, $91, $9F, $99, $C3, $FF
	.byte	$E7, $C3, $99, $81, $99, $99, $99, $FF
	.byte	$83, $99, $99, $83, $99, $99, $83, $FF
	.byte	$C3, $99, $9F, $9F, $9F, $99, $C3, $FF
	.byte	$87, $93, $99, $99, $99, $93, $87, $FF
	.byte	$81, $9F, $9F, $87, $9F, $9F, $81, $FF
	.byte	$81, $9F, $9F, $87, $9F, $9F, $9F, $FF
	.byte	$C3, $99, $9F, $91, $99, $99, $C3, $FF
	.byte	$99, $99, $99, $81, $99, $99, $99, $FF
	.byte	$C3, $E7, $E7, $E7, $E7, $E7, $C3, $FF
	.byte	$E1, $F3, $F3, $F3, $F3, $93, $C7, $FF
	.byte	$99, $93, $87, $8F, $87, $93, $99, $FF
	.byte	$9F, $9F, $9F, $9F, $9F, $9F, $81, $FF
	.byte	$9C, $88, $80, $94, $9C, $9C, $9C, $FF
	.byte	$99, $89, $81, $81, $91, $99, $99, $FF
	.byte	$C3, $99, $99, $99, $99, $99, $C3, $FF
	.byte	$83, $99, $99, $83, $9F, $9F, $9F, $FF
	.byte	$C3, $99, $99, $99, $99, $C3, $F1, $FF
	.byte	$83, $99, $99, $83, $87, $93, $99, $FF
	.byte	$C3, $99, $9F, $C3, $F9, $99, $C3, $FF
	.byte	$81, $E7, $E7, $E7, $E7, $E7, $E7, $FF
	.byte	$99, $99, $99, $99, $99, $99, $C3, $FF
	.byte	$99, $99, $99, $99, $99, $C3, $E7, $FF
	.byte	$9C, $9C, $9C, $94, $80, $88, $9C, $FF
	.byte	$99, $99, $C3, $E7, $C3, $99, $99, $FF
	.byte	$99, $99, $99, $C3, $E7, $E7, $E7, $FF
	.byte	$81, $F9, $F3, $E7, $CF, $9F, $81, $FF
	.byte	$C3, $CF, $CF, $CF, $CF, $CF, $C3, $FF
	.byte	$F3, $ED, $CF, $83, $CF, $9D, $03, $FF
	.byte	$C3, $F3, $F3, $F3, $F3, $F3, $C3, $FF
	.byte	$FF, $E7, $C3, $81, $E7, $E7, $E7, $E7
	.byte	$FF, $EF, $CF, $80, $80, $CF, $EF, $FF
	.byte	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
	.byte	$E7, $E7, $E7, $E7, $FF, $FF, $E7, $FF
	.byte	$99, $99, $99, $FF, $FF, $FF, $FF, $FF
	.byte	$99, $99, $00, $99, $00, $99, $99, $FF
	.byte	$E7, $C1, $9F, $C3, $F9, $83, $E7, $FF
	.byte	$9D, $99, $F3, $E7, $CF, $99, $B9, $FF
	.byte	$C3, $99, $C3, $C7, $98, $99, $C0, $FF
	.byte	$F9, $F3, $E7, $FF, $FF, $FF, $FF, $FF
	.byte	$F3, $E7, $CF, $CF, $CF, $E7, $F3, $FF
	.byte	$CF, $E7, $F3, $F3, $F3, $E7, $CF, $FF
	.byte	$FF, $99, $C3, $00, $C3, $99, $FF, $FF
	.byte	$FF, $E7, $E7, $81, $E7, $E7, $FF, $FF
	.byte	$FF, $FF, $FF, $FF, $FF, $E7, $E7, $CF
	.byte	$FF, $FF, $FF, $81, $FF, $FF, $FF, $FF
	.byte	$FF, $FF, $FF, $FF, $FF, $E7, $E7, $FF
	.byte	$FF, $FC, $F9, $F3, $E7, $CF, $9F, $FF
	.byte	$C3, $99, $91, $89, $99, $99, $C3, $FF
	.byte	$E7, $E7, $C7, $E7, $E7, $E7, $81, $FF
	.byte	$C3, $99, $F9, $F3, $CF, $9F, $81, $FF
	.byte	$C3, $99, $F9, $E3, $F9, $99, $C3, $FF
	.byte	$F9, $F1, $E1, $99, $80, $F9, $F9, $FF
	.byte	$81, $9F, $83, $F9, $F9, $99, $C3, $FF
	.byte	$C3, $99, $9F, $83, $99, $99, $C3, $FF
	.byte	$81, $99, $F3, $E7, $E7, $E7, $E7, $FF
	.byte	$C3, $99, $99, $C3, $99, $99, $C3, $FF
	.byte	$C3, $99, $99, $C1, $F9, $99, $C3, $FF
	.byte	$FF, $FF, $E7, $FF, $FF, $E7, $FF, $FF
	.byte	$FF, $FF, $E7, $FF, $FF, $E7, $E7, $CF
	.byte	$F1, $E7, $CF, $9F, $CF, $E7, $F1, $FF
	.byte	$FF, $FF, $81, $FF, $81, $FF, $FF, $FF
	.byte	$8F, $E7, $F3, $F9, $F3, $E7, $8F, $FF
	.byte	$C3, $99, $F9, $F3, $E7, $FF, $E7, $FF
	.byte	$FF, $FF, $FF, $00, $00, $FF, $FF, $FF
	.byte	$F7, $E3, $C1, $80, $80, $E3, $C1, $FF
	.byte	$E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7
	.byte	$FF, $FF, $FF, $00, $00, $FF, $FF, $FF
	.byte	$FF, $FF, $00, $00, $FF, $FF, $FF, $FF
	.byte	$FF, $00, $00, $FF, $FF, $FF, $FF, $FF
	.byte	$FF, $FF, $FF, $FF, $00, $00, $FF, $FF
	.byte	$CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF
	.byte	$F3, $F3, $F3, $F3, $F3, $F3, $F3, $F3
	.byte	$FF, $FF, $FF, $1F, $0F, $C7, $E7, $E7
	.byte	$E7, $E7, $E3, $F0, $F8, $FF, $FF, $FF
	.byte	$E7, $E7, $C7, $0F, $1F, $FF, $FF, $FF
	.byte	$3F, $3F, $3F, $3F, $3F, $3F, $00, $00
	.byte	$3F, $1F, $8F, $C7, $E3, $F1, $F8, $FC
	.byte	$FC, $F8, $F1, $E3, $C7, $8F, $1F, $3F
	.byte	$00, $00, $3F, $3F, $3F, $3F, $3F, $3F
	.byte	$00, $00, $FC, $FC, $FC, $FC, $FC, $FC
	.byte	$FF, $C3, $81, $81, $81, $81, $C3, $FF
	.byte	$FF, $FF, $FF, $FF, $FF, $00, $00, $FF
	.byte	$C9, $80, $80, $80, $C1, $E3, $F7, $FF
	.byte	$9F, $9F, $9F, $9F, $9F, $9F, $9F, $9F
	.byte	$FF, $FF, $FF, $F8, $F0, $E3, $E7, $E7
	.byte	$3C, $18, $81, $C3, $C3, $81, $18, $3C
	.byte	$FF, $C3, $81, $99, $99, $81, $C3, $FF
	.byte	$E7, $E7, $99, $99, $E7, $E7, $C3, $FF
	.byte	$F9, $F9, $F9, $F9, $F9, $F9, $F9, $F9
	.byte	$F7, $E3, $C1, $80, $C1, $E3, $F7, $FF
	.byte	$E7, $E7, $E7, $00, $00, $E7, $E7, $E7
	.byte	$3F, $3F, $CF, $CF, $3F, $3F, $CF, $CF
	.byte	$E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7
	.byte	$FF, $FF, $FC, $C1, $89, $C9, $C9, $FF
	.byte	$00, $80, $C0, $E0, $F0, $F8, $FC, $FE
	.byte	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
	.byte	$0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
	.byte	$FF, $FF, $FF, $FF, $00, $00, $00, $00
	.byte	$00, $FF, $FF, $FF, $FF, $FF, $FF, $FF
	.byte	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $00
	.byte	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
	.byte	$33, $33, $CC, $CC, $33, $33, $CC, $CC
	.byte	$FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC
	.byte	$FF, $FF, $FF, $FF, $33, $33, $CC, $CC
	.byte	$00, $01, $03, $07, $0F, $1F, $3F, $7F
	.byte	$FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC
	.byte	$E7, $E7, $E7, $E0, $E0, $E7, $E7, $E7
	.byte	$FF, $FF, $FF, $FF, $F0, $F0, $F0, $F0
	.byte	$E7, $E7, $E7, $E0, $E0, $FF, $FF, $FF
	.byte	$FF, $FF, $FF, $07, $07, $E7, $E7, $E7
	.byte	$FF, $FF, $FF, $FF, $FF, $FF, $00, $00
	.byte	$FF, $FF, $FF, $E0, $E0, $E7, $E7, $E7
	.byte	$E7, $E7, $E7, $00, $00, $FF, $FF, $FF
	.byte	$FF, $FF, $FF, $00, $00, $E7, $E7, $E7
	.byte	$E7, $E7, $E7, $07, $07, $E7, $E7, $E7
	.byte	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
	.byte	$1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
	.byte	$F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8
	.byte	$00, $00, $FF, $FF, $FF, $FF, $FF, $FF
	.byte	$00, $00, $00, $FF, $FF, $FF, $FF, $FF
	.byte	$FF, $FF, $FF, $FF, $FF, $00, $00, $00
	.byte	$FC, $FC, $FC, $FC, $FC, $FC, $00, $00
	.byte	$FF, $FF, $FF, $FF, $0F, $0F, $0F, $0F
	.byte	$F0, $F0, $F0, $F0, $FF, $FF, $FF, $FF
	.byte	$E7, $E7, $E7, $07, $07, $FF, $FF, $FF
	.byte	$0F, $0F, $0F, $0F, $FF, $FF, $FF, $FF
	.byte	$0F, $0F, $0F, $0F, $F0, $F0, $F0, $F0
	
.segment "SCRCHRDATA"
	.byte	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	.byte	$55, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $49
	.byte	$42, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $42
	.byte	$42, $66, $66, $66, $4F, $77, $77, $77, $77, $77, $77, $77, $50, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $4F, $77, $77, $77, $77, $77, $50, $66, $66, $66, $42
	.byte	$42, $66, $66, $66, $74, $2E, $20, $20, $2E, $20, $55, $49, $67, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $65, $2E, $2E, $20, $20, $2E, $67, $66, $66, $66, $42
	.byte	$42, $66, $66, $66, $74, $20, $2E, $20, $20, $20, $4A, $4B, $67, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $74, $20, $20, $2E, $20, $20, $67, $66, $66, $66, $42
	.byte	$42, $66, $66, $66, $4C, $6F, $6F, $6F, $6F, $6F, $6F, $6F, $7A, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $4C, $6F, $6F, $6F, $6F, $6F, $7A, $66, $66, $66, $42
	.byte	$42, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $42
	.byte	$42, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $42
	.byte	$42, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $42
	.byte	$42, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $42
	.byte	$6B, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $43, $73
	.byte	$42, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $42
	.byte	$42, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $42
	.byte	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	.byte	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	.byte	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	.byte	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	.byte	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	.byte	$42, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $42
	.byte	$42, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $42
	.byte	$4A, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $40, $4B
	.byte	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	.byte	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $14, $09, $0D, $05, $3A, $20, $39, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	.byte	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20



