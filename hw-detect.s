********************************
*                              *
*      Machine Detection       *
*                              *
*           Routines           *
*                              *
*      BY MARC GOLOMBECK       *
*                              *
*   VERSION 1.00 / 26.04.2019  *
********************************
*
 					DSK 	detect
 					MX 		%11
          			ORG 	$901		; routine starts at $0901
*
HOME      			EQU $FC58     		; CLEAR SCREEN
COUT      			EQU $FDED     		; PRINT CHARACTER
KYBD      			EQU $C000     		; READ KEYBOARD
STROBE    			EQU $C010     		; CLEAR KEYBOARD
BELL      			EQU $FBDD     		; RING BELL
WAIT      			EQU $FCA8     		; WAIT A BIT
PREAD				EQU	$FB1E			; switch to LORES
VERTBLANK			EQU	$C019			; vertical blanking -> available only IIe and above
MEMCOPY				EQU	$FE2C			; monitor routine for copying RAM
WRITERAM			EQU	$C081			; read ROM write RAM - toggle 2 times
READROM				EQU	$C082			; read ROM no write  - toggle 1 time
READWRITE			EQU	$C083			; read & write RAM   - toggle 2 times
READRAM				EQU	$C080			; read RAM no write  - toggle 1 time
*
bMB					EQU	$2078			; MockingBoard available? ($00/No | $01 Yes)
bMachine			EQU	$2079			; auto detect Apple II machine type
bMem				EQU	$207A			; 128k? ($00 No | $78 Yes)
bZC					EQU	$207B			; ZIPchip installed? ($00/No | $01 Yes)
bFC					EQU	$207C			; FastChip installed? ($00/No | $01 Yes)
bLC					EQU	$207D			; Language Card available? ($00/No | $01 Yes)
bMBSlot				EQU	$207E			; slot of MockingBoard ($01..$07)
bRefresh			EQU $207F   		; byte REFRESH RATE ($00/50Hz | $01/60Hz)
b65C02 				EQU	$20F8  			; byte CPU ($00/6502 | $01/65C02 | $FF/65816)
bEmu				EQU	$20F9			; emulator detection ($00/No | $01 Yes)
*
chkMEM				EQU	$0100			; adress of AUX mem checking routine
dest				EQU $F8				; + $F9
MB_ADDRL			EQU	$06
MB_ADDRH			EQU	$07
TICKS				EQU	$09
charFLAG			EQU	$FF				; only uppercase letters?
*
*
* FastChip constants
*
FC_UNLOCK 			EQU	$6A				; FastChip unlock value
FC_LOCK				EQU $A6				; FastChip lock value
FC_LOCK_REG			EQU	$C06A			; FastChip lock register
FC_EN_REG 			EQU	$C06B			; FastChip enable register
FC_SPD_REG			EQU	$C06D			; FastChip speed register
*
INIT				SEI					; just to be sure...
					JSR	DETECTA2		; get machine type
					STA $C000			; 80STOREOFF
					STA	$C008			; zero Page = Main (+ $FXXX MAIN)
*
					JSR	COPYAUX			; copy main program to AUX-memory
					JSR	COPYROM			; copy MONITOR to language card
					JMP	chkMEM			; jump to short routine in Stack for checking AUX MEM size
					JSR	DETECTCPU
					JSR	DETECTREFRESH
*					
					JSR	DETECTZIP		; detect and disable a ZIPchip
					JSR	DETECTFC		; detect FastChip and set to 1 MHz
					JSR	DETECTEMU		; detect possible emulator first
					JSR	DETECTLC		; can we also detect a language card?
					JSR	MB_DETECT		; detect MB-slot
					JSR	HOME			; clear screen
					JSR	PRTRES			; print detection results on screen
*
					RTS

*
*
*************************************************
* COPY PROGRAM TO AUX MEMORY                    *
*************************************************
*
COPYAUX
					STA	$C002			; read MAIN
					STA	$C005			; write AUX
					LDA	#$09			; set start adress HI-byte
					STA	dest+1			; for page copy
					LDA	#0
					STA	dest			; LO-byte
*					
cpAUXlp2			LDY #0				; loop over Y
cpAUXlp1			LDA	(dest),Y		; read from MAIN memory
					STA	(dest),Y		; store to AUX memory at same address!
					INY
					BNE	cpAUXlp1
					INC	dest+1			; increase HI-byte +1
					LDA	dest+1			; must include the MB-playing routine in code copy!
					CMP	#$0d			; HI-Byte = $14? Copy until reaching $13FF!
					BNE	cpAUXlp2		; no, copy next page
					STA	$C004			; write MAIN
					
					LDA	#>CODE
					STA	dest+1
					LDA	#<CODE			; -> copy MEM chk routine to Stack area
					STA	dest
					LDY	#0
					STY	bMem			; init result byte
cpSTACK				LDA	(dest),Y
					STA	chkMEM,Y
					INY
					CPY	#15
					BNE	cpSTACK
*					
					RTS
*	
*
* copy monitor ROM
*
COPYROM				LDY	#$F8			; copy Monitor-ROM to Language Card
					STY	$3D
					STY	$43
					LDY	#$FF
					STY	$3E
					STY	$3F
					INY
					STY	$3C
					STY	$42
					LDA	WRITERAM		; toggle Language Card RAM for writing
					LDA	WRITERAM
					JSR	MEMCOPY
					LDA	READROM			; toggle Language Card off
					RTS

*=============================================================================
*
* detect slot with MockingBoard
*
MB_DETECT			LDA	#0
					STA	MB_ADDRL
*					
MB_DET_lp			LDA	#$07			; we start in slot 7 ($C7) and go down to 0 ($C0)
					STA	bMBSlot			; slot with MB
					ORA	#$C0			; make it start with C
					STA	MB_ADDRH
					LDY	#04				; $CX04
					LDX	#02				; 2 tries?
MB_CHK_CYC			LDA	(MB_ADDRL),Y	; timer 6522 (Low Order Counter)
*										; count down
					STA	TICKS			; 3 cycles
					LDA	(MB_ADDRL),Y	; + 5 cycles EQU 8 cycles
*										; between the two accesses to the timer
					SEC
					SBC	TICKS			; subtract to see if we had 8 cycles (accepted range 7-9)
					CMP	#$f8			; -8
					BEQ	DEC_X
					CMP	#$F9			; -7 - range is necessary if FastChip is installed
					BEQ	DEC_X
					CMP	#$F7			; -9
					BNE	MB_NOT_SLOT
DEC_X				DEX					; decrement, try one more time
					BNE	MB_CHK_CYC		; loop detection
					INX					; Mockingboard found (XEQU1)
DONE_DET			STX	bMB				; store result to bMB
					RTS					; return
*
MB_NOT_SLOT			DEC	MB_DET_lp+1		; decrement the "slot" (self_modify)
					BNE	MB_DET_lp		; loop down to one
					LDX	#00
					BEQ	DONE_DET				

*=============================================================================
*
* detect type of Apple 2 computer
*
DETECTA2
					LDA	#$FF
					STA	charFLAG		; allow lower case output
        			LDA $FBB3
        			CMP #$06        	; IIe/IIc/IIGS = 06 
        			BEQ checkII	   		; if not II ($38) or II+ ($EA)
_G22PLUS  			STA	bMachine    	; save $38 or $EA as machine byte
					LDA	#%11011111		; convert lowercase chars to uppercase
					STA	charFLAG
        			RTS       
*        								
checkII	  			LDA $FBC0       	; detect IIc
        			BEQ _G2C        	; 0 = IIc / Other = no IIc
*				
        			SEC					; IIgs or IIe ? 
        			JSR $FE1F       	; test for GS 
        			BCS _G2E        	; if carry flag is set -> IIE
*        	
        			LDA #$FF        	; IIGS -> $FF
        			STA bMachine
*=============================================================================
*
* condition some IIgs settings
*        
					LDA $C036			; put IIgs in 8-bit-mode
 					AND #$7F
 					STA $C036 			; slow speed
* 	
 					LDA $C034 			;
					AND #$F0
 					STA $C034 			; black  border
*
					LDA $C022
					AND #$F0			; bit 0-3 at 0 = background black
					ORA #$F0			; bit 7-4 at 1 = text white
					STA $C022			; background black/text white	
        			RTS
*        
_G2E    			LDA	#$7F        	; IIE -> $7F
    				STA bMachine    
        			RTS
*        	
_G2C    			STA bMachine    	; IIc -> $00
        			RTS
*=============================================================================
*
* detect CPU type
*
DETECTCPU
          			LDA	#00
        			BRA _is65C02		; results in a BRA +4 if 65C02/65816 or NOPs if 6502
         			BEQ _contD  		; a 6502 drops through directly to the BEQ here 
_is65C02  			INC	A				; 65C02/65816 arrives here
*										; some 65816 code here
        			XBA               	; .byte $eb, put $01 in B accu -> equals a NOP for 65C02
        			DEC	A            	; .byte $3a, A=$00 if 65C02
        			XBA               	; .byte $eb, get $01 back if 65816
        			INC A            	; .byte $1a, make $01/$02
_contD    			STA b65C02
    				RTS    
        	
*=============================================================================
*
* detect and disable a ZIPchip
*
DETECTZIP
					LDA #$5A			; unlock ZC
					STA $C05A
					STA $C05A
					STA $C05A
					STA $C05A

					LDX	#$00
CACHE				LDA	ALTER,X			; put altering part of the code in the cache!
					INX
					BNE	CACHE				
			
ALTER				LDA $C05C         	; Get the slot delay status
        			EOR #$FF          	; Flip it
        			STA $C05C         	; Save it
        			CMP $C05C         	; Correct?
        			BNE NOZIP        	; No, ZIP CHIP not found.

        			EOR #$FF          	; Get back old status
        			STA $C05C         	; Save it
        			CMP $C05C         	; Correct?
        			BNE NOZIP         	; No, ZIP CHIP not found.
	
					LDA #$00			; other value as $5A or $A5
					STA $C05A			; will disable ZC
					LDA	#$A5			; lock ZC
					STA	$C05A
					LDA #01
					STA bZC
					RTS
			
NOZIP   			LDA #00
					STA	bZC
					RTS							
*=============================================================================
*
* detect a FastChip and set speed to 1 MHz
*
DETECTFC			LDA	bMachine		; get machine type byte
        			LDX	#$00
        			CMP	#$7F          	; Apple IIe
        			BEQ	chk_slot
					BNE	not_found
chk_slot			LDY	#$04			; loop 4 times
        			LDA	#FC_UNLOCK    	; load the unlock value
unlock_lp			STA	FC_LOCK_REG   	; store in lock register
        			DEY
        			BNE unlock_lp
        			LDA	#$00			; MegaAudio check
        			STA	FC_EN_REG 		; enable the Fast Chip
        			LDY	FC_EN_REG 		; bit 7 will be high when enabled
        			CPY	#$80
        			BNE	not_found
found				LDA	#$80			; reset speed register
					STA	FC_SPD_REG
					LDA	#$09			; set FC speed to 1.10 MHz 
					STA	FC_SPD_REG		; necessary for correct MB-detection!
					LDA	#FC_LOCK
        			STA	FC_LOCK_REG   	; lock the registers again
        			LDA	#$01
					STA	bFC
					RTS
not_found			TXA
					STA	bFC
					RTS
*=============================================================================
*
* detect Language Card
*
DETECTEMU
					LDA	#0
					STA	bEmu			; set value to "real wood"
					LDX	#0				; read PDL(0)
					JSR	PREAD
					STY	PDL0			; store value
					LDX	#1				; read PDL(0)
					JSR	PREAD
					STY	PDL1			; store value
					LDX	#2				; read PDL(0)
					JSR	PREAD
					STY	PDL2			; store value
					LDX	#3				; read PDL(0)
					JSR	PREAD
					STY	PDL3			; store value
*					
					LDA	#0
					STA	PDL0+1
					STA	PDL1+1
					STA	PDL2+1
					STA	PDL3+1
					
					CLC
					LDA	PDL0			; adding all read PDL values
					ADC	PDL1			; emulators mainly return #127 for PDL-read
					STA	PDLsum			; real wood returns 255 if no paddle is connected
					LDA	PDL0+1			; or anything else (but mostly not 127 for all values)
					ADC	PDL1+1
					STA	PDLsum+1
					CLC
					LDA	PDLsum			; adding all read PDL values
					ADC	PDL2			; emulators mainly return #127 for PDL-read
					STA	PDLsum
					LDA	PDLsum+1
					ADC	PDL2+1
					STA	PDLsum+1
					CLC
					LDA	PDLsum			; adding all read PDL values
					ADC	PDL3			; emulators mainly return #127 for PDL-read
					STA	PDLsum
					LDA	PDLsum+1
					ADC	PDL3+1
					STA	PDLsum+1
					
					LDA	PDLsum+1
					CMP	#1
					BEQ Pchk1			; check for 508 as magic number = 4*127
					JMP	Pchk3
Pchk1				LDA	PDLsum
					CMP	#252
					BEQ	Pchk2
					JMP	Pchk3
Pchk2									; magic number found				
					INC	bEmu			; bEmu = 1								
					RTS
*
Pchk3				LDA	PDL0			; second check if first check fails
					CMP	PDL1			; PDL(0) = PDL(1)?
					BEQ	Pchk3a			; yes -> check for other two values
					RTS					; values are different, likely real wood 
Pchk3a				LDA	PDL0			; open gate? (=255) is likely to be real wood here with no joystick connected
					CMP	#255
					BNE	Pchk3b
					RTS
Pchk3b					LDA	PDL2
					CMP	PDL3
					BEQ	Pchk2			; assume emulator
					RTS
					
PDL0				DS 	2				; paddle read values
PDL1				DS 	2
PDL2				DS 	2
PDL3				DS 	2
PDLsum				DS 	2					
*=============================================================================
*
* detect Emulator
*
DETECTLC
					LDA	$C083			; turn on LC
					LDA	$C083
					LDA	$D000
					INC	$D000
				
					CMP	$D000
					BEQ	noLCARD
					DEC	$D000
					LDA	#1				; language card positive
					STA	bLC				; -> might be an emulator!
					JMP	exitEMU
noLCARD				LDA	#0
					STA bLC
					LDA	bEmu			; emulator detected?
					BNE exitEMU			
					LDA	#2				; can't tell if running in an emulator -> sorry!
					STA bEmu
exitEMU				LDA	$C081			; turn off LC
					RTS				
*=============================================================================
*
*	detect screen refresh rate
*
DETECTREFRESH
					LDA bMachine
        			BEQ Refresh_IIC    	; IIc -> another refresh detection method is necessary
          			CMP #$EA
          			BEQ _badguy        	; II+ -> no detection possible
          			CMP #$38
          			BEQ _badguy        	; II -> no detection possible
                                                            
_L1       			CMP VERTBLANK      	; works with IIGS/IIE      
          			BPL _L1            	; wait until the end of the vertical blank
                                                
_L2       			CMP VERTBLANK  		; synchronizing counting algorithm      
          			BMI _L2            	; wait until the end of the complete display -> start of vertical blanking
_BP3     			INC COUNTREF    	; 6 increment counter

          			LDX #$09        	; wait for a certain number of cycles                        
_WL3      			DEX             	;                   
          			BNE _WL3        	; = 46 cycles
                            			; + 6 + 3 + 3 + 4 + 3 = 65 !
          			LDA bMachine  		; 3
          			LDA bMachine    	; 3
        			CMP VERTBLANK   	; 4
          			Bpl _BP3       		; 3 -> repeat and increment counter until VBL is done
        
        			LDA COUNTREF
          			CMP #72         	; >= 72 equals 50 HZ (120*65 cycles of VBL)
          			BCS _GO3
         			LDA #01         	; 60HZ (VBL = 70x65 cycles) 
         			JMP _GO4
_GO3     			LDA #00         	; 50HZ (VBL = 120x65 cycles)
_GO4     			STA bRefresh    
          			RTS
*
_badguy   			LDA #$FF			; system without VBL-signal is a bad guy
          			STA bRefresh
          			RTS
*
Refresh_IIC   							; specific treatment for Apple IIc
         			STA $C070
          			STA $C07F
          			STA $C05B
_l1       			LDA $C019			; preparation for the beginning of VBL
          			bpl _l1                      
          			STA $C070
          			STA $C07F
          			STA $C05B			; enable VBL interrupt
            
_BP2     			INC COUNTREF    	; 6 increment counter

          			LDX #$09        	;                           
_WL1      			DEX             	;                   
          			BNE _WL1        	; = 46 cycles
                            			; + 6 + 6 + 4 + 3 = 65 !
          			NOP
          			NOP
          			NOP             	; 3*2 = 6 additional cycles

         			LDA $C019			; waiting for the next VBL to begin
          			bpl _BP2         	; 3 -> wait for vblflag = 1

          			STA $C058+2     	; disable VBL int. Contains a complete VBL + one display. Exit on the beginning of  
         			LDA COUNTREF    	; the next VBL gives = 192+70+(50) = $138 or ($106) hence $38 or $06
          			CMP #$10        	; >= 16 hence 50 HZ 
          			BCS _GO1
          			LDA #01         	; 60HZ 
          			JMP _GO2
_GO1      			LDA #00         	; 50HZ
_GO2      			STA bRefresh    
          			RTS 
COUNTREF			DS	1        


*==============================================================
*
* print detection results on screen
*
PRTRES				LDA	#0				; set cursor position
					STA	$25
					JSR LFEED

					LDA	#6				; HTAB 6
					STA	$24
					LDX	#<T_APPLE
					LDY	#>T_APPLE
					JSR	PRINT
					JSR LFEED
					JSR LFEED
					LDA	#7				; HTAB 7
					STA	$24
								
					LDX	#<T_MACH
					LDY	#>T_MACH
					JSR	PRINT
				
					LDA	bMachine
					BNE	comp1			; //c = $00?
					LDX	#<T_COMP3
					LDY	#>T_COMP3
					JSR	PRINT
					JMP	NEXT1

comp1				CMP	#$7F			; //e?
					BNE	comp2				
					LDX	#<T_COMP1
					LDY	#>T_COMP1
					JSR	PRINT
					JMP	NEXT1
				
comp2				CMP	#$FF			; IIgs?
					BNE	comp3
					LDX	#<T_COMP2
					LDY	#>T_COMP2
					JSR	PRINT
					JMP	NEXT1

comp3				CMP	#$38			; II?
					BNE	comp4
					LDX	#<T_COMP4
					LDY	#>T_COMP4
					JSR	PRINT
					JMP	NEXT1

comp4				CMP	#$EA			; II?
					BNE	comp5
					LDX	#<T_COMP5
					LDY	#>T_COMP5
					JSR	PRINT
					JMP	NEXT1

comp5				LDX	#<T_COMP6		; unknown machine
					LDY	#>T_COMP6
					JSR	PRINT	

NEXT1			
					JSR LFEED			; print CPU type
					LDA	#7				; HTAB 7
					STA	$24
					LDX	#<T_CPU
					LDY	#>T_CPU
					JSR	PRINT

	        		LDA b65C02
	        		CMP	#2				; 65816 in any machine setting!
	        		BEQ	_s2_cpu
	        		LDA	b65C02
	        		BEQ _s1_cpu         ; 0     (6502)
	        		BMI _s2_cpu         ; >$7F  (65816 or 65C802)
	        		LDX #<T_CPU2        ; 1     (65C02)
 	       			LDY #>T_CPU2
 	       			JSR	PRINT
	        		JMP NEXT1a
_s1_cpu
        			LDX #<T_CPU1		
        			LDY #>T_CPU1
        			JSR PRINT
        			JMP NEXT1a
_s2_cpu									; CPU is 65816 or 65C802
					LDA	bMachine
					CMP	#$FF			
					BEQ	cpu816			; is 65816
        			LDX #<T_CPU4		; is 65C802
        			LDY #>T_CPU4
        			JSR PRINT
					JMP	NEXT1a
cpu816				
        			LDX #<T_CPU3
        			LDY #>T_CPU3
        			JSR PRINT

NEXT1a				JSR LFEED			; output RAM
					LDA	#7				; HTAB 7
					STA	$24
					LDX	#<T_RAM
					LDY	#>T_RAM
					JSR	PRINT
				
					LDA	bMem
					CMP	#$78			; $78 is flag byte for 128k!
					BNE	not128			; 64 or 48k
					LDA	bMachine		; check for Apple ][ and ][+ -> no 128k supported
					CMP	#$38
					BEQ	not128
					CMP	#$EA
					BEQ	not128

					LDX	#<T_128			; 128k
					LDY	#>T_128
					JSR	PRINT
					JMP	NEXT1b			

not128				LDA	bLC
					BEQ	only48				
					LDX	#<T_64			; 64k
					LDY	#>T_64
					JSR	PRINT
					JMP	NEXT1b		

only48				LDX	#<T_48			; 48k
					LDY	#>T_48
					JSR	PRINT

NEXT1b				JSR LFEED			; output Language Card
					LDA	#7				; HTAB 7
					STA	$24
					LDX	#<T_LANG
					LDY	#>T_LANG
					JSR	PRINT

					LDA	bLC
					BEQ	noLC				
					LDX	#<T_YES			; YES
					LDY	#>T_YES
					JSR	PRINT
					JMP	NEXT2	

noLC				LDX	#<T_NO			; NO LC
					LDY	#>T_NO
					JSR	PRINT
				

				
NEXT2				JSR LFEED			; print refresh rate
					LDA	#7				; HTAB 7
					STA	$24
        			LDX #<T_REFRESH
        			LDY	#>T_REFRESH
        			JSR	PRINT
        			LDA	bRefresh
        			BMI	_s2_rate
        			BEQ	_s1_rate
        			LDX #<T_RATE2
        			LDY #>T_RATE2
        			JSR PRINT
        			JMP NEXT3
_s1_rate
        			LDX #<T_RATE1
        			LDY #>T_RATE1
        			JSR PRINT
        			JMP NEXT3
_s2_rate
        			LDX #<T_RATE3
       				LDY #>T_RATE3
        			JSR	PRINT   
				
NEXT3				JSR LFEED			; output MockingBoard detection results
					LDA	#7				; HTAB 7
					STA	$24
					LDX	#<T_MOCK
					LDY	#>T_MOCK
					JSR	PRINT

					LDA	bMB
					BEQ	noMB			; no MB detected
					LDX	#<T_SLOT		; slot no
					LDY	#>T_SLOT
					JSR	PRINT
					LDA	bMBSlot			; get slot no and convewrt to ASCII-value
					CLC
					ADC	#$B0
					JSR	COUT			; output ASCII-value!
					JMP	NEXT4		
		
noMB				LDX	#<T_NONE		; no MB!
					LDY	#>T_NONE
					JSR	PRINT
				
NEXT4				JSR LFEED			; output FastChip detection
					LDA	#7				; HTAB 7
					STA	$24
					LDX	#<T_FC
					LDY	#>T_FC
					JSR	PRINT

					LDA	bFC
					BEQ	noFC			; no FC detected

					LDX	#<T_YES			; FC detected!
					LDY	#>T_YES
					JSR	PRINT
					JMP	NEXT5
				
noFC				LDX	#<T_NO			; no FC!
					LDY	#>T_NO
					JSR	PRINT

NEXT5				JSR LFEED			; output ZipChip detection result
					LDA	#7				; HTAB 7
					STA	$24
					LDX	#<T_ZC
					LDY	#>T_ZC
					JSR	PRINT

					LDA	bZC
					BEQ	noZC			; no ZC detected

					LDX	#<T_YES			; ZC detected!
					LDY	#>T_YES
					JSR	PRINT
					JMP	NEXT6
				
noZC				LDX	#<T_NO			; no ZC!
					LDY	#>T_NO
					JSR	PRINT

NEXT6				JSR LFEED			; output Emulator detection
					JSR	LFEED
					LDA	#7				; HTAB 7
					STA	$24
					LDX	#<T_EMU
					LDY	#>T_EMU
					JSR	PRINT
				
					LDA	bEmu
					BEQ pnoEmu
				
					CMP #1
					BEQ pyesEmu
				
					LDX	#<T_UNCLEAR		; unclear condition
					LDY	#>T_UNCLEAR
					JSR	PRINT
					JMP	prtSHK

pnoEmu				LDX	#<T_RATE3		; no EMU detected!
					LDY	#>T_RATE3
					JSR	PRINT
					JMP	prtSHK

pyesEmu				LDX	#<T_POS			; EMU detected!
					LDY	#>T_POS
					JSR	PRINT
 			
			
prtSHK				JSR	LFEED			; output copyright notice
					JSR	LFEED
					JSR	LFEED
					LDA	#4				; HTAB 4
					STA	$24
					LDX	#<T_SHACK
					LDY	#>T_SHACK
					JSR	PRINT
		
	
resRTS				RTS
*
*
* text output routines
*
LFEED				LDA	#$8D			; print a line feed
					JSR	COUT
					RTS
*				
PRINT									; x = LO-Byte Text, y = HI-Byte Text
!zone
					STX ba+1 
					STY ba+2
		
					LDX #00
ba					LDA $1000,X
					BEQ rtsPRINT
					CMP #$E1			; "a" char in lower case range?
					BCC	doCOUT			; no -> direct output
					CMP	#$FB			; "z"
					BCS	doCOUT			; no -> direct output
					AND	charFLAG		; lowercase conversion if necessary
doCOUT				JSR COUT			; output ASCII on screen
					INX
					BNE ba
rtsPRINT			RTS

*=============================================================================
*
* AUX-memory detection code
*
CODE				HEX	8D03C0AD01098D02
					HEX	C08D7A204C1409
*
*
* text output data
*
T_APPLE				ASC	"* Hardware Autodetection *"
					HEX	00
T_SHACK				ASC	"* V.1.00 by 8-BIT-SHACK 2019 *"
					HEX	00
T_MACH				ASC	"Machine:       "
					HEX	00		
T_COMP1				ASC "Apple //e"
					HEX	00
T_COMP2				ASC "Apple IIgs"
					HEX	00
T_COMP3				ASC	"Apple //c"
					HEX	00
T_COMP4				ASC	"Apple ]["
					HEX	00
T_COMP5				ASC	"Apple ][+"
					HEX	00
T_COMP6				ASC	"Other"
					HEX	00
T_RAM				ASC	"RAM:           "
					HEX	00
T_48				ASC	"48kB "
					HEX	00						
T_64				ASC	"64kB (LC) "
					HEX	00	
T_128				ASC	">= 128kB "
					HEX	00	
T_MOCK				ASC	"MockingBoard:  "
					HEX 00
T_NONE				ASC	"None "
					HEX	00
T_SLOT				ASC	"Slot #"
					HEX	00			
T_FC				ASC	"FastChip:      "
					HEX	00
T_YES				ASC	"Yes"
					HEX	00
T_ZC				ASC	"ZIPChip:       "
					HEX	00
T_NO				ASC	"No"
					HEX	00
T_CPU				ASC	"CPU-Type:      "
					HEX	00
T_REFRESH			ASC "Refresh Rate:  "
					HEX 00
T_LANG				ASC	"Language Card: "
					HEX	00
T_EMU				ASC	"Emulator:      "
					HEX	00
T_CPU1    			ASC "6502"
					HEX 00
T_CPU2    			ASC "65C02"
					HEX 00
T_CPU3    			ASC "65816"
					HEX 00
T_CPU4    			ASC "65C802"
					HEX 00
T_RATE1   			ASC "50Hz"
					HEX 00
T_RATE2   			ASC "60Hz"
					HEX 00
T_RATE3   			ASC "Not detected!"
					HEX 00
T_POS   			ASC "Detected!"
					HEX 00
T_UNCLEAR  			ASC "Unclear..."
					HEX 00
*
* end of file
*
