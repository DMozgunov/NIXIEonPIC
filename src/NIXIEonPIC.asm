;*******************************************************************************
;                                                                              *
;    Filename:                                                                 *
;    Date: 13.04.2015
;    File Version: 0.1
;    Author:
;    Company:
;    Description: Initial assembly file made in MPLAB 8 for DIY NIXIE project
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Known Issues: This template is designed for relocatable code.  As such,   *
;    build errors such as "Directive only allowed when generating an object    *
;    file" will result when the 'Build in Absolute Mode' checkbox is selected  *
;    in the project properties.  Designing code in absolute mode is            *
;    antiquated - use relocatable mode.                                        *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Revision History:                                                         *
;                                                                              *
;*******************************************************************************



;*******************************************************************************
; Processor Inclusion
;
; TODO Step #1 Open the task list under Window > Tasks.  Include your
; device .inc file - e.g. #include <device_name>.inc.  Available
; include files are in C:\Program Files\Microchip\MPLABX\mpasmx
; assuming the default installation path for MPLAB X.  You may manually find
; the appropriate include file for your device here and include it, or
; simply copy the include generated by the configuration bits
; generator (see Step #2).
;
;*******************************************************************************

;#include <p16f887.inc>
#include "p16f628a.inc"  ; Include header file

;*******************************************************************************
;
; TODO Step #2 - Configuration Word Setup
;
; The 'CONFIG' directive is used to embed the configuration word within the
; .asm file. MPLAB X requires users to embed their configuration words
; into source code.  See the device datasheet for additional information
; on configuration word settings.  Device configuration bits descriptions
; are in C:\Program Files\Microchip\MPLABX\mpasmx\P<device_name>.inc
; (may change depending on your MPLAB X installation directory).
;
; MPLAB X has a feature which generates configuration bits source code.  Go to
; Window > PIC Memory Views > Configuration Bits.  Configure each field as
; needed and select 'Generate Source Code to Output'.  The resulting code which
; appears in the 'Output Window' > 'Config Bits Source' tab may be copied
; below.
;
;*******************************************************************************
;; config for DEV board with pic16F887
;__CONFIG    _CONFIG1, _LVP_OFF & _FCMEN_OFF & _IESO_OFF & _BOR_OFF & _CPD_OFF & _CP_OFF & _MCLRE_OFF & _PWRTE_ON & _WDT_OFF & _INTRC_OSC_NOCLKOUT
;__CONFIG    _CONFIG2, _WRT_OFF & _BOR21V

; config for PIC16F628
__config  _INTOSC_OSC_NOCLKOUT & _LVP_OFF & _WDT_OFF & _PWRTE_ON & _BODEN_OFF & _MCLRE_OFF & _CP_OFF
;

;*******************************************************************************
;
; TODO Step #3 - Variable Definitions
;
; Refer to datasheet for available data memory (RAM) organization assuming
; relocatible code organization (which is an option in project
; properties > mpasm (Global Options)).  Absolute mode generally should
; be used sparingly.
;
; Example of using GPR Uninitialized Data
;
;   GPR_VAR        UDATA
;   MYVAR1         RES        1      ; User variable linker places
;   MYVAR2         RES        1      ; User variable linker places
;   MYVAR3         RES        1      ; User variable linker places
;
;   ; Example of using Access Uninitialized Data Section (when available)
;   ; The variables for the context saving in the device datasheet may need
;   ; memory reserved here.
;   INT_VAR        UDATA_ACS
;   W_TEMP         RES        1      ; w register for context saving (ACCESS)
;   STATUS_TEMP    RES        1      ; status used for context saving
;   BSR_TEMP       RES        1      ; bank select used for ISR context saving
;
;*******************************************************************************

; make PORTA and PORTB pins more readable
RA0 EQU 0
RA1 EQU 1
RA2 EQU 2
RA3 EQU 3
RA4 EQU 4
RA5 EQU 5
RA6 EQU 6
RA7 EQU 7

RB0 EQU 0
RB1 EQU 1
RB2 EQU 2
RB3 EQU 3
RB4 EQU 4
RB5 EQU 5
RB6 EQU 6
RB7 EQU 7

; other staff
CCP1 EQU RB3

FALSE	EQU 0
TRUE	EQU 1

NIXIE_ZERO equ b'00000110'
NIXIE_NINE equ b'00001111'

POWER_IS_UP equ RA5

; Trying to be as clear as possible about port connection
; Still not sure it is a good idea
; all K155ID1 address lines connected to PORTA
K155ID1_A0_on_pA equ RA3
K155ID1_A1_on_pA equ RA4
K155ID1_A2_on_pA equ RA6
K155ID1_A3_on_pA equ RA7

; 74LS138 address lines are both on PORTA and PORTB
; need to be carefull
_74LS138_A0_on_pA equ RA2
_74LS138_A1_on_pB equ RB5
_74LS138_A2_on_pB equ RB4


BT_MODE equ RA3
BT_INC equ RA4
BT_DEC equ RA6
BT_SNOOZE equ RA7
 
 
Debug EQU TRUE	        ; A Debugging Flag

cblock 0x20		; (max 80 Bytes)
    Delay1
    Delay2
<<<<<<< HEAD
    
    USART_RECEIVED
=======

>>>>>>> origin/master

    SECONDS_BCD		; Storing seconds for display on NIXIE ()
    MINUTES_BCD
    HOURS_BCD

    PORTA_TMP		; Used to save PORTA value during indication process. Used for output values
    PORTB_TMP		; This is tmp reg for PORTB set up during dynamic indication

    PORTA_IN_TMP	;

    ; Buttons debounce
    BUTTONS_COUNT_BT_MODE   ; counters for debounce
    BUTTONS_COUNT_BT_INC
    BUTTONS_COUNT_BT_DEC
    BUTTONS_COUNT_BT_SNOOZE

    BUTTONS_UP		; Buttons, pushed by user and debounced in software

    VAL_FOR_INDICATION	; This reg stores value of seconds, minutes or houres
			; which is to be shown on tubes

    INDICATION_TMP1	; temporary register to hold PORTA output bits


    endc


    cblock 0x70     ; put these up in unbanked RAM (max 16 Bytes)
		W_Save
		STATUS_Save
    endc

;*******************************************************************************
; Reset Vector
;*******************************************************************************

RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    MAIN_PROGRAM                   ; go to beginning of program

;*******************************************************************************
; TODO Step #4 - Interrupt Service Routines
;
; There are a few different ways to structure interrupt routines in the 8
; bit device families.  On PIC18's the high priority and low priority
; interrupts are located at 0x0008 and 0x0018, respectively.  On PIC16's and
; lower the interrupt is at 0x0004.  Between device families there is subtle
; variation in the both the hardware supporting the ISR (for restoring
; interrupt context) as well as the software used to restore the context
; (without corrupting the STATUS bits).
;
; General formats are shown below in relocatible format.
;
;------------------------------PIC16's and below--------------------------------
;
; ISR       CODE    0x0004           ; interrupt vector location
;
;     <Search the device datasheet for 'context' and copy interrupt
;     context saving code here.  Older devices need context saving code,
;     but newer devices like the 16F#### don't need context saving code.>
;
;     RETFIE
;
;*******************************************************************************

ISR       CODE    0x0004
<<<<<<< HEAD
    	
       
    movwf W_Save		; Save context
=======


    movwf W_Save              ; Save context
>>>>>>> origin/master
    movf STATUS,w
    movwf STATUS_Save

    bcf STATUS,RP0		; select Register Page 0

    if ( Debug )
	;RA0 is our LED debug output for now

	call LED_debug
	
	movlw 'I'
	call SendByte
	call EndLine
    endif 


    ; Select Interrupt to process

ServiceTimer1:
    bcf PIR1,TMR1IF         ; clear the interrupt flag. (must be done in software)

    MOVLW 0x80
    MOVWF TMR1H ; 1 Second Overflow
    clrf  TMR1L

    ; Count NIXIE TIME
    ; Look at the seconds/minutes coding

    ;|--------|--------|
    ;|  0000  |  0110  | 0 sec.
    ;|  0000  |  0111  | 1 sec.
    ;|  0000  |  1000  | 2 sec.
    ;|  0000  |  1001  | 3 sec.
    ;.................
    ;|  0000  |  1111  | 9 sec.

    ;|  0001  |  0000  | 10 sec.
    ;..................
    ;|  0101  |  1111  | 59 sec.

    ;|  0110  |  0000  | 60 sec. after common increment, but we have 0 as 0110
    ;  in lower nibble so our 60 seconds should look like this:
    ;|  0110  |  0110  |

    ; it makes incremet working correctly
    ; but also means we have to substitute 0110 every time to get correct
    ; value to make it possible to display lower nibble digit correctly

    incf SECONDS_BCD, F 		; add one more second to NIXIE TIME

    movlw NIXIE_NINE			;
    andwf SECONDS_BCD, W

    btfss STATUS, Z			; if we've got 0001 0000 in  SECONDS_BCD then we need to zero (xxxx 0110)
    goto SECONDS_CHECK			; last NIXIE digit and add one to decade

    movlw NIXIE_ZERO
    iorwf SECONDS_BCD, F

SECONDS_CHECK

    movlw b'01100110'			; this is how 60 looks like in this system.
    subwf SECONDS_BCD, W

    btfss STATUS, Z			; if we got 60 seconds, add one minute and clear seconds to zero (00000110)
    goto ExitISR

    movlw NIXIE_ZERO			; move 0110 to lower nibble. 00 seconds it is
    movwf SECONDS_BCD

    incf MINUTES_BCD, F

    movlw NIXIE_NINE			; reached 9 and have to zero lower nibble
    andwf MINUTES_BCD, W

    btfss STATUS, Z
    goto MINUTES_CHECK

    movlw NIXIE_ZERO
    iorwf MINUTES_BCD, F

MINUTES_CHECK

    movlw b'01100110'
    subwf MINUTES_BCD, W

    btfss STATUS, Z			; if we got 60 minutes, add one minute and clear seconds to zero (00000110)
    goto ExitISR

    movlw NIXIE_ZERO			; move 0110 to lower nibble. 00 minutes it is
    movwf MINUTES_BCD

    incf HOURS_BCD, F

    movlw NIXIE_NINE			; reached 9 and have to zero lower nibble
    andwf HOURS_BCD, W

    btfss STATUS, Z
    goto HOURS_CHECK

    movlw NIXIE_ZERO
    iorwf HOURS_BCD, F

HOURS_CHECK
    movlw b'00101010'
    subwf HOURS_BCD, W

    btfss STATUS, Z			; we reached 24 houres, so time to 00:00:00
    goto ExitISR

    movlw NIXIE_ZERO			;
    movwf HOURS_BCD


ExitISR
    movf      STATUS_Save,w       ; Restore context
    movwf     STATUS
    swapf     W_Save,f            ; swapf doesn't affect Status bits, but MOVF would
    swapf     W_Save,w

    if ( Debug )
<<<<<<< HEAD
    btfsc PIR1,TMR1IF           ; Check Timer 1 - one more second to go
    goto ServiceTimer1
    
    
    
    
    goto ExitISR
=======
    btfsc     PIR1,TMR1IF           ; Check Timer 1 - one more second to go
    goto      ServiceTimer1

    goto      ExitISR
>>>>>>> origin/master

    retfie


MAIN_PROGRAM:
; ------------------------------------
; PORTS SETUP
; ------------------------------------

<<<<<<< HEAD
   
    endif  
=======
    bcf     PORTB, 0            ; Set high, use to measure total
    endif
>>>>>>> origin/master

    clrf PORTA
    clrf PORTB

    bsf STATUS,RP0		; select Register Page 1

<<<<<<< HEAD
    bcf STATUS,RP0		; select Register Page 0
    bcf STATUS,RP1		; 
    
    movlw 7 
    movwf CMCON			; CMCON=7 set comperators off 
=======
    movlw 7
    movwf CMCON             ; CMCON=7 set comperators off
>>>>>>> origin/master


    bsf PORTA, RA0

; ------------------------------------
; TIMER1 and interrupts SETUP
    bsf PIE1, TMR1IE		; TMR1 overflow interrupt
    ;bsf PIE1, RCIE		; USART receiver interrupt

    bcf STATUS,RP0		; select Register Page 0

    CLRF PIR1

    movlw b'00001110' 		; prescaler 1:1; OSC is on; Asynchonous input; Source - external clock; Timer is off
    movwf T1CON

    bsf INTCON, PEIE
    bsf INTCON, GIE 		; Enable all Interrupts

    movlw 0x80
    movwf TMR1H 		; 1 Second Overflow
    clrf  TMR1L			; clear to start again

    BSF T1CON, TMR1ON 		; Turn Timer 1 ON



;===============================================================================
; PWM setup
;===============================================================================  
; not finished
 
        ; ШИМ Page 61 of datasheet
;    movlw	   	0x3F			; частота 0,6КГц
 ;   movwf	   	PR2
 
    
;	movlw		b'00101100'	  	;
;	movwf		CCP1CON			; активируем ШИМ правого двигателя  
 
;	clrf		TMR2			; очистка Таймера 2
;
;	clrf		CCPR1L			; скважность = 0% правого двигателя 
;
;	movlw		b'00101100'	  	;

;	movwf		CCP1CON			; активируем ШИМ правого двигателя 
;	   	  
;==== таймер ШИМ
;	bcf			PIR1,TMR2IF		; очистка флага прерывания таймера
;	
;	clrf		T2CON		  
;	bsf			T2CON,T2CKPS1	; prescaler = 16
;	bsf			T2CON,TMR2ON	; активируем таймер
 ;
 
;===============================================================================
; USART setup
;===============================================================================
 
;	movlw		0x0C            ; 19200 
	movlw		0x19		; 9600, calculated constant
	movwf		SPBRG

	movlw		b'00100100'     ; brgh = high (2)
	movwf		TXSTA           ; Asynchronous mode
    
	movlw		b'10010000'     ; Asynchronous mode
	movwf		RCSTA
    	clrf		USART_RECEIVED	
    
    
    
;===============================================================================
; Initial time to display - 0:00:00
;===============================================================================
;
    movlw NIXIE_ZERO		; To tell 74LS138 to display ZERO on NIXIE use NIXIE_ZERO
    movwf SECONDS_BCD		; NIXIE - ZERO seconds
    movwf MINUTES_BCD		; NIXIE - ZERO minutes
    movwf HOURS_BCD		; NIXIE - ZERO hours

    clrf PORTB_TMP

;===============================================================================
; Main loop
; infinite loop to serve indication on tubes
;===============================================================================
;

<<<<<<< HEAD
    movlw b'00100010' 
    movwf TRISA			; portA pins RA1 and RA5 are inputs, all the others are output 
=======
    movlw b'00100010'
    movwf TRISA             ; portA pins RA1 and RA5 are inputs, all the others are output
>>>>>>> origin/master
    movf SECONDS_BCD, w			; select seconds data for subroutine
    movwf VAL_FOR_INDICATION		; move it into tmp reg to use inside subroutine

    ; set demultiplexor
    bsf PORTB_TMP, _74LS138_A2_on_pB
    bcf PORTB_TMP, _74LS138_A1_on_pB

    call TIME_INDICATION		; show seconds on tubes

    call WASTE_TIME_DYNAMICALLY		; need some time before turning nex tube on

    ; MINUTES
    movf MINUTES_BCD, W			; select minutes data for subroutine
    movwf VAL_FOR_INDICATION		; move it into tmp reg to use inside subroutine

    ; set demultiplexor
    bcf PORTB_TMP, _74LS138_A2_on_pB
    bsf PORTB_TMP, _74LS138_A1_on_pB

    call TIME_INDICATION		; show minutes on tubes

    call WASTE_TIME_DYNAMICALLY		; need some time before turning nex tube on

    ; HOURS
    movf HOURS_BCD, W			; select houres data for subroutine
    movwf VAL_FOR_INDICATION		; move it into tmp reg to use inside subroutine

    ; set demultiplexor
    bcf PORTB_TMP, _74LS138_A2_on_pB	;
    bcf PORTB_TMP, _74LS138_A1_on_pB

    call TIME_INDICATION		; show houres on tubes

    call WASTE_TIME_DYNAMICALLY		; need some time before turning next tubes on

    goto MAIN_LOOP


;===============================================================================
; Subroutine to show either houres, minutes or seconds
;===============================================================================
TIME_INDICATION:


    movlw NIXIE_ZERO

    subwf VAL_FOR_INDICATION, F		; get coorect values for indication

    clrf PORTA_TMP
    bsf PORTA_TMP, _74LS138_A0_on_pA	; lower nibble first

showDozensOnSecondPass

    movf PORTA, W			; preserve some PORTA bits from changing
    andlw b'00100011'			; RA0 RA1 RA5
    iorwf PORTA_TMP, F			;


    ;btfsc PORTA_TMP, _74LS138_A0_on_pA	; если бит PORTA_TMP выставлен, то организуем вывод единиц. так как вывод производится
    swapf VAL_FOR_INDICATION, F		; из старшего разряда, то необходимо обменять полубайты
					;для вывода десятков обмен полубайтов не производим

    ; get PORTA output bits for 155ID1 (schematics Rev.3)
    ; it is connected on RA3 RA4 RA6 RA7, PORTA mask is 11011000
    movf VAL_FOR_INDICATION, W

    andlw b'00110000'			; need to get this bits from upper nibble and rotate tem left

    movwf INDICATION_TMP1
    rrf INDICATION_TMP1, F		; mask is 00011000

    movf VAL_FOR_INDICATION, W
    andlw b'11000000'			;

    iorwf INDICATION_TMP1, W		; mask is 11011000

    ; Now get output bits fot 155ID1 in PORTA
    iorwf PORTA_TMP, F			;


    ; Now get PORTB bits

    ; 74LS138 has 000 or 001 on A2 A1 A0 inputs respectfully
    ; so PORTB look like xx00xxxx in this case
    movf PORTB_TMP, W			;

    ;Checking weather we work with houres
    andlw b'00110000'			; mask for Houres to show on tubes

    btfss STATUS, Z			; if Z=1 then we should check if decades of houres should be displayed
    goto Set_PORTx_for_DISPLAY

    btfsc PORTA_TMP, _74LS138_A0_on_pA	; (RA2) = 0 decades of houres should be displayed.
    goto Set_PORTx_for_DISPLAY

    movwf INDICATION_TMP1 		; save data for PORTB from W

    ; 00 houres is 00000110
    ; but we shoul show only 0, so upper nibble is skipped in this case
    movf PORTA_TMP, W			;
    andlw b'11011000'			;

    btfsc STATUS, Z			; Z = 1 means we have no zero in houres decades which is not to be shown on tube
    goto END_TIME_INDICATION

    movf INDICATION_TMP1, W		; move PORTB bits data back to W


Set_PORTx_for_DISPLAY

    ; W contains bits for PORTB
    bcf PORTB, _74LS138_A2_on_pB	; set PORTB to demultiplexor outputs
    bcf PORTB, _74LS138_A1_on_pB	;
    iorwf PORTB, F

    movf PORTA_TMP, W			; Set PORTA outputs
    movwf PORTA				;

    clrf PORTA_TMP			;

    btfss PORTA, _74LS138_A0_on_pA	; if set then go to display decades on tube
    goto END_TIME_INDICATION

    call WASTE_TIME_DYNAMICALLY		; need some time before turning next tubes on
    goto showDozensOnSecondPass

END_TIME_INDICATION
    return


;===============================================================================
; * Special routine for all unimportant stuff
;===============================================================================
WASTE_TIME_DYNAMICALLY:		;

    ; Not good to waste time, we have 1.5 ms so

    ; Good idea to add USART buffers check here.
    ; Check for power is present. If not -> sleep with ISR on Timer1 active only
    ; Comparator -> ADC lightness check

    goto BUTTONS 

; approximately 1.5 ms
    movlw 0x05
    movwf Delay2
dl_set
    movlw 0x3E			;1496 инструкций	;7D - для 1 мс ; 3E - 0,5мс ;BB - 1,5мс
    movwf Delay1

dl_loop
    decfsz    Delay1,f      ; Waste time.
    goto      dl_loop	    ;
    decfsz    Delay2,f      ;
    goto      dl_set	    ;
                            ;

    return

BUTTONS:
;===============================================================================
; * Button debounce and further process routine
;===============================================================================
;

    movf PORTA, W	    ; save current PORTA output pin state
    movwf PORTA_TMP

    bsf STATUS,RP0	    ; select Register Page 1
    ; PORTA pins RA1 RA3 RA4 RA5 RA6 RA7 are inputs for now
    movlw b'11111010'
    movwf TRISA
    bcf STATUS,RP0	    ; select Register Page 0

    movf PORTA, W	    ; read PORTA inputs
    movwf PORTA_IN_TMP

    bsf STATUS,RP0	    ; select Register Page 1
    ; restore PORTA outputs pins RA1 and RA5 are inputs, all the others are output
    movlw b'00100010'
    movwf TRISA
    bcf STATUS,RP0	    ; select Register Page 0

    movf PORTA_TMP, W
    movwf PORTA		    ; restore current PORTA output state

    ;; now debounce. we do it every indication loop (1.5 Ms) no to waste time


    ; debounce BT_MODE
    btfss PORTA_IN_TMP, BT_MODE
    goto clear_BT_MODE	    ; clear counter, assume button was never pushed

    incf BUTTONS_COUNT_BT_MODE, W
    movwf BUTTONS_COUNT_BT_MODE

    xorlw 5		    ; looking for 5 in a row

    btfss STATUS, Z	    ; if Z=1 then we have 5 in row
    goto check_BT_INC

    bsf BUTTONS_UP, BT_MODE ; BT_MODE is pushed by user and debounced

clear_BT_MODE
    clrf BUTTONS_COUNT_BT_MODE


    ; debounce BT_INC
check_BT_INC

    btfss PORTA_IN_TMP, BT_INC
    goto clear_BT_INC

    incf BUTTONS_COUNT_BT_INC, W
    movwf BUTTONS_COUNT_BT_INC

    xorlw 5		    ; looking for 5 in a row

    btfss STATUS, Z	    ; if Z=1 then we have 5 in row
    goto check_BT_DEC

    bsf BUTTONS_UP, BT_INC ; BT_INC is pushed by user and debounced

clear_BT_INC
    clrf BUTTONS_COUNT_BT_INC


    ; debounce BT_DEC
check_BT_DEC

    btfss PORTA_IN_TMP, BT_DEC
    goto clear_BT_DEC

    incf BUTTONS_COUNT_BT_DEC, W
    movwf BUTTONS_COUNT_BT_DEC

    xorlw 5		    ; looking for 5 in a row

    btfss STATUS, Z	    ; if Z=1 then we have 5 in row
    goto check_BT_SNOOZE

    bsf BUTTONS_UP, BT_DEC ; BT_INC is pushed by user and debounced

clear_BT_DEC
    clrf BUTTONS_COUNT_BT_DEC


    ; debounce BT_SNOOZE
check_BT_SNOOZE

    btfss PORTA_IN_TMP, BT_SNOOZE
    goto clear_BT_SNOOZE

    incf BUTTONS_COUNT_BT_SNOOZE, W
    movwf BUTTONS_COUNT_BT_SNOOZE

    xorlw 5		    ; looking for 5 in a row

    btfss STATUS, Z	    ; if Z=1 then we have 5 in row
    goto process_BUTTONS_UP

    bsf BUTTONS_UP, BT_SNOOZE ; BT_INC is pushed by user and debounced

clear_BT_SNOOZE
    clrf BUTTONS_COUNT_BT_SNOOZE


process_BUTTONS_UP
    ;PORTA, RA0 - test led output. temporary.



    comf BUTTONS_UP, W

    btfsc STATUS, Z	; Z=1 means nothing have been pushed and debounced
    goto end_debounce

    ; debug
    btfsc PORTA, RA0
    goto invertLED

    bsf PORTA, RA0
<<<<<<< HEAD
    movlw b'11000011'		; RB7-RB6(RTC crystal), RB1(RX) and RB0(INT for PWM control) = input, others output 
    movwf TRISB 
=======
    movlw b'11000010'       ; RB7-RB6(RTC crystal), RB1(RX) and RB0(INT for PWM control) =input, others output
    movwf TRISB
>>>>>>> origin/master
    bcf PORTA, RA0


;BT_MODE
;BT_INC
;BT_DEC
;BT_SNOOZE

    clrf BUTTONS_UP

end_debounce
    return


;===============================================================================
; * Delays
;===============================================================================


;---
; approximately 197 ms
;-----

DELAY_197_MS:

    decfsz    Delay1,f       ; Waste time.
    goto      DELAY_197_MS    ; The Inner loop takes 3 instructions per loop * 256 loopss = 768 instructions
    decfsz    Delay2,f       ; The outer loop takes and additional 3 instructions per lap * 256 loops
    goto      DELAY_197_MS    ; (768+3) * 256 = 197376 instructions / 1M instructions per second = 0.197 sec.
                              ; call it two-tenths of a second.

    return

<<<<<<< HEAD
; ------------------------------------ 
; 
=======
; ------------------------------------

>>>>>>> origin/master
