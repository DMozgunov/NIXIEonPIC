
; PIC16F628A Configuration Bit Settings

; Assembly source line config statements

#include "p16f628a.inc"

; CONFIG
; __config 0xFF18
 __CONFIG _FOSC_INTOSCIO & _WDTE_OFF & _PWRTE_OFF & _MCLRE_OFF & _BOREN_OFF & _LVP_OFF & _CPD_OFF & _CP_OFF

;
;===============================================================================
; Some sugar
;===============================================================================
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

; Ised during time changing
HIDE_CURRENT EQU 7	    ; helps blink selected value
HOURES EQU 7		    ; select houres to blink
MINUTES EQU 6		    ;
SECONDS EQU 5		    ;
TIME_CHANGE_MODE EQU 0	    ; indicates that we are changing time

PRESS_COUNT EQU 10	    ; look for this count in a row to debounce buttons

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

; BUTTONS_UP	    ; Buttons, pushed by user and debounced in software
; same bits
BT_MODE equ RA3
BT_INC equ RA4
BT_DEC equ RA6
BT_SNOOZE equ RA7


Debug EQU TRUE
;Debug EQU FALSE

;===============================================================================
; Store DATA here
;===============================================================================

    cblock 0x20		; (max 80 Bytes)
	Delay1
	Delay2

	USART_RECEIVED

	SECONDS_BCD		; Storing seconds for display on NIXIE ()
	MINUTES_BCD
	HOURES_BCD

	SECONDS_BCD_DEBUG
	MINUTES_BCD_DEBUG
	HOURES_BCD_DEBUG

	PORTA_TMP		; Used to save PORTA value during indication process. Used for output values
	PORTB_TMP		; This is tmp reg for PORTB set up during dynamic indication

	PORTA_IN_TMP	;

	; Buttons debounce
	BUTTONS_COUNT_BT_MODE   ; counters for debounce
	BUTTONS_COUNT_BT_INC
	BUTTONS_COUNT_BT_DEC
	BUTTONS_COUNT_BT_SNOOZE

	BUTTONS_MODE	; Helps to determine values changing mode
			    ; bit (if set)
			    ; 0 - Enable time changing
			    ; 7 - change houres
			    ; 6 - change minutes
			    ; 5 - change seconds
			    ;
	; three values to use in subroutine, changing time on buttons pushed
	OVERFLOW_VAL	    ; Used to pass into subroutine upper unexisted value to check equal
			    ; i.e. 24 houres and 60 secons is what we never see on clock
	CHANGE_VAL	    ; Value to change
	;UNDER_VAL	    ; Used to pass into subroutine value which shold be assigned if zero was decreased
			    ; i.e. 0 houres - 23 houres
			    ; 00 minutes/seconds - 59 minutes/seconds

	BUTTONS_UP	    ; Buttons, pushed by user and debounced in software

	VAL_FOR_INDICATION  ; This reg stores value of seconds, minutes or houres
			    ; which is to be shown on tubes

	INDICATION_TMP1	; temporary register to hold PORTA output bits

	BLINK_COUNTER	; Helps to blink tubes while changing values
			; bits (if set)
			; 7 - 1 hide current values(either houres, minutes)
			; or second. Selection via BUTTONS_MODE
			; 3:0 - counter

    endc


    cblock 0x70     ; put these up in unbanked RAM (max 16 Bytes)
	W_Save
	STATUS_Save
    endc

;===============================================================================
; executable CODE is here
;===============================================================================
RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    MAIN_PROGRAM           ; go to beginning of program

ISR       CODE    0x0004	    ; Interrupts

    movwf W_Save		; Save context
    movf STATUS,w
    movwf STATUS_Save

    bcf STATUS,RP0		; select Register Page 0

    if ( Debug )

	call LED_debug

	movlw 'I'
	call SendByte
	call EndLine
    endif


    ; Select Interrupt to process
    btfsc PIR1,TMR1IF           ; Check Timer 1 - one more second to go
    goto ServiceTimer1

    ; add more if needed

    goto ExitISR

;===============================================================================
; Timer1 serves RTC needs. Routine prepares time data to display
;===============================================================================
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

    incf HOURES_BCD, F

    movlw NIXIE_NINE			; reached 9 and have to zero lower nibble
    andwf HOURES_BCD, W

    btfss STATUS, Z
    goto HOURS_CHECK

    movlw NIXIE_ZERO
    iorwf HOURES_BCD, F

HOURS_CHECK
    movlw b'00101010'
    subwf HOURES_BCD, W

    btfss STATUS, Z			; we reached 24 houres, so time to 00:00:00
    goto ExitISR

    movlw NIXIE_ZERO			;
    movwf HOURES_BCD


ExitISR
    movf      STATUS_Save,w       ; Restore context
    movwf     STATUS
    swapf     W_Save,f            ; swapf doesn't affect Status bits, but MOVF would
    swapf     W_Save,w


    retfie


MAIN_PROG CODE                      ; let linker place main program


MAIN_PROGRAM:


;===============================================================================
; Initial time to display - 0:00:00
;===============================================================================
;
    movlw NIXIE_ZERO		; To tell 74LS138 to display ZERO

    movwf SECONDS_BCD		; NIXIE - ZERO seconds
    movwf MINUTES_BCD		; NIXIE - ZERO minutes
    movwf HOURES_BCD		; NIXIE - ZERO hours

;===============================================================================

    movlw b'00010111'

    movwf SECONDS_BCD_DEBUG	; NIXIE - ZERO seconds
    movwf MINUTES_BCD_DEBUG	; NIXIE - ZERO minutes
    movwf HOURES_BCD_DEBUG	; NIXIE - ZERO hours

    ; Buttons debounce
    clrf BUTTONS_COUNT_BT_MODE   ; counters for debounce
    clrf BUTTONS_COUNT_BT_INC
    clrf BUTTONS_COUNT_BT_DEC
    clrf BUTTONS_COUNT_BT_SNOOZE

    clrf PORTB_TMP


;===============================================================================
; Setup
;===============================================================================
Initial_Setup:			; On mains power up, first one or from sleep(which is on battery)

    bcf STATUS, RP0		; select Register Page 0
    bcf STATUS, RP1		;

    movlw 7
    movwf CMCON			; CMCON=7 set comperators off

    bsf STATUS, RP0		; select Register Page 1

    movlw b'00100010'
    movwf TRISA			; portA pins RA1 and RA5 are inputs, all the others are output

    movlw b'11000011'		; RB7-RB6(RTC crystal), RB1(RX) and RB0(INT for PWM control) = input, others output
    movwf TRISB


;------------------------------------
; USART setup part 1
;------------------------------------

    ;movlw  0x0C		; 19200
    movlw 0x19			; 9600, calculated constant
    movwf SPBRG

    movlw b'00100100'		; brgh = high (2)
    movwf TXSTA			; Asynchronous mode

; ------------------------------------
; TIMER1 and interrupts SETUP
; ------------------------------------
;
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


;------------------------------------
;  INITIAL PORTx and data registers setup
;------------------------------------
    bcf STATUS, RP0		; select Register Page 0
    bcf STATUS, RP1
    clrf PORTA
    clrf PORTB

    clrf BUTTONS_MODE ; after reset it should be clean


;------------------------------------
; USART setup part 2
;------------------------------------

    movlw   b'10010000'		; Asynchronous mode
    movwf   RCSTA
    clrf    USART_RECEIVED



;===============================================================================
; Main loop
; infinite loop to serve indication on tubes
;===============================================================================
;
MAIN_LOOP:


    ;BLINK_COUNTER bit 7  (HIDE_CURRENT)

    ;BUTTONS_MODE	; Helps to determine values changing mode
			    ; bit (if set)
			    ; 0 - Enable time changing (TIME_CHANGE_MODE)
			    ; 7 - change houres (HOURES)
			    ; 6 - change minutes (MINUTES)
			    ; 5 - change seconds (SECONDS)


    movlw b'00001111'
    andwf BLINK_COUNTER, W

    ; When counter becomes 0 it starts again after inverting bit which is used to
    ; periodically hide values from display on tubes

    btfss STATUS, Z
    goto blink_decrement

    movlw b'00000010'
    iorwf BLINK_COUNTER, F

    ; invert bit that shows/hides current values on tubes making them blink
    btfss BLINK_COUNTER, HIDE_CURRENT
    goto $+3

    bcf BLINK_COUNTER, HIDE_CURRENT
    goto $+2

    bsf BLINK_COUNTER, HIDE_CURRENT

blink_decrement
    decf BLINK_COUNTER, F	    ;



    if ( Debug )

	;call BIIG_DELAY_for_DEBUG

	;call LED_debug

	;movlw 'm'
	;call SendByte
	;call EndLine


	;call ReceiveByte
	;movf USART_RECEIVED, W

	;;comf USART_RECEIVED, F
	;btfss STATUS,Z

	;call SendByte
	;call EndLine

	;clrf USART_RECEIVED

	;call BUTTONS

	;call TEST_DEMULTIPLEXING

	; SECONDS
	movf SECONDS_BCD_DEBUG, w		; select seconds data for subroutine
	movwf VAL_FOR_INDICATION		; move it into tmp reg to use inside subroutine

	; set demultiplexor
	bsf PORTB_TMP, _74LS138_A2_on_pB
	bcf PORTB_TMP, _74LS138_A1_on_pB

	call TIME_INDICATION			; show seconds on tubes

	call WASTE_TIME_DYNAMICALLY		; need some time before turning nex tube on

	; MINUTES
	movf MINUTES_BCD_DEBUG, W		; select minutes data for subroutine
	movwf VAL_FOR_INDICATION		; move it into tmp reg to use inside subroutine

	; set demultiplexor
	bcf PORTB_TMP, _74LS138_A2_on_pB
	bsf PORTB_TMP, _74LS138_A1_on_pB

	call TIME_INDICATION			; show minutes on tubes

	call WASTE_TIME_DYNAMICALLY		; need some time before turning nex tube on

	; HOURS
	movf HOURES_BCD_DEBUG, W		; select houres data for subroutine
	movwf VAL_FOR_INDICATION		; move it into tmp reg to use inside subroutine

	; set demultiplexor
	bcf PORTB_TMP, _74LS138_A2_on_pB
	bcf PORTB_TMP, _74LS138_A1_on_pB

	call TIME_INDICATION			; show houres on tubes

	call WASTE_TIME_DYNAMICALLY		; need some time before turning next tubes on

    endif

    if (!Debug)

	 ; SECONDS
	movf SECONDS_BCD, w			; select seconds data for subroutine
	movwf VAL_FOR_INDICATION		; move it into tmp reg to use inside subroutine

	; set demultiplexor
	bsf PORTB_TMP, _74LS138_A2_on_pB
	bcf PORTB_TMP, _74LS138_A1_on_pB

	; before indicate anything check if it should be displayed

	btfsc BUTTONS_MODE, SECONDS		; if it should blink
	btfss BLINK_COUNTER, HIDE_CURRENT	; select to show or to hide
	call TIME_INDICATION			; show seconds on tubes

	call WASTE_TIME_DYNAMICALLY		; need some time before turning nex tube on

	; MINUTES
	movf MINUTES_BCD, W			; select minutes data for subroutine
	movwf VAL_FOR_INDICATION		; move it into tmp reg to use inside subroutine

	; set demultiplexor
	bcf PORTB_TMP, _74LS138_A2_on_pB
	bsf PORTB_TMP, _74LS138_A1_on_pB

	btfsc BUTTONS_MODE, MINUTES		; if it should blink
	btfss BLINK_COUNTER, HIDE_CURRENT	; select to show or to hide
	call TIME_INDICATION			; show minutes on tubes

	call WASTE_TIME_DYNAMICALLY		; need some time before turning nex tube on

	; HOURS
	movf HOURES_BCD, W			; select houres data for subroutine
	movwf VAL_FOR_INDICATION		; move it into tmp reg to use inside subroutine

	; set demultiplexor
	bcf PORTB_TMP, _74LS138_A2_on_pB	;
	bcf PORTB_TMP, _74LS138_A1_on_pB

	btfsc BUTTONS_MODE, HOURES		; if it should blink
	btfss BLINK_COUNTER, HIDE_CURRENT	; select to show or to hide
	call TIME_INDICATION		; show houres on tubes

	call WASTE_TIME_DYNAMICALLY		; need some time before turning next tubes on

    endif

    call BUTTONS				; not too often

    GOTO MAIN_LOOP				; loop until power off
;===============================================================================
; Main loop end
; infinite loop to serve indication on tubes
;===============================================================================


;===============================================================================
; * Special routine for all unimportant stuff
;===============================================================================
WASTE_TIME_DYNAMICALLY:		;

    ; Not good to waste time, we have 1.5 ms so

    ; Good idea to add USART buffers check here.
    ; Check for power is present. If not -> sleep with ISR on Timer1 active only
    ; Comparator -> ADC lightness check

    ; approximately 1.5 ms

    ;call BUTTONS	;

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

;===============================================================================
; Subroutine to show either houres, minutes or seconds
;===============================================================================
TIME_INDICATION:

    movlw NIXIE_ZERO

    subwf VAL_FOR_INDICATION, F		; get correct values for indication

;    if(Debug)
;
;	movlw '_'
;	call SendByte
;
;       	movf VAL_FOR_INDICATION, W
;;	addlw b'00110000'		; 0 in ASKII,
;
;	call SendByte			; readable value of current indication value
;
;	;call LED_debug
;    endif

    clrf PORTA_TMP
    bsf PORTA_TMP, _74LS138_A0_on_pA	; lower nibble first

showDozensOnSecondPass

    movf PORTA, W			; preserve some PORTA bits from changing
    andlw b'00100011'			; RA0 RA1 RA5

    iorwf PORTA_TMP, F			;

;    if(Debug)
;       	movf PORTA_TMP, W
;	;addlw b'00110000'		; 0 in ASKII,
;
;	call SendByte			; readable value of current indication value
;    endif

    ;btfsc PORTA_TMP, _74LS138_A0_on_pA	; если бит PORTA_TMP выставлен, то организуем вывод единиц. так как вывод производится
    swapf VAL_FOR_INDICATION, F		; из старшего разряда, то необходимо обменять полубайты
					;для вывода десятков обмен полубайтов не производим

    ; get PORTA output bits for 155ID1 (schematics Rev.3)
    ; it is connected on RA3 RA4 RA6 RA7, PORTA mask is 11011000
    movf VAL_FOR_INDICATION, W
    andlw b'00110000'			; need to get this bits from upper nibble and rotate tem left

    movwf INDICATION_TMP1

    bcf STATUS, C			; it has 1 somehow. must be cleared manually

    rrf INDICATION_TMP1, F		; mask is 00011000

;    if(Debug)
;       	movf INDICATION_TMP1, W
;	;addlw b'00110000'		; 0 in ASKII,
;
;	call SendByte			; readable value of current indication value
;    endif

    movf VAL_FOR_INDICATION, W
    andlw b'11000000'			;

    iorwf INDICATION_TMP1, W		; mask is 11011000

;    if(Debug)
;       	movf INDICATION_TMP1, W
;	;addlw b'00110000'		; 0 in ASKII,
;
;	call SendByte			; readable value of current indication value
;    endif

    ; Now get output bits fot 155ID1 in PORTA
    iorwf PORTA_TMP, F			;


;    if(Debug)
;       	movf PORTA_TMP, W
;	;addlw b'00110000'		; 0 in ASKII,
;
;	call SendByte			; readable value of current indication value
;    endif

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

    ; 00 houres is 00000000
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

;    if(Debug)
;       	movf PORTA, W
;	;addlw b'00110000'		; 0 in ASKII,
;
;	call SendByte			; readable value of current indication value
;    endif

    clrf PORTA_TMP			;

    btfss PORTA, _74LS138_A0_on_pA	; if set then go to display decades on tube
    goto END_TIME_INDICATION

    call WASTE_TIME_DYNAMICALLY		; need some time before turning next tubes on
    goto showDozensOnSecondPass

END_TIME_INDICATION

    if(Debug)

	movlw '_'
	call SendByte
    endif
    return


;===============================================================================
; * Button debounce and further process routine
;===============================================================================
;
BUTTONS:

    movf PORTA, W	    ; save current PORTA output pin state
    movwf PORTA_TMP

    ; PORTA pins RA1 RA3 RA4 RA5 RA6 RA7 are inputs for now
    movlw b'11111010'

    bsf STATUS, RP0	    ; select Register Page 1
    movwf TRISA

    bcf STATUS, RP0	    ; select Register Page 0

    movf PORTA, W	    ; read PORTA inputs

    movwf PORTA_IN_TMP

    ; restore PORTA outputs pins .
    bsf STATUS, RP0	    ; select Register Page 1
    movlw b'00100010'	    ; PORTA pins RA1 RA5 are inputs
    movwf TRISA
    bcf STATUS, RP0	    ; select Register Page 0

    ; restore current PORTA output state
    movf PORTA_TMP, W
    movwf PORTA


    ;; now debounce. we do it every indication loop (1.5 Ms) no to waste time

    ; debounce BT_MODE
    btfss PORTA_IN_TMP, BT_MODE
    goto clear_BT_MODE	    ; clear counter, assume button was never pushed

    if(Debug)
       	movlw '1'
	call SendByte
    endif

    incf BUTTONS_COUNT_BT_MODE, W
    movwf BUTTONS_COUNT_BT_MODE

    xorlw PRESS_COUNT		    ; looking for some in a row

    btfss STATUS, Z	    ; if Z=1 then we have 5 in row
    goto check_BT_INC

    bsf BUTTONS_UP, BT_MODE ; BT_MODE is pushed by user and debounced

clear_BT_MODE
    clrf BUTTONS_COUNT_BT_MODE


    ; debounce BT_INC
check_BT_INC

    btfss PORTA_IN_TMP, BT_INC
    goto clear_BT_INC

    if(Debug)
       	movlw '2'
	call SendByte
    endif

    incf BUTTONS_COUNT_BT_INC, W
    movwf BUTTONS_COUNT_BT_INC

    xorlw PRESS_COUNT		    ; looking for some in a row

    btfss STATUS, Z	    ; if Z=1 then we have 5 in row
    goto check_BT_DEC

    bsf BUTTONS_UP, BT_INC ; BT_INC is pushed by user and debounced

clear_BT_INC
    clrf BUTTONS_COUNT_BT_INC


    ; debounce BT_DEC
check_BT_DEC

    btfss PORTA_IN_TMP, BT_DEC
    goto clear_BT_DEC

    if(Debug)
       	movlw '3'
	call SendByte
    endif

    incf BUTTONS_COUNT_BT_DEC, W
    movwf BUTTONS_COUNT_BT_DEC

    xorlw PRESS_COUNT		    ; looking for some in a row

    btfss STATUS, Z	    ; if Z=1 then we have 5 in row
    goto check_BT_SNOOZE

    bsf BUTTONS_UP, BT_DEC ; BT_INC is pushed by user and debounced

clear_BT_DEC
    clrf BUTTONS_COUNT_BT_DEC


    ; debounce BT_SNOOZE
check_BT_SNOOZE

    btfss PORTA_IN_TMP, BT_SNOOZE
    goto clear_BT_SNOOZE

    if(Debug)
       	movlw '4'
	call SendByte
    endif

    incf BUTTONS_COUNT_BT_SNOOZE, W
    movwf BUTTONS_COUNT_BT_SNOOZE

    xorlw PRESS_COUNT		    ; looking for some in a row

    btfss STATUS, Z	    ; if Z=1 then we have 5 in row
    goto process_BUTTONS_UP

    bsf BUTTONS_UP, BT_SNOOZE ; BT_INC is pushed by user and debounced

clear_BT_SNOOZE
    clrf BUTTONS_COUNT_BT_SNOOZE



process_BUTTONS_UP

    ; check for MODE button
    btfss BUTTONS_UP, BT_MODE
    goto check_INC

    bcf BUTTONS_UP, BT_MODE	; value processed, clear

    if(Debug)
       	movlw 'm'
	call SendByte
    endif

    ; HOURES have already been selected, move to MINUTES
    btfss BUTTONS_MODE, HOURES
    goto $+4

    bcf BUTTONS_MODE, HOURES
    bsf BUTTONS_MODE, MINUTES

    goto END_BUTTONS

     ; MINUTES have already been selected, move to SECONDS
    btfss BUTTONS_MODE, MINUTES
    goto $+4

    bcf BUTTONS_MODE, MINUTES
    bsf BUTTONS_MODE, SECONDS

    goto END_BUTTONS

    ; SECONDS have already been selected, move to SECONDS
    btfss BUTTONS_MODE, SECONDS
    goto $+3 ; first time here! need to set bit HOURES

    clrf BUTTONS_MODE	    ; changes have been made, exit this mode
    goto END_BUTTONS

    ; firstr entry to MODE
    bsf BUTTONS_MODE, HOURES
    bsf BUTTONS_MODE, TIME_CHANGE_MODE

    goto END_BUTTONS

check_INC
    btfss BUTTONS_UP, BT_INC
    goto check_DEC

    btfss BUTTONS_MODE, TIME_CHANGE_MODE    ; Not in time change mode. exit
    goto END_BUTTONS

    if(Debug)
       	movlw 'i'
	call SendByte
    endif

    call change_selected_value

    bcf BUTTONS_UP, BT_INC	; value processed, clear

    goto END_BUTTONS

check_DEC
    btfss BUTTONS_UP, BT_DEC
    goto check_SNOOZE

    btfss BUTTONS_MODE, TIME_CHANGE_MODE    ; Not in time change mode. exit
    goto END_BUTTONS

    if(Debug)
       	movlw 'd'
	call SendByte
    endif

    call change_selected_value

    bcf BUTTONS_UP, BT_DEC	; value processed, clear

    goto END_BUTTONS

    ; if alarms shall be implemented
check_SNOOZE
    btfss BUTTONS_UP, BT_SNOOZE
    goto END_BUTTONS

    if(Debug)
       	movlw 's'
	call SendByte
    endif

    ; when you have an alarm put some code here

    bcf BUTTONS_UP, BT_SNOOZE	; value processed, clear
    ;goto END_BUTTONS

END_BUTTONS

    return


; if user have selected somthing to change and pushed INC or DEC we
; process it here
change_selected_value:

    movlw b'01100110'	    ; for minutes and seconds it is the same
    movwf OVERFLOW_VAL	    ; moved here not to duplicate below

    btfss BUTTONS_MODE, HOURES
    goto change_minutes

    movlw b'00101010'	    ; replace with another constant, sutable for houres
    movwf OVERFLOW_VAL

    movf HOURES_BCD, W
    movwf CHANGE_VAL

    call INC_or_DEC_current_time_value

    movwf HOURES_BCD	    ; upper subroutine leaves new value in W

    goto end_change_selected_value

change_minutes
    btfss BUTTONS_MODE, MINUTES
    goto change_seconds

   ; movlw b'01100110'
   ; movwf OVERFLOW_VAL

    movf MINUTES_BCD, W
    movwf CHANGE_VAL

    call INC_or_DEC_current_time_value

    movwf MINUTES_BCD	    ; upper subroutine leaves new value in W

    goto end_change_selected_value

change_seconds
    btfss BUTTONS_MODE, SECONDS
    goto end_change_selected_value

    ; movlw b'01100110'
    ; movwf OVERFLOW_VAL

    movf SECONDS_BCD, W
    movwf CHANGE_VAL

    call INC_or_DEC_current_time_value

    movwf SECONDS_BCD	    ; upper subroutine leaves new value in W

end_change_selected_value
    return

; this is subroutine for BUTTONS routine.
; it takes one of houres, minutes or seconds values as input
; changes it accordingly and checks for stepping over boundaries
INC_or_DEC_current_time_value:
    ; Return value is in W. Use it directly

    btfss BUTTONS_UP, BT_INC
    goto dec_value

    incf HOURES_BCD, F
    ; what if we've got 24 houres, 60 minutes or seconds

    movf OVERFLOW_VAL, W	;
    subwf CHANGE_VAL, W		;

    btfss STATUS, Z		;
    goto end_INC_or_DEC

    movlw NIXIE_ZERO		;
    ;movwf CHANGE_VAL		;


dec_value
    btfss BUTTONS_UP, BT_DEC
    goto end_INC_or_DEC

    decf HOURES_BCD, F
    ; what if we have 0 houres, minutes, seconds

    movlw b'00000101'		;= (NIXIE_ZERO - 1). Constant for all three variants
    subwf CHANGE_VAL, W

    btfss STATUS, Z		; we decreased 0 houres so it shold be 23
    goto end_INC_or_DEC

    decf OVERFLOW_VAL, W	; 23 for houres, or 59 for minutes/seconds in NIXIE BCD
				;

    ;movwf CHANGE_VAL		;
end_INC_or_DEC
    return



;===============================================================================
; Delays
;===============================================================================

DELAY_197_MS:

    decfsz    Delay1,f       ; Waste time.
    goto      DELAY_197_MS    ; The Inner loop takes 3 instructions per loop * 256 loopss = 768 instructions
    decfsz    Delay2,f       ; The outer loop takes and additional 3 instructions per lap * 256 loops
    goto      DELAY_197_MS    ; (768+3) * 256 = 197376 instructions / 1M instructions per second = 0.197 sec.
                              ; call it two-tenths of a second.

    return

;===============================================================================
; USART
;===============================================================================

ReceiveByte:
    btfss   PIR1, RCIF		;
    goto    NothingReceived

    movf    RCREG, W		;

    movwf  USART_RECEIVED
    call SendByte		; confirm receiption

NothingReceived

    btfsc  RCSTA, OERR		; buffer overflow
    bcf	    RCSTA, OERR


    return



SendByte:
    ; Byte to send is in W
    bsf	    STATUS, RP0		;

StillNotSent			; check weather previous byte was sent
    btfss   TXSTA,TRMT		;
    goto    StillNotSent

    bcf	    STATUS,RP0

    movwf   TXREG		; send new byte

    return


EndLine:
        movlw  0x0D ; CR
        call SendByte
        movlw  0x0A ; LF
        call SendByte

	return
    ;IfRS232Forward:
    ;movf      	RS232Received,w
    ;xorlw     	'8'
    ;btfss     	STATUS,Z
    ;goto      	IfRS232Backward

    ;call      	L_FORWARD_R_FORWARD   	; Движение вперёд

    ;clrf		RS232Received			; чтобы не было циклических повторов выполнения одной и той же команды



;===============================================================================
; to DEBUG or not to..
;===============================================================================

    if (Debug)

TUBES_TEST_VALUES:
    ; generating values to display
    ; 99 99 99
    ; to
    ; 00 00 00
    ; values should change every 3 seconds

    ; NOT FINISHED YET!

    ;NIXIE_ZERO equ b'00000110'
    ;NIXIE_NINE equ b'00001111'

    movlw b'00000110'	    ; 00
;    subwf
    movf  SECONDS_BCD_DEBUG, W


    btfss STATUS, Z
    goto next_tubes_test_values

    movlw b'10011111'	    ; 99
    movwf SECONDS_BCD_DEBUG

    goto set_tubes_test_values


    ; check if 3 seconds passed


    ; prepare next values to display
next_tubes_test_values

    decf SECONDS_BCD_DEBUG, F
    swapf SECONDS_BCD_DEBUG, F
    decf SECONDS_BCD_DEBUG, F
    swapf SECONDS_BCD_DEBUG, F

    movf SECONDS_BCD_DEBUG, W
set_tubes_test_values
    movwf   MINUTES_BCD_DEBUG
    movwf   HOURES_BCD_DEBUG


    return

;
;TEST_DEMULTIPLEXING:
; helps to check connection on ports only
; need smth more advanced

;    ; Seconds
;
;
;    movlw b'00010000'
;    movwf PORTB
;
;
;    movlw b'00000101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00001100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00010101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00011100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01000101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01001100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01010101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01011100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'10000101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'10001100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    ; проверка секунд (десятки)
;
;
;
;    movlw b'00000001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00001000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00010001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00011000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01000001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01001000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01010001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01011000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'10000001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'10001000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;
;
;    ; check minutes (least significant)
;
;    movlw b'00100000'
;    movwf PORTB
;
;
;    movlw b'00000101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00001100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00010101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00011100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01000101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01001100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01010101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01011100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'10000101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'10001100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    ; check minutes (most significant)
;
;    movlw b'00000001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00001000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00010001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00011000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01000001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01001000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01010001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01011000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'10000001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'10001000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;
;
;    ; check minutes (least significant)
;
;    movlw b'00000000'
;    movwf PORTB
;
;
;    movlw b'00000101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00001100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00010101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00011100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01000101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01001100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01010101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01011100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'10000101'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'10001100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    ; check minutes (most significant)
;
;    movlw b'00000001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00001000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00010001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00011000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01000001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01001000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01010001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'01011000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'10000001'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'10001000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;
;    ; dots/ additional lamps
;
;    movlw b'00110000'
;    movwf PORTB
;
;    movlw b'00000000'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    movlw b'00000100'
;    movwf PORTA
;
;    call BIIG_DELAY_for_DEBUG
;
;    return


BIIG_DELAY_for_DEBUG:

    call  DELAY_197_MS
    call  DELAY_197_MS
    call  DELAY_197_MS
    call  DELAY_197_MS
   ; call  DELAY_197_MS

    return

LED_debug:
    ;RA0 is our LED debug output for now
    btfss PORTA, RA0
    goto $+3

    bcf PORTA, RA0

    goto $+2
    bsf PORTA, RA0

    return

    endif



    END