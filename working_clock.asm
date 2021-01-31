; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
ALARM_BUTTON  equ P0.7
SOUND_OUT     equ P3.7
UPDOWN        equ P0.0

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
min_counter:  ds 1 ; minute counter
hour_counter: ds 1 ; hour counter

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
daytime:      dbit 1 ; am=1, pm=0

cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
;;ALARM NOISE
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	;cpl SOUND_OUT ; Connect speaker to P3.7!
	reti


;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
;;MAIN TIMER
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	;check if second has passed
	mov a,Count1ms+0
	cjne a,#low(1000),Timer2_ISR_done
	mov a,Count1ms+1
	cjne a,#high(1000),Timer2_ISR_done

	;its been 1seconds so set flag
	setb seconds_flag
	;cpl TR0 ;makes beeping noise
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, BCD_counter
	jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a
	cjne a,#0x3,cont
	
	;;its been 60sec
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Now clear the BCD counter
	mov BCD_counter, a
	setb TR2                ; Start timer 2
	mov a,min_counter
	add a,#0x1
	da a	
	mov min_counter,a
	
	cjne a,#0x59,cont
	;;its been an hour, reset mins and inc hour
	mov a,hour_counter
	cjne a,#0x12,check_daytime
	;;its 12:59 so we clear mins normally but we reset hours to 1
	clr a
	mov min_counter,a
	mov a,#0x1
	mov hour_counter,a
	sjmp cont
	
check_daytime:
	;;check if its 11:59 for datime switch
	cjne a,#0x11,clear_normal
	cpl daytime
	
clear_normal:
	clr a
	mov min_counter,a
	mov a,hour_counter
	add a,#0x1
	da a
	mov hour_counter,a
	
cont:	
	sjmp Timer2_ISR_done



Timer2_ISR_decrement:		;;reuse this to make it switch to alarm set display
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.

Timer2_ISR_done:
	pop psw
	pop acc
	reti
	

; These custom characters copied from https://cdn.instructables.com/ORIG/FGY/5J1E/GYFYDR5L/FGY5J1EGYFYDR5L.txt
Custom_Characters:
	WriteCommand(#40h) ; Custom characters are stored starting at address 40h
; Custom made character 0
	WriteData(#00111B)
	WriteData(#01111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 1
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
; Custom made character 2
	WriteData(#11100B)
	WriteData(#11110B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 3
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#01111B)
	WriteData(#00111B)
; Custom made character 4
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 5
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11110B)
	WriteData(#11100B)
; Custom made character 6
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 7
	WriteData(#11111B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	ret

; For all the big numbers, the starting column is passed in register R1
Draw_big_0:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)  
	WriteData(#1) 
	WriteData(#2)
	;WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)  
	WriteData(#4)  
	WriteData(#5)
	;WriteData(#' ')
	ret
	
Draw_big_1:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
;	WriteData(#1)
	WriteData(#' ')
	WriteData(#0)
	;WriteData(#' ')
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
;	WriteData(#4)
	WriteData(#' ')
	WriteData(#255)
;	WriteData(#4)
	WriteData(#' ')
	ret

Draw_big_2:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#6)
	WriteData(#6)
	WriteData(#2)
	;WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#7)
	WriteData(#7)
	;WriteData(#' ')
	ret

Draw_big_3:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#6)
	WriteData(#6)
	WriteData(#2)
	;WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#7)
	WriteData(#7)
	WriteData(#5)
	;WriteData(#' ')
	ret

Draw_big_4:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#4)
	WriteData(#2)
	;WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#255)
	;WriteData(#' ')
	ret

Draw_big_5:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#255)
	WriteData(#6)
	WriteData(#6)
	;WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#7)
	WriteData(#7)
	WriteData(#5)
	;WriteData(#' ')
	ret

Draw_big_6:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)
	WriteData(#6)
	WriteData(#6)
	;WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#7)
	WriteData(#5)
	;WriteData(#' ')
	ret

Draw_big_7:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#1)
	WriteData(#1)
	WriteData(#2)
	;WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#0)
	;WriteData(#' ')
	ret

Draw_big_8:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)
	WriteData(#6)
	WriteData(#2)
	;WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#7)
	WriteData(#5)
	;WriteData(#' ')
	ret

Draw_big_9:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)
	WriteData(#6)
	WriteData(#2)
	;WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#255)
	;WriteData(#' ')
	ret

; The number to display is passed in accumulator.  The column where to display the
; number is passed in R1. This works only for numbers 0 to 9.
Display_big_number:
	; We need to multiply the accumulator by 3 because the jump table below uses 3 bytes
	; for each 'ljmp' instruction.
	mov b, #3
	mul ab
	mov dptr, #Jump_table
	jmp @A+dptr
Jump_table:
	ljmp Draw_big_0 ; This instruction uses 3 bytes
	ljmp Draw_big_1
	ljmp Draw_big_2
	ljmp Draw_big_3
	ljmp Draw_big_4
	ljmp Draw_big_5
	ljmp Draw_big_6
	ljmp Draw_big_7
	ljmp Draw_big_8
	ljmp Draw_big_9
; No 'ret' needed because we are counting of on the 'ret' provided by the Draw_big_x functions above

; Takes a BCD 2-digit number passed in the accumulator and displays it at position passed in R0
Display_Big_BCD:
	push acc
	; Display the most significant decimal digit
	mov b, R0
	mov R1, b
	swap a
	anl a, #0x0f
	lcall Display_big_number
	
	; Display the least significant decimal digit, which starts 4 columns to the right of the most significant digit
	mov a, R0
	add a, #3
	mov R1, a
	pop acc
	anl a, #0x0f
	lcall Display_big_number
	
	ret

Draw_0:
	mov a,R1
	lcall ?WriteCommand
	mov a,#'0'
	lcall ?WriteData
	ret
	
Draw_1:
	mov a,R1
	lcall ?WriteCommand
	mov a,#'1'
	lcall ?WriteData
	ret
	
Draw_2:
	mov a,R1
	lcall ?WriteCommand
	mov a,#'2'
	lcall ?WriteData
	ret
	
Draw_3:
	mov a,R1
	lcall ?WriteCommand
	mov a,#'3'
	lcall ?WriteData
	ret
	
Draw_4:
	mov a,R1
	lcall ?WriteCommand
	mov a,#'4'
	lcall ?WriteData
	ret
	
Draw_5:
	mov a,R1
	lcall ?WriteCommand
	mov a,#'5'
	lcall ?WriteData
	ret
	
Draw_6:
	mov a,R1
	lcall ?WriteCommand
	mov a,#'6'
	lcall ?WriteData
	ret
	
Draw_7:
	mov a,R1
	lcall ?WriteCommand
	mov a,#'7'
	lcall ?WriteData
	ret
	
Draw_8:
	mov a,R1
	lcall ?WriteCommand
	mov a,#'8'
	lcall ?WriteData
	ret
	
Draw_9:
	mov a,R1
	lcall ?WriteCommand
	mov a,#'9'
	lcall ?WriteData
	ret

; The number to display is passed in accumulator.  The column where to display the
; number is passed in R1. This works only for numbers 0 to 9.
Display_number:
	; We need to multiply the accumulator by 3 because the jump table below uses 3 bytes
	; for each 'ljmp' instruction.
	mov b, #3
	mul ab
	mov dptr, #Jump_number
	jmp @A+dptr
Jump_number:
	ljmp Draw_0 ; This instruction uses 3 bytes
	ljmp Draw_1
	ljmp Draw_2
	ljmp Draw_3
	ljmp Draw_4
	ljmp Draw_5
	ljmp Draw_6
	ljmp Draw_7
	ljmp Draw_8
	ljmp Draw_9
	
; Takes a BCD 2-digit number passed in the accumulator and displays it at position passed in R0
Display_mini_BCD:
	push acc
	; Display the most significant decimal digit
	mov b, R0
	mov R1, b
	swap a
	anl a, #0x0f
	lcall Display_number
	
	; Display the least significant decimal digit, which starts 4 columns to the right of the most significant digit
	mov a, R0
	add a, #1
	mov R1, a
	pop acc
	anl a, #0x0f
	lcall Display_number
	
	ret

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0 configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    lcall Custom_Characters ; Custom characters are needed to display big numbers.  This call generates them.
    setb seconds_flag
    clr daytime
	mov BCD_counter, #0x00
	mov min_counter, #0x57
	mov hour_counter,#0x11
	
	; After initialization the program stays in this 'forever' loop
loop:
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Now clear the BCD counter
	mov BCD_counter, a
	setb TR2                ; Start timer 2
	sjmp loop_b             ; Display the new value	

loop_a:
	jnb seconds_flag,loop
loop_b:
	clr seconds_flag
	
	mov a,#0x86
	lcall ?WriteCommand
	mov a,#'.'
	lcall ?WriteData
	mov a,#0xc6
	lcall ?WriteCommand
	mov a,#'.'
	lcall ?WriteData
	
seconds:
	mov R0,#0xce
	mov a,BCD_counter
	lcall Display_mini_BCD
	lcall minutes
	lcall hours
	lcall daytime_set
	ljmp loop

minutes:
	mov R0,#7
	mov a,min_counter
	lcall Display_Big_BCD
	ret
	;;;;;;
hours:
	mov R0,#0
	mov a,hour_counter
	lcall Display_Big_BCD
	ret

daytime_set:
	
	jnb daytime,display_pm
	;; display am
	Writecommand(#0x8e)
	WriteData(#'A')
	Writecommand(#0x8f)
	WriteData(#'M')
	ret
	
display_pm:
	Writecommand(#0x8e)
	WriteData(#'P')
	Writecommand(#0x8f)
	WriteData(#'M')
	ret


END