; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 440 Hz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P3.7 is pressed.
$NOLIST
$MODEFM8LB1
$LIST

CLK           EQU 24000000 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 2000*2    ; The tone we want out is A mayor.  Interrupt rate must be twice as fast.
TIMER0_RELOAD EQU ((65536-(CLK/(TIMER0_RATE))))
TIMER2_RATE   EQU 100    ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/(TIMER2_RATE))))

BOOT_BUTTON   equ P3.7
SOUND_OUT     equ P2.1
;clearalarm    equ P3.3
UPDOWN        equ p3.1  ; minute dec for clock
downhour	equ p3.3  ;hour dec for clock
downalarm     equ p2.4
upalarm       equ p2.2
clearalarm  equ p0.0
;ALARMBUTTON   equ p3.3

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
;BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
hours_count: ds 1
minutes_count: ds 1
seconds_count: ds 1

ahours_count: ds 1
aminutes_count: ds 1
aseconds_count: ds 1

time20sec: ds 1


; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed

AM_PM_flag: dbit 1
alarm_flag: dbit 1
AM_PM_flagalarm: dbit 1

buttonpress: dbit 1
time20secf: dbit 1

cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P2.0
LCD_RW equ P1.7
LCD_E  equ P1.6
LCD_D4 equ P1.1
LCD_D5 equ P1.0
LCD_D6 equ P0.7
LCD_D7 equ P0.6
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                   1234567890123456    <- This helps determine the location of the counter
Clock_message:  db '--:--:-- -M     ', 0
Alarm_meesage: db  'ALARM --:-- -M', 0
date_message: db   '01 february 2020 ', 0
;-----------------------------------;
; Routine to initialize the timer 0 ;
;-----------------------------------;
Timer0_Init:
	orl CKCON0, #00000100B ; Timer 0 uses the system clock
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.                ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	; Timer 0 can not autoreload so we need to reload it in the ISR:
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	
	jnb alarm_flag, noalarm2
	setb TR0
	cpl SOUND_OUT ; Toggle the pin connected to the speaker
	noalarm2:
	reti

;---------------------------------;
; Routine to initialize timer 2   ;
;---------------------------------;
Timer2_Init:
	orl CKCON0, #0b00010000 ; Timer 2 uses the system clock
	mov TMR2CN0, #0 ; Stop timer/counter.  Autoreload mode.
	mov TMR2H, #high(TIMER2_RELOAD)
	mov TMR2L, #low(TIMER2_RELOAD)
	; Set the reload value
	mov TMR2RLH, #high(TIMER2_RELOAD)
	mov TMR2RLL, #low(TIMER2_RELOAD)
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
	clr TF2H  ; Timer 2 doesn't clear TF2H automatically. Do it in ISR
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(500), Timer2_ISR_doneinterm2 ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(500), Timer2_ISR_doneinterm2
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	
	setb half_seconds_flag ; Let the main program know half second had passed
	clr SOUND_OUT
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	setb SOUND_OUT
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	
	
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	;;;;;;ALARM START
	jnb upalarm, alarm_inc2
	jnb downalarm, alarm_dec2
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ALARM DONE

	seconds:
	mov 	a, 	seconds_count
	jnb UPDOWN, Timer2_ISR_decrement
	jnb downhour, dechour
	add     a, #0x01;
             ; reset second, increment minute
    da 		a
    mov 	seconds_count,    a
	; Increment the BCD counter
	
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov seconds_count, a
	cjne 	a, 	#0x60,     Timer2_ISR_doneinterm
	
	minutes:
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	mov seconds_count, a
	setb TR2
	mov 	a, 	minutes_count
	jnb UPDOWN, Timer2_ISR_decrement
	add     a, #0x01;
             ; reset second, increment minute
    da 		a
    mov 	minutes_count,    a
	mov minutes_count, a
	cjne 	a, 	#0x60,     Timer2_ISR_doneinterm
	
	hours:
	mov a,hours_count
	cjne a,#0x12,AM_PM
	clr a
	mov minutes_count,a
	mov a,#0x1
	mov hours_count,a
	sjmp Timer2_ISR_doneinterm
	
	AM_PM:
	mov a,hours_count
	cjne a,#0x11, clear
	cpl AM_PM_flag
	clear:
	clr a
	mov minutes_count,a					
	mov 	a, 	hours_count
	jnb UPDOWN, Timer2_ISR_decrement
	add     a, #0x01;
             ; reset second, increment minute
    da 		a
    mov 	hours_count,    a
    
	sjmp  Timer2_ISR_doneinterm
Timer2_ISR_doneinterm2: ljmp Timer2_ISR_doneinterm
alarm_inc2: ljmp alarm_inc
alarm_dec2: ljmp alarm_dec

Timer2_ISR_decrement:
minutedec:
	mov a, minutes_count
	cjne a,#0x00, minjmp
	mov a, #0x59
	da a
	mov minutes_count, a
	sjmp dechour
	
minjmp:	mov a, minutes_count
	add a, #0x99
	da a
	mov minutes_count, a
	sjmp Timer2_ISR_doneinterm

;	mov a, seconds_count
;	cjne a, #0x00, secjmp
;	mov a, #0x59
;	da a ; Decimal adjust instruction.  Check datasheet for more details!
;	mov seconds_count, a
;	sjmp minutedec
;	secjmp:mov a, seconds_count
;	add a, #0x99
;	da a
;	mov seconds_count, a
	

 Timer2_ISR_doneinterm: ljmp Timer2_ISR_done
 seconds2: ljmp seconds
 
 ;minutedec:
	;mov a, minutes_count
	;cjne a,#0x00, minjmp
	;mov a, #0x59
	;da a
	;mov minutes_count, a
	;sjmp Timer2_ISR_doneinterm
	
;minjmp:	mov a, minutes_count
	;add a, #0x99
	;da a
	;mov minutes_count, a
	;sjmp Timer2_ISR_doneinterm
	
dechour:
	mov a, hours_count
	cjne a, #0x01, hourjmp
	mov a,#0x12
	da a
	mov hours_count, a
	mov a, hours_count
	
	sjmp Timer2_ISR_doneinterm
	
	
hourjmp:	
	mov a, hours_count

	add a,#0x99
	da a
	cjne a, #0x11, skipflag
	cpl AM_PM_flag
	skipflag:
	mov hours_count, a
	sjmp Timer2_ISR_doneinterm
	
alarm_inc:

	mov a, aminutes_count
	cjne a,#0x59, minjmp3
	mov a, #0x00
	da a
	mov aminutes_count, a
	sjmp dechour3
	
minjmp3:	
	mov a, aminutes_count
	add a, #0x01
	da a
	mov aminutes_count, a
	sjmp seconds2
	
dechour3:
	mov a, ahours_count
	cjne a, #0x12, hourjmp3
	mov a,#0x01
	da a
	mov ahours_count, a
	mov a, ahours_count
	
	sjmp Timer2_ISR_doneinterm
	
	seconds3: ljmp seconds2
hourjmp3:	
	mov a, ahours_count

	add a,#0x01
	da a
	cjne a, #0x12, skipflag3
	cpl AM_PM_flagalarm
	skipflag3:
	mov ahours_count, a
	sjmp seconds2



alarm_dec:
	dechour5:
	mov a, ahours_count
	cjne a, #0x12, hourjmp5
	mov a,#0x01
	da a
	mov ahours_count, a
	mov a, ahours_count
	
	sjmp Timer2_ISR_doneinterm
	
hourjmp5:	
	mov a, ahours_count

	add a,#0x01
	da a
	cjne a, #0x12, skipflag5
	cpl AM_PM_flagalarm
	skipflag5:
	mov ahours_count, a
	sjmp seconds2

	mov a, seconds_count
	cjne a, #0x59, secjmp2
	clr a
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov seconds_count, a
	sjmp seconds3
	secjmp2:mov a, seconds_count
	add a, #0x01
	da a
	mov seconds_count, a

 
Timer2_ISR_done:
	pop psw
	pop acc
	reti

;---------------------------------;
; Hardware initialization         ;
;---------------------------------;
Initialize_All:
    ; DISABLE WDT: provide Watchdog disable keys
	mov	WDTCN,#0xDE ; First key
	mov	WDTCN,#0xAD ; Second key

    ; Enable crossbar and weak pull-ups
	mov	XBR0,#0x00
	mov	XBR1,#0x00
	mov	XBR2,#0x40

	mov	P2MDOUT,#0x02 ; make sound output pin (P2.1) push-pull
	
	; Switch clock to 24 MHz
	mov	CLKSEL, #0x00 ; 
	mov	CLKSEL, #0x00 ; Second write to CLKSEL is required according to the user manual (page 77)
	
	; Wait for 24 MHz clock to stabilze by checking bit DIVRDY in CLKSEL
waitclockstable:
	mov a, CLKSEL
	jnb acc.7, waitclockstable 

	; Initialize the two timers used in this program
    lcall Timer0_Init
    lcall Timer2_Init

    lcall LCD_4BIT ; Initialize LCD
    
    setb EA   ; Enable Global interrupts

	ret

;---------------------------------;
; Main program.                   ;
;---------------------------------;
main:
	; Setup the stack start to the begining of memory only accesible with pointers
    mov SP, #7FH
    
	lcall Initialize_All
	
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Clock_message)
	Set_Cursor(2, 1)
    Send_Constant_String(#Alarm_meesage)
    setb half_seconds_flag
	clr AM_PM_flag
	mov seconds_count, #0x53
	mov minutes_count, #0x58
	mov hours_count, #0x11
	
	clr alarm_flag
	
	mov aminutes_count, #0x59
	mov ahours_count, #0x11
	clr AM_PM_flagalarm
	
	
	
	
	
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
	mov seconds_count, a
	setb TR2                ; Start timer 2
	
	
	sjmp loop_b             ; Display the new value
	
	
	clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
    
    
loop_a:
	
	jnb half_seconds_flag, loop
	
loop_b:
	
	
	
	jb clearalarm, loop_c  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb clearalarm, loop_c  ; if the 'BOOT' button is not pressed skip
	jnb clearalarm, $		; Wait for button release.  The '$' means: jump to same instruction.

	clr alarm_flag

loop_c:
	mov a, minutes_count
	mov b, aminutes_count
	cjne a, b, clearflag
	mov a,hours_count
	mov b,ahours_count
	cjne a, b, clearflag
	
	jb AM_PM_flag,test1
	jb AM_PM_flagalarm,not_equal
	sjmp equal
	

	test1: jnb AM_PM_flagalarm, not_equal
	sjmp equal
	
	equal: 
	;jb alarm_flag, clearflag
	
	setb alarm_flag
	mov a, seconds_count
	cjne a, #0x30, alarmdonedone
	sjmp clearflag
	
	sjmp alarmdonedone
	not_equal:
	sjmp clearflag 
	

	clearflag: 
	clr alarm_flag
	
 
	alarmdonedone:
	Set_Cursor(1, 7)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(seconds_count) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 4)
	Display_BCD(minutes_count)
	Set_Cursor(1, 1)
	Display_BCD(hours_count)
	jnb AM_PM_flag, display_AM
		Set_Cursor(1, 10)
		Display_char(#'A')
		sjmp ampmdone
	display_AM: 
	Set_Cursor(1, 10)
	Display_char(#'P')
	 ampmdone:
	 
	 
	Set_Cursor(2, 10)
	Display_BCD(aminutes_count)
	Set_Cursor(2, 7)
	Display_BCD(ahours_count)
	jnb AM_PM_flagalarm, display_AM2
		Set_Cursor(2, 13)
		Display_char(#'A')
		sjmp ampmdone2
	display_AM2: 
	Set_Cursor(2, 13)
	Display_char(#'P')
	 ampmdone2:
    ljmp loop
END
