; Will Stevens
; 25th Feb 2022
; 1K 8080 BASIC
;
; Memory map:
; system vars
; var space : 52 bytes
; input buffer : 64 bytes
; (also used as expression stack)
; program area
; stack area - top of RAM

; 0-25 are variables
IntegerToken equ 26 ; followed by 16-bit integer
LinenumToken equ 27 ; followed by 16-bit integer
StringToken equ 28 ; followed by 1 byte length, followed by string characters

; Other tokens are the low byte of a pointer into TokenList which stores an address to call corresponding to the token

; Errors are display as Ex where x is a hex character
; E0 - unrecognised token during parsing
; EA - newline encountered in string
; EF - input buffer overflow

ORG 0400h

PROG_PTR:
	DW PROG_BASE
PROG_PARSE_PTR:
	DW PROG_BASE
	
VAR_SPACE:
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0
	
INPUT_BUFFER:
;	DB "10 LET B=123",10
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
INPUT_BUFFER_END:

PROG_BASE:

ORG 10h

GetLine:
	LXI H,INPUT_BUFFER-1

InputChar:
	INX H
	MOV A,L
	CPI INPUT_BUFFER_END&0ffh
	JZ InputBufferOverflow
	MVI C,01
	PUSH H
	CALL 5
	POP H
	
	MOV M,A
	
	CPI 10
	JNZ InputChar
	
	; Now we have a line terminated by chr(10)
	; Get location of input buffer into HL
	LXI H,INPUT_BUFFER

	CALL NextToken
	
	; If first token was an int, change it to a line no. marker and add the line to the program
	
	; Otherwise execute the statement
	
	JMP InputChar
	
NextToken:
	; Get class of first char
	MOV A,M
	CPI 10
	RZ
	CALL CharClass
	MOV B,A
	
	; Copy HL to DE so that DE points to start
	PUSH H
	POP D
	
NextChar:
	; Are we in a string
	MOV A,B
	CPI '"'
	JNZ NotInString
	
	; If yes then is M a chr(10) or quote
	MOV A,M
	INX H
	CPI 10
	JZ PopError
	CPI '"'
	JNZ NextChar
	
	
	; Otherwise fall through and whole string will be handled
	
NotInString:
	; Is M a different class from B
	MOV A,M
	CALL CharClass
	
	CMP B
	JNZ DiffClass
	
	INX H
	JMP NextChar

; If we reach this point then contents of buffer from DE to HL-1 are a token. C is the length
; If it is an integer then calculate value
; Otherwise lookup the token
DiffClass:
	; Set the hi bit of the last char in the token
	DCX H
	MOV A,M
	ORI 128
	MOV M,A
	INX H
	
	MOV A,B
	CPI ' '
	JZ NextToken
	CPI '0'
	JZ Integer
	CPI 'A'
	JNZ NotVar
	MOV A,L
	DCR A
	CMP E
	JZ Var
	
NotVar:
	
	CALL LookupToken
	CPI (NotFoundAddr+1)&0ffh
	JZ TokenNotFound

	PUSH H
	
; Store token in program
	LHLD PROG_PARSE_PTR
	MOV M,A
	INX H
	SHLD PROG_PARSE_PTR
	
	POP H
	
	JMP NextToken

TokenNotFound:
	; Unrecognised token
	MVI A,0
	JMP PopError

Var:
	LDAX D
	ANI 128
	SBI 'A'
	
	; Store var token in program
	LHLD PROG_PARSE_PTR
	MOV M,A
	INX H
	SHLD PROG_PARSE_PTR
	
	; DE points to the single-char varname, so swap it with HL and then inc to get a pointer to the next char to look at
	XCHG
	INX H
	
	JMP NextToken
	
Integer:
	; The integer will be constructed in HL
	LXI H,0
	
IntegerNext:
	; Muliply by 10
	PUSH H
	POP B
	
	DAD H
	DAD H
	DAD B
	DAD H

	LDAX D
	ANI 07fh
	SUI '0'
	MOV C,A
	MVI B,0
	DAD B
	
	LDAX D
	INX D
	ANI 128
	JZ IntegerNext
	
	; At this point HL contains the integer
	; DE points to tbe char after end of token
	
	PUSH D
	XCHG
	
	; Store integer in program
	LHLD PROG_PARSE_PTR
	MVI A,IntegerToken
	MOV M,A
	INX H
	MOV M,E
	INX H
	MOV M,D
	INX H
	SHLD PROG_PARSE_PTR
	
	POP H
	
	JMP NextToken

; DE points to start of token
; HL points 1 char after
LookupToken:
	PUSH H
	
	LXI H,TokenList-2
LookupTokenNext:
	INR L
	INR L
	MOV A,L
	CPI NotFoundAddr&0ffh
	JZ NotFoundToken
	PUSH D
	CALL Strcmp
	POP D
	CPI 0
	JZ FoundToken
	
LookupTokenFindNext:
	MOV A,M
	INR L
	ANI 128
	JNZ LookupTokenNext
	JMP LookupTokenFindNext

NotFoundToken:
FoundToken:
; HL points to the last char of the token we've found
; Advance past this so that it points to a subroutine address
	MOV A,L
	INR A
	
	POP H
	RET

; DE points to hi-bit terminated string
; HL points to hi-bit terminated string
; Returns 0 in A on match
Strcmp:
	LDAX D
	SUB M
	RNZ
	
	MOV A,M
	ANI 128
	XRI 128
	RZ
	
	INX D
	INX H
	JMP Strcmp
	
; Return the class of a character for tokenizing
; Digit
; Alphabetical
; LT, GT or EQ
; All others are distinct classes

CharClass:
	CPI '0'
	JC NotDigit
	CPI '9'+1
	JC Digit
NotDigit:
	CPI 'A'
	JC NotAlpha
	CPI 'Z'+1
	JC Alpha
NotAlpha:
	RET

Digit:
	MVI A,'0'
	RET
Alpha:
	MVI A,'A'
	RET

PrintSub:
	RET

LetSub:
	RET

InputBufferOverflow:
	MVI A,0fh
	JMP Error
;Display error code in A and go back to line entry
PopError:
	POP H
Error:
	CPI 10
	JNZ ErrorLt10
	ADI 'A'-'0'-10
ErrorLt10:
	ADI '0'
	PUSH PSW
	MVI A,10
	CALL PutChar
	MVI A,69
	CALL PutChar
	POP PSW
	CALL PutChar
	MVI A,10
	CALL PutChar
	JMP Getline

PutChar:
	MOV E,A
	MVI C,02
	JMP 5

; TokenList must be on same page and index to subroutine address must not overlap with other token values
ORG 0380h

TokenList:
	DB "PRIN",'T'+128
	DW PrintSub
	DB "LE",'T'+128
	DW LetSub
	DB "GOT",'O'+128
	DW 0
	DB "GOSU",'B'+128
	DW 0
	DB "RETUR",'N'+128
	DW 0
	DB "I",'F'+128
	DW 0
	DB "EN",'D'+128
	DW 0
; Before this are keywords allowed at run-time
	DB "THE",'N'+128
	DW 0
	DB "RU",'N'+128
	DW 0
	DB "LIS",'T'+128
	DW 0
	DB "CLEA",'R'+128
	DW 0
	DB ','+128
	DW 0
; After this are all things that can be found in expressions
	DB '+'+128
	DW 0
	DB '-'+128
	DW 0
	DB '*'+128
	DW 0
	DB '/'+128
	DW 0
	DB '='+128
	DW 0
	DB "<>"+128
	DW 0
	DB '<'+128
	DW 0
	DB '>'+128
	DW 0
	DB "<",'='+128
	DW 0
	DB ">",'='+128
	DW 0
	DB '('+128
	DW 0
	DB ')'+128
	DW 0
NotFoundAddr:
	DB 0
