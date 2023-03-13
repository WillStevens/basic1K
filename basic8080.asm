; Will Stevens
; 25th Feb 2023
; 1K 8080 BASIC
;
; 2023-03-03 About 450 bytes long
; 2023-03-08 About 750 bytes long
; 2023-03-11 About 840 bytes long
; 2023-03-12 About 930 bytes long.
;   String tokens added.
;   * and / yet to be added.
;   Some scope for size optimization.
; 2023-03-12 About 940 bytes long
		* added
		some reduction in code size done
		
; Memory map:
; system vars
; var space : 52 bytes
; input buffer : 64 bytes
; (also used as expression stack)
; program area
; stack area - top of RAM

; For development purposes assume we have
; 1K ROM from 0000h-03FFh containing BASIC
; 1K RAM from 0400h-07FFh

RAM_TOP equ 0800h ; 1 more than top byte of RAM

; Token values
; 193-218 are variables
IntegerToken equ 254 ; followed by 16-bit integer
LinenumToken equ 255 ; followed by 16-bit integer
StringToken equ 253; followed by 1 byte length, followed by string characters
CommaToken equ 252
NotFoundToken equ 251

; Callable tokens are low byte of subroutine to call

; Errors are display as Ex where x is a hex character
; E0 - unrecognised token during parsing
; E1 - end of program when looking for line
; EA - newline encountered in string
; EF - input buffer overflow

ORG 0400h

; For efficient access, this must be on a 256 byte boundary
VAR_SPACE:
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0
	
PROG_PTR:
	DW PROG_BASE
PROG_PARSE_PTR:
	DW PROG_BASE

; Input buffer and operator stack can share the
; same memory - not used at same time
; Input buffer must not be over a 256 byte 
; boundary
; Stack is just below input buffer to save code
; when initialising

org RAM_TOP-64
STACK_INIT:
INPUT_BUFFER:
OPERATOR_STACK_PTR:
	DW 0
OPERATOR_STACK_BASE:
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
INPUT_BUFFER_END:

PROG_BASE:

ORG 10h

GetLine:

	; Set stack pointer to just below input buffer
	; Do this every time to guard against
	; GOSUB with no RETURN errors
	
	LXI H,INPUT_BUFFER-1
	SPHL
	
	; H is also the initial pointer to input buffer

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
	
	LHLD PROG_PTR
	MOV A,M
	CPI IntegerToken
	JNZ ExecuteDirect
	INR A
	MOV M,A
	LHLD PROG_PARSE_PTR
	SHLD PROG_PTR
	
	JMP GetLine
	
ExecuteDirect: ; Depth = 0
	
	LHLD PROG_PTR
	SHLD PROG_PARSE_PTR
	
	; Otherwise execute the statement
	; Assume its RUN
	JMP ExecuteProgram
	
	JMP GetLine
	
NextToken: ; Depth = 1
	; Get class of first char
	MOV A,M
	CPI 10
	RZ
	CALL CharClass
	MOV B,A
	
	; Copy HL to DE so that DE points to start
	PUSH H
	POP D
	
	; If first char is double quote, then we're not yet inside the string
	CPI '"'
	JZ NotInString
	
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
	CPI '"'
	JZ String
	CPI 'A'
	JNZ NotVar
	MOV A,L
	DCR A
	CMP E
	JZ Var
	
NotVar:
	
	CALL LookupToken
	CPI NotFoundToken
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
	XRA A
	JMP PopError

Var:
	LDAX D
	; Token value is var letter+128
	
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
	ANI 07f
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
	MVI M,IntegerToken
	INX H
	MOV M,E
	INX H
	MOV M,D
	INX H
	SHLD PROG_PARSE_PTR
	
	POP H
	
	JMP NextToken

; DE points to first double quote
; HL-1 is last double quote
String:
	INR E
	MOV A,L
	SUB E
	DCR A
	;A contains the length
	
	PUSH H
	
	;Store token ID, length and string
	LHLD PROG_PARSE_PTR
	MVI M,StringToken
	INX H
	MOV M,A
	INX H
	CALL StrCpy
	SHLD PROG_PARSE_PTR
	
	POP H
	
	JMP NextToken
	

; DE points to start of token
; HL points 1 char after
LookupToken: ; Depth = 2
	PUSH H
	
	LXI H,TokenList-1
LookupTokenNext:
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
; Advance past this then get the token value
	INX H
	MOV A,M
	
	POP H
	RET

; DE points to hi-bit terminated string
; HL points to hi-bit terminated string
; Returns 0 in A on match
Strcmp: ; Depth = 3
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

; DE points to start
; HL points to dest
; A is length

; Leaves HL pointing to char after string
StrCpy:
	ORA A
	RZ
	
	PUSH PSW
	LDAX D
	MOV M,A
	POP PSW

	INX D
	INX H
	
	DCR A
	JMP Strcpy
	


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

PrintStringToken:
	INX B
	LDAX B
	INX B
	PUSH B
	POP H
	CALL OutputString
	MOV B,H
	MOV C,L
	RET
	
GetLineNum:
	; Line number is in DE, look it up in the program and set BC to the position after it
	LXI B,PROG_BASE

GetLineNumLoop:
	INX B
	LDAX B
	INX B
	CMP E
	JNZ GetLineNumNext
	LDAX B
	CMP D
	JNZ GetLineNumNext
	
	INX B
	RET
	
GetLineNumNext:
	INX B
	CALL AdvanceToNextLineNum
	JMP GetLineNumLoop
	
AdvanceToNextLineNum:
; BC is a pointer to somewhere in the program
; move onto the next line number
; or error if we fall out of program
	LDAX B
	CPI LinenumToken
	RZ
	CPI StringToken
	JZ ATNLN_String
	CPI IntegerToken
	JNZ ATNLN_NotInt
	INX B
	INX B
ATNLN_NotInt:
	INX B
	
	LDA PROG_PTR
	CMP C
	JNZ AdvanceToNextLineNum
	LDA PROG_PTR+1
	CMP B
	JNZ AdvanceToNextLineNum
	
	; Error, fell off end of program
	MVI A,1
	
	JMP PopError

ATNLN_String:
	INX B
	LDAX B
	INX B
	; Add A onto BC
	ADD C
	MOV C,A
	; TODO think this can be
	; ADC B, SBB C, MOV B,A
	MVI A,0
	ADC B
	MOV B,A
	
	JMP ATNLN_NotInt
	


ExecuteProgram: ; Depth = 0
	; Point BC to first line
	; Skip over the line number
	LXI B,PROG_BASE+3

ExecuteProgramLoop:
	; Check that we haven't reached end of program
	LHLD PROG_PTR
	MOV A,L
	CMP C
	JNZ ExecuteProgramNotEnd
	MOV A,H
	CMP B
	JZ GetLine
	
	; TODO need to check that we haven't ended with a GOSUB without return condition
	; stack should be empty
	; if not then correct it
	
ExecuteProgramNotEnd:
	LDAX B
	
	; Is it a line number?
	CPI LinenumToken
	JNZ ExecuteProgramNotLineNum
	
	INX B
	INX B
	INX B
	LDAX B
	
ExecuteProgramNotLineNum:
	INX B

	; TODO Check that it is a keyword allowed in a program
	
	; Put return address onto stack
	LXI H,ExecuteProgramLoop
	PUSH H
	
	; Put pointer to call address into HL
	MOV L,A
	MVI H,(TokenList&0ff00h)/256
	
	; Jump to it
	PCHL

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

OutputString:
;Pointer in H
;Length in A

	PUSH B
	
OutputStringLoop:
	ORA A
	JNZ OutputStringContinue
	POP B
	RET
	
OutputStringContinue:
	
	MOV E,M
	MVI C,02
	CALL 5
	
	INX H
	
	DCR A
	JMP OutputStringLoop

;Output the value in DE
PrintHex4:
	call PrintHex2
	mov d,e

;Output the value in D
PrintHex2:
	mov a,d
	rrc a
	rrc a
	rrc a
	rrc a
	ani 0fh
	call PrintHex
	mov a,d
	ani 0fh

;Output single hex value
PrintHex:
	adi 48
	cpi 58
	jc PrintHexSkip
	adi 7
	PrintHexSkip:
	PUSH PSW
	PUSH D
	CALL PutChar
	POP D
	POP PSW
	RET

; BC points to program
; DE contains value
; operator stack pointer stored in mem
; in OPERATOR_STACK_PTR
; Operand stack - SP

ExpError:
	; TODO implement this

ExpEvaluate:
	LXI H,OPERATOR_STACK_BASE
	SHLD OPERATOR_STACK_PTR

ExpEvaluateNum:
	; Expecting ( var integer
	LDAX B
	CPI LeftBraceSub&0ffh
	JZ ExpLeftBrace
	CPI IntegerToken
	JZ ExpInteger
	CPI 128+'Z'+1
	JNC ExpError
	CPI 128+'A'
	JC ExpError
	
	; Fall through to ExpVar
ExpVar:
	; Get var value into DE
	
	MVI H,VAR_SPACE/256
	SUI 128+'A'
	ADD A
	MOV L,A
	
	MOV E,M
	INX H
	MOV D,M
	
	INX B
	JMP ExpEvaluateOp
	
ExpLeftBrace:
	; push it onto operator stack
	LHLD OPERATOR_STACK_PTR
	MOV M,A
	INX H
	SHLD OPERATOR_STACK_PTR
	
	INX B
	JMP ExpEvaluateNum

ExpInteger:
	INX B
	LDAX B
	MOV E,A
	INX B
	LDAX B
	MOV D,A
	INX B
	JMP ExpEvaluateOp
	
ExpEvaluateOp:
	;Expecting operator or right bracket or
	;end of expression
	
	;Are there operators on the stack?
	LDA OPERATOR_STACK_PTR
	CPI OPERATOR_STACK_BASE&0ffh
	JZ SkipExpApplyOp
	
	LDAX B
	
	CPI RightBraceToken&0ffh ; operators or right bracket
	; Is it the end of the expression?
	JC ExpApplyOp
	
	; or does operator on stack have GTE precedence?
	DCR A
	LHLD OPERATOR_STACK_PTR
	CMP M
	
	JNC SkipExpApplyOp ; no, dont apply op

ExpApplyOp:
	; POP operator from stack
	; this also handles the case when a
	; left brace is encountered
	
	LHLD OPERATOR_STACK_PTR
	DCX H
	MOV A,M
	SHLD OPERATOR_STACK_PTR
	
	MVI H,TokenList/256
	MOV L,A
	
	; Push the operator function address
	; onto the stack, and use RET to branch to
	; it in a moment
	PUSH H
	; Put the address that we want to return to
	; into HL. 
	LXI H,ExpEvaluateOp
	
	RET	; Jump to operator function
	
SkipExpApplyOp:
	LDAX B
	
	CPI RightBraceToken&0ffh ; operators or right bracket
	; Is it the end of the expression?
	RC
	
	; Push onto the operator stack
	LHLD OPERATOR_STACK_PTR
	MOV M,A
	INX H
	SHLD OPERATOR_STACK_PTR

	; move onto next token
	INX B
	
	;because we are
	;expecting another value, push DE onto stack
	PUSH D
	
	JMP ExpEvaluateNum

NotEqualSub_1:
	; SubSub will have been executed prior to this
	; so test for zero
	LXI H,0ffffh
	DAD D
	JC NotEqualSub_2
	JMP ExpEvaluateOp

LTESub_1:
	; SubSub will have been executed prior to this
	JC EqualSub_2
	
NotEqualSub_2:
	LXI D,1
	JMP ExpEvaluateOp
	
EqualSub_1:
	; SubSub will have been executed prior to this
	; so test for zero
	LXI H,0ffffh
	DAD D
	JC EqualSub_2
	INX D
	JMP ExpEvaluateOp

GTSub_1:
	; SubSub will have been executed prior to this
	JC NotEqualSub_2
	
EqualSub_2:
	LXI D,0
	JMP ExpEvaluateOp

; TokenList must be on same page and index to subroutine address must not overlap with other token values
ORG 0300h

TokenList:
	DB "PRIN",'T'+128
	DB PrintSub&0ffh
	DB "LE",'T'+128
	DB LetSub&0ffh
	DB "GOT",'O'+128
	DB GotoSub&0ffh
	DB "GOSU",'B'+128
	DB GosubSub&0ffh
	DB "RETUR",'N'+128
	DB ReturnSub&0ffh
	DB "I",'F'+128
	DB IfSub&0ffh
	DB "EN",'D'+128
	DB 0
; Before this are keywords allowed at run-time
	DB "THE",'N'+128
	DB 0
	DB "RU",'N'+128
	DB 0
	DB "LIS",'T'+128
	DB 0
	DB "CLEA",'R'+128
	DB  0
	DB ','+128
	DB CommaToken
	DB '('+128
	DB LeftBraceSub&0ffh
;After this label, only expect things which count as operators
	DB ')'+128
	DB RightBraceToken&0ffh
	DB '<'+128
	DB LTSub&0ffh
	DB '>'+128
	DB GTSub&0ffh
	DB ">",'='+128
	DB GTESub&0ffh
	DB "<",'='+128
	DB LTESub&0ffh
	DB '='+128
	DB EqualSub&0ffh
	DB "<>"+128
	DB NotEqualSub&0ffh
	DB '-'+128
	DB SubSub&0ffh
	DB '+'+128
	DB AddSub&0ffh
	DB '*'+128
	DB MulSub&0ffh
	DB '/'+128
	DB DivSub&0ffh
NotFoundAddr:
	DB NotFoundToken
	
PrintSub:
	LDAX B
	CPI StringToken
	JZ PrintStringToken
	CALL ExpEvaluate
	PUSH B
	CALL PrintHex4
	POP B

PrintSubEndTest:
	LDAX B
	INX B
	CPI ','
	JZ PrintSub
	DCX B
	; TODO print new line
	RET

LetSub:
	; TODO test that we have var and equal sign
	LDAX B
	PUSH PSW
	INX B
	INX B
	
	CALL ExpEvaluate
	
	POP PSW
	
	; Put DE into var
	
	MVI H,VAR_SPACE/256
	SUI 128+'A'
	ADD A
	MOV L,A
	
	MOV M,E
	INX H
	MOV M,D
	
	RET
	
GotoSub:
	CALL ExpEvaluate
	JMP GetLineNum
	
GosubSub: ; Depth = 1
	CALL ExpEvaluate
	POP H	; Preserve return address
	PUSH B
	PUSH H
	JMP GetLineNum
	
ReturnSub:
	;TODO - how to always detect
	; return without GOSUB
	; could put marker word onto stack?
	; and inc/dec every call/return
	; and check for marker march
	
	POP H	; Get return address first
	POP B ; Get pointer to program loc to return to
	PCHL

IfSub:
	CALL ExpEvaluate
	MOV A,E
	ORA D
	RNZ
	
	; If DE zero then fall through to next line
	JMP AdvanceToNextLineNum
	
LeftBraceSub:
	LDAX B
	; Is current operator a right brace?
	CPI RightBraceToken&0ffh
	JNZ ExpError ; expecting right brace
	
	INX B
; This is a dummy label
; to make sure that right brace token value is between LeftBraceSub and LTSub
RightBraceToken:
	JMP ExpEvaluateOp

LTSub:
	; Swap operands and fall through
	XCHG
	XTHL
	XCHG
GTSub:
	LXI H,GTSub_1
	JMP SubSub

GTESub:
	; Swap operands and fall through
	XCHG
	XTHL
	XCHG
LTESub:
	LXI H,LTESub_1
	JMP SubSub

EqualSub:
	LXI H,EqualSub_1
	JMP SubSub
	
NotEqualSub:
	LXI H,NotEqualSub_1

SubSub:
	MOV A,E
	CMA
	MOV E,A
	MOV A,D
	CMA
	MOV D,A
	INX D
	
AddSub:
	; exchange H with stack top to get operand into H and return address on stack
	XTHL

	;Add DE to top of stack and keep in DE
	DAD D
	XCHG
	
	RET

MulSub:
	POP H
	PUSH B
	MOV B,H
	MOV C,L

Multiply:
;multiply BC and DE into HL

	MVI A,16
	LXI H,0
MulLoop:
	DAD H
	XCHG
	DAD H
	XCHG
	JNC DontAdd
	DAD B
DontAdd:

	DCR A
	JNZ MulLoop
	
	XCHG
	POP B
	RET

DivSub:
;Divide (SP) by DE
	POP H
	PUSH B
	
	LXI B,0 	; Accumulator starts at zero
	; Complement DE
	
	DAD D
	INX B
	

	POP B
	RET
