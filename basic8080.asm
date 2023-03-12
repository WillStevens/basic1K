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
;
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

RAM_TOP equ 07ffh ; highest byte of RAM

; Token values
; 0-25 are variables
IntegerToken equ 26 ; followed by 16-bit integer
LinenumToken equ 27 ; followed by 16-bit integer
StringToken equ 28 ; followed by 1 byte length, followed by string characters

; Other tokens are the low byte of a pointer into TokenList which stores an address to call corresponding to the token

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
; Input buffer must not be over a 256 byte boundary
INPUT_BUFFER:
;	DB "10 LET B=123",10
OPERATOR_STACK_PTR:
	DW 0
OPERATOR_STACK_BASE:
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
INPUT_BUFFER_END:

PROG_BASE:

ORG 10h

GetLine:

	; Set stack pointer to top of RAM
	; Do this every time to guard against
	; GOSUB with no RETURN errors
	
	LXI H,RAM_TOP+1
	SPHL
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
	
	LHLD PROG_PTR
	MOV A,M
	CPI 26
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
	ANI 07fh
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
	ADD A
	MOV L,A
	
	MOV M,E
	INX H
	MOV M,D
	
	RET
	
GosubSub: ; Depth = 1
	CALL ExpEvaluate
	POP H	; Preserve return address
	PUSH B
	PUSH H
	JMP GetLineNum
	
GotoSub:
	CALL ExpEvaluate

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

IfSub:
	CALL ExpEvaluate
	MOV A,E
	ORA D
	RNZ
	
	; If DE zero then fall through to next line
	
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
	
ReturnSub:
	;TODO - how to always detect
	; return without GOSUB
	; could put marker word onto stack?
	; and inc/dec every call/return
	; and check for marker march
	
	POP H	; Get return address first
	POP B ; Get pointer to program loc to return to
	PCHL

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
	; Get call address into DE
	MOV E,M
	INX H
	MOV D,M
	; Move it to HL
	XCHG
	
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
	CPI LeftBraceToken&0ffh
	JZ ExpLeftBrace
	CPI IntegerToken&0ffh
	JZ ExpInteger
	CPI 27
	JC ExpVar
	
	; TODO Error - expecting ( var integer
	JMP ExpError
	
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
	
ExpVar:
	; Get var value into DE
	
	MVI H,VAR_SPACE/256
	ADD A
	MOV L,A
	
	MOV E,M
	INX H
	MOV D,M
	
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
	
	CPI TokenListExpr&0ffh ; operators or right bracket
	; Is it the end of the expression?
	JC ExpApplyOp
	
	; or does operator on stack have GTE precedence?
	DCR A
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
	
	MOV A,M
	INX H
	MOV H,M
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
	
	CPI TokenListExpr&0ffh ; operators or right bracket
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
	
; If start address of all of the operator subroutines fit in 256 bytes then there is a potential saving by using only single byte in lookup table, and maybe also using this byte as the token 

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

LeftBraceSub:
	LDAX B
	; Is current operator a right brace?
	CPI RightBraceToken&0ffh
	JNZ ExpError ; expecting right brace
	
	INX B
	JMP ExpEvaluateOp

NotEqualSub:
	LXI H,NotEqualSub_1
EqualSub:
	LXI H,EqualSub_1
	
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

; TokenList must be on same page and index to subroutine address must not overlap with other token values
ORG 0380h

TokenList:
	DB "PRIN",'T'+128
	DW PrintSub
	DB "LE",'T'+128
	DW LetSub
	DB "GOT",'O'+128
	DW GotoSub
	DB "GOSU",'B'+128
	DW GosubSub
	DB "RETUR",'N'+128
	DW ReturnSub
	DB "I",'F'+128
	DW IfSub
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
	DB '('+128
LeftBraceToken:
	DW LeftBraceSub
;After this label, only expect things which count as operators
TokenListExpr:
	DB ')'+128
RightBraceToken:
	DW 0
	DB '='+128
	DW EqualSub
	DB "<>"+128
	DW NotEqualSub
	DB '<'+128
	DW LTSub
	DB '>'+128
	DW GTSub
	DB "<",'='+128
	DW LTESub
	DB ">",'='+128
	DW GTESub
	DB '+'+128
	DW AddSub
	DB '-'+128
	DW SubSub
	DB '*'+128
	DW 0
	DB '/'+128
	DW 0
NotFoundAddr:
	DB 0
