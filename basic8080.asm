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
;		* added
;		some reduction in code size done
; 2023-03-16 About 970 bytes long
;   unsigned / and integer output added
;		about 30 bytes could be saved
;		by using RST in place of call
;   in some places
; 2023-03-17 About 940 bytes long
; 2023-03-17 About 970 bytes long
;		INPUT added 
; 2023-03-19 About 960 bytes long
;    some bug fixes
;		 capable of playing lunar lander
; 2023-03-22 About 970 bytes long
;		 signed / added
; 2023-03-24 About 950 bytes long
;		 more code size reductions
;		 signed integer parsing supported
; 2023-03-24 About 940 bytes long
;    more code size reductions
; 2023-03-27 About 950 bytes long
;		 simplified operator calling and
;		 simplified a few operators
;		 working on memory rotate function needed
;		 for line deletion and insertion

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
; 0-25 are variables
; StringToken ; 26
IntegerToken equ 28 ; followed by 16-bit integer
LinenumToken equ 27 ; followed by 16-bit integer
										; followed by 1 byte length
										; followed by 2 byte ptr
										; to next line (0 = end)
StringToken equ 26 ; followed by 1 byte length, followed by string characters
CommaToken equ 29

; Callable tokens are low byte of subroutine to call

; Errors are display as Ex where x is a letter
; Error code is the lowest 16 bits of the
; address that called RST 4. If there is a
; collision the shuffling subroutine might
; resolve it.

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

ORG 0400h

; this must be on a 256 byte boundary
VAR_SPACE:
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0
	
PROG_PTR:
	DW PROG_BASE
PROG_PARSE_PTR:
	DW PROG_BASE

PROG_BASE:

ORG 00h
	RST 4 ; call error routine
				; which will fall through to Ready
	
; Space for 7 byte subroutine


ORG 08h

; PutChar is called frequently
; it can be called using RST 1

PutChar:
	; Assume that port 0 is for char I/O
	; And port 1 is for char ready status
	
	OUT 0
	RET

; Space for 5 byte subroutine(s)


ORG 10h

; GetChar can be called with RST 2

GetChar:
	IN 1
	JZ GetChar
	IN 0
	RET
	
ORG 18h

CompareHLDE:
; compare HL and DE, return
; Z if equal, NZ if not equal
	MOV A,L
	SUB E
	RNZ
	MOV A,H
	SUB D
	RET
	
; 2 bytes free

ORG 20h

;Display error code and go back to line entry
;TODO waste to have this as RST because its
; usually called with condition test
Error:
	MVI A,10
	RST 1
	MVI A,'H'
	RST 1
	POP PSW		; discard return address and
						; get error code
	ANI 0fh
	ADI 'I'
	RST 1
	MVI A,10
	RST 1
	
Ready:
	; Set stack pointer to just below input buffer
	; Do this every time to guard against
	; GOSUB with no RETURN errors
	
	LXI H,INPUT_BUFFER-1
	SPHL
	
	; H is also the initial pointer to input buffer

	CALL Getline
	
	; Now we have a line terminated by chr(10)
	; Get location of input buffer into HL
	LXI H,INPUT_BUFFER

	CALL NextToken
	
	LHLD PROG_PTR
	MOV A,M
	CPI IntegerToken
	JNZ ExecuteDirect

	; Is it an integer all by itself? 
	; If so then delete the line
	LDA PROG_PARSE_PTR
	SBI 3
	CMP L
	JZ DeleteProgramLine

	; If first token was an int, change it to a
	; LinenimToken and add the line to the program
	
	DCR A
	MOV M,A
	LHLD PROG_PARSE_PTR
	SHLD PROG_PTR
	
	JMP Ready

DeleteProgramLine:
	; TODO still working on this
	; set PROG_PARSE_PTR back to PROG_PTR
	SHLD PROG_PARSE_PTR
	
	INX H
	MOV E,M
	INX H
	MOV D,M
	
	CALL GetLineNum
	JNZ Ready		; if line not found, do nothing
	
	PUSH B
	CALL AdvanceToNextLineNum
	
	; (SP) = first
	; PROG_PTR = last
	; BC = middle
	
	; next = middle
	MOV H,B
	MOV L,C
	
DPL_Loop:
	; while (first!=next)
	
	; swap (*first++,*next++);
	MOV D,M
	XTHL
	MOV A,M
	MOV M,D
	XTHL
	MOV M,A

	XCHG
	LHLD PROG_PTR
	MOV A,L
	SUB E
	
    
;  if (next==last) next=middle;
; else if (first==middle) middle=next;
; }
	
	JMP Ready

MemoryShift:

GetLine:
	INX H
	MOV A,L
	CPI INPUT_BUFFER_END&0ffh
	CZ Error	; input buffer overflow
	RST 2
	
	MOV M,A
	
	CPI 10
	JNZ GetLine
	RET

ExecuteDirect: ; Depth = 0
	
	LHLD PROG_PTR
	SHLD PROG_PARSE_PTR
	
	; Otherwise execute the statement
	; Assume its RUN
	JMP ExecuteProgram
	
	JMP Ready
	
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
	
	INX H
	
NextChar:
	; Are we in a string
	MOV A,B
	CPI '"'
	JNZ NotInString
	
	; If yes then is M a chr(10) or quote
	MOV A,M
	INX H
	CPI 10
	JZ Error
	CPI '"'
	JNZ NextChar
	
	; Otherwise fall through and whole string will be handled
	
NotInString:
	; Is M a different class from B, or
	; a char that can't belong to a sequence
	MOV A,M
	CALL CharClass
	
	CMP B
	JNZ DiffClass
	ANI 80h
	JZ DiffClass
	
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
	CPI '0'+128
	JZ Integer
	CPI '"'
	JZ String
	CPI 'P'+128
	JNZ NotVar
	LDAX D
	SUI 'A'+128	; if hi bit is set then length=1
	JNC Var			; and it must be a var
	
NotVar:
	PUSH H
	
	CALL LookupToken
	
	; HL points to the last char of the token we've found
	; Advance past this then get the token value
	INX H
	MOV A,M
	
; Store token in program
	LHLD PROG_PARSE_PTR
	MOV M,A
	INX H
	SHLD PROG_PARSE_PTR
	
	POP H
	
	JMP NextToken

Var:
	; Token value is var letter-'A'
	
	; Store var token in program
	LHLD PROG_PARSE_PTR
	MOV M,A
	INX H
	SHLD PROG_PARSE_PTR
	
	; DE points to the single-char varname, so swap it with HL and then inc to get a pointer to the next char to look at
	XCHG
	INX H
	
	JMP NextToken

ParseInteger:
	; DE points to integer
	; The integer will be constructed in HL
	; on return DE points to char after int

	LXI H,0
	
ParseIntegerNext:
	; Muliply by 10
	PUSH H
	POP B
	
	DAD H
	DAD H
	DAD B
	DAD H

	LDAX D
	INX D
	SUI '0'
	
	MVI B,0
	MOV C,A
	DAD B
	
	JP ParseIntegerNext
	
	; Subtract 128 because fhe last byte had hi bit set
	LXI B,0ff80h
	DAD B
	
	RET
	
Integer:
	CALL ParseInteger
	
	; At this point HL contains the integer
	; DE points to tbe char after end of token
	
	PUSH D
	XCHG
	
	; Store integer in program
	LHLD PROG_PARSE_PTR
	MVI M,IntegerToken
	INX H
	MOV M,E	; TODO code in common with var assign
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
	
	LXI H,TokenList-1
LookupTokenNext:
	INX H
	PUSH D
	CALL Strcmp
	POP D
	CPI 080h
	RZ
	
LookupTokenFindNext:
	MOV A,M
	CPI 0
	JZ Error
	INX H
	ANI 128	; TODO can save a few (3?) bytes by
					; combining with zero test above
					; somehow, and the fal through
					; would be the error condition
					; and could be called using RST
	JZ LookupTokenFindNext

	JMP LookupTokenNext

; DE points to hi-bit terminated string
; HL points to hi-bit terminated string
; Returns 128 in A on match
; Can't have 128 as a character in D
; (so assume that strings must be ASCII)
Strcmp: ; Depth = 3
	LDAX D
	CMP M
	RNZ
	
	ANI 128
	RNZ
	
	INX D
	INX H
	JMP Strcmp

; DE points to start
; HL points to dest
; A is length (max 127 bytes)

; Leaves HL pointing to char after string
StrCpy:
	DCR A
	RM		; DCR doesn't affect carry, but does 
				; affect sign bit, so 
	
	PUSH PSW
	LDAX D
	MOV M,A
	POP PSW

	INX D
	INX H
	
	JMP Strcpy
	


; Return the class of a character for tokenizing
; Digit
; Alphabetical
; All others are distinct classes

CharClass:
	CPI '0'
	RC	 			; LT '0' then return
	CPI 'Z'+1
	RNC				; GT Z then return
	CPI '9'+1
	JC Digit	; LTE 9 and it is a digit	
	CPI '<'
	RC				; LT '<' then return
	CPI '>'+1	; LTE '>' and its comp operator
	JC CompOp
	CPI 'A'
	RC 				; LT 'A' then return
	

	; Otherwise fall through
	
Alpha:
Digit:
; on entry A will be 0-9 or A-Z
; on exit it will be either 0 or P with high bit set
	ANI 60h
	ADI 90h
	RET
	
CompOp:
; on entry A will be < (60), = (61), > (62)
; on exit it will be < with high bit set
	ANI 3ch
	ORI 80h
	RET

; TODO base it on looking for 
; hi bit at end of string and inline
; outputstring
; this could save about 10 bytes
PrintStringToken:
	INX B
	LDAX B
	INX B
	
	CALL OutputString
	JMP PrintSubEndTest
	
OutputString:
;Pointer in B
;Length in A

	ORA A
	RZ
	
	PUSH PSW
	LDAX B
	RST 1
	POP PSW
	
	INX B
	
	DCR A
	JMP OutputString
	
GetLineNum:
	; Line number is in DE, look it up in the program and set BC to the position after it
	; return with Z set if successful
	; Z clear if not successful
	LXI B,PROG_BASE

GetLineNumLoop:
	INX B
	
	; Test for (BC)=DE, and return if true
	; (after advancing BC to next token)
	LDAX B
	SUB E
	MOV L,A
	INX B
	LDAX B
	INX B		; advance
	SUB D
	ORA L
	RZ
	
GetLineNumNext:
	CALL AdvanceToNextLineNum
	RNZ
	JMP GetLineNumLoop
	
AdvanceToNextLineNum:
; BC is a pointer to somewhere in the program
; move onto the next line number
; return with Z set if successful
; Z clear jf fell off end of program

	LDAX B
	
	SBI LinenumToken
	RZ
	
	INR A
	CPI (IntegerToken-LinenumToken+1)
	JZ ATNLN_Int
	CPI (StringToken-LinenumToken+1)
	JNZ ATNLN_NotInt 
	
ATNLN_String:
	INX B
	LDAX B
ATNLN_Int:
	; Add A onto BC
	ADD C
	MOV C,A
	
	MVI A,0
	ADC B
	MOV B,A
	
ATNLN_NotInt:
	INX B
	
	LHLD PROG_PTR
	MOV A,L
	SUB C
	MOV L,A
	MOV A,H
	SUB B
	ORA L
	JNZ AdvanceToNextLineNum
	
	; Error, fell off end of program
	;!Return with Z flag clear
	INR A
	RET

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
	JZ Ready
	
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
	MVI H,PrintSub/256
	
	; Jump to it
	PCHL

;Output the value in DE
PrintInteger:
	XRA A		; end marker is zero flag
	PUSH PSW
	
	MOV A,D
	ANI 80h
	XRI 080h+'-'	
	PUSH PSW		; sign bit clear if negative
							; zero flag clear
	CP NegateDE
	
PrintIntegerLoop:
	XCHG
	LXI D,10
	
	CALL DivideHL
	; HL contains remainder after / 10
	; DE contains the quotient
	
	MOV A,L
	ADI '0'
	MOV H,A
	XTHL		; swap (SP) (sign) with remainder
					; Top 2 bits of L are clear
					; so sign and zero will be clear
					; when this value is popped into
					; PSW
	PUSH H	; and push sign back
	
	; if DE is zero we are done
	MOV A,D
	ORA E
	JNZ PrintIntegerLoop
	
PrintIntegerLoop2:
	POP PSW
	RZ
	CP PutChar
	JMP PrintIntegerLoop2
	

; BC points to program
; DE contains value
; operator stack pointer stored in mem
; in OPERATOR_STACK_PTR
; Operand stack - SP

ExpEvaluate:
	LXI H,OPERATOR_STACK_BASE

ExpEvaluateNum:
	; Expecting ( var integer
	LDAX B
	CPI LeftBraceSub&0ffh
	JZ ExpLeftBrace
	CPI IntegerToken
	JZ ExpInteger
	CPI SubSub&0xff
	JZ ExpNegate
	CPI 26
	CNC Error
	
	; Fall through to ExpVar
ExpVar:
	; Get var value into DE
	PUSH H
	
	MVI H,VAR_SPACE/256
	ADD A
	MOV L,A
	
	MOV E,M
	INX H
	MOV D,M
	
	INX B
	
	POP H
	JMP ExpEvaluateOp
	
ExpNegate:
	; Put 0 onto stack and - onto
	; operator stack
	LXI D,0
	PUSH D
	
ExpLeftBrace:
	; push it onto operator stack
	MOV M,A
	INX H
	
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

ExpEvaluateOpRestore:
	LHLD OPERATOR_STACK_PTR
	
ExpEvaluateOp:
	;Expecting operator or right bracket or
	;end of expression
	
	;Are there operators on the stack?
	MOV A,L
	DCX H	; decrement now in anticipation of
				; needing to look at the top
				; in a moment
	CPI OPERATOR_STACK_BASE&0ffh
	JZ SkipExpApplyOp
	
	LDAX B
	
	CPI (RightBraceToken&0ffh)+1 ; operators or right bracket
	; Is it the end of the expression or a right bracket
	JC ExpApplyOp
	
	; or does operator on stack have GTE precedence?
	DCR A
	CMP M
	
	JNC SkipExpApplyOp ; no, dont apply op

ExpApplyOp:
	; POP operator from stack
	; this also handles the case when a
	; left brace is encountered
	
	MOV A,M
	; Store operator stack pointer, it
	; will be restored on return from operator
	SHLD OPERATOR_STACK_PTR
	
	; Put the address that we want to return to
	; into HL. 
	LXI H,ExpEvaluateOpRestore
	XTHL	; exhange with operand
	PUSH H	; put operand back onto stack
	
	; Address that we want to call into HL
	MVI H,PrintSub/256
	MOV L,A
	
	; Exchange with operand
	XTHL
	
	; use RET to call it
 	RET
	
SkipExpApplyOp:
	INX H	; undo the DCX that was done prior to jump
	
	LDAX B
	
	CPI RightBraceToken&0ffh ; operators or right bracket
	; Is it the end of the expression?
	RC
	
	; Push onto the operator stack
	MOV M,A
	INX H

	; move onto next token
	INX B
	
	;because we are
	;expecting another value, push DE onto stack
	PUSH D
	
	JMP ExpEvaluateNum

NegateDE:
	;flags are not affected
	MOV A,E
	CMA
	MOV E,A
	MOV A,D
	CMA
	MOV D,A
	INX D
	
	RET
	
; TokenList must be on same page and index to subroutine address must not overlap with other token values
ORG 02d0h

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
	DB "INPU",'T'+128
	DB InputSub&0ffh
	DB "EN",'D'+128
	DB 1
; Before this are keywords allowed at run-time
	DB "RU",'N'+128
	DB 1
	DB "LIS",'T'+128
	DB 1
	DB "NE",'W'+128
	DB  1
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
	DB '+'+128
	DB AddSub&0ffh
	DB '-'+128
	DB SubSub&0ffh
	DB '*'+128
	DB MulSub&0ffh
	DB '/'+128
	DB DivSub&0ffh
	DB 0	; zero can only occur at the end
	
PrintSub:
	LDAX B
	CPI StringToken
	JZ PrintStringToken
	CALL ExpEvaluate
	PUSH B
	CALL PrintInteger
	POP B

PrintSubEndTest:
	LDAX B
	INX B
	CPI CommaToken
	JZ PrintSub
	DCX B
	MVI A,10
	RST 1
	RET

LetSub:
	; TODO test that we have var and equal sign
	LDAX B
	PUSH PSW
	INX B
	INX B
	
	CALL ExpEvaluate
	
	POP PSW
	
AssignToVar:
	; Put DE into var
	
	MVI H,VAR_SPACE/256
	ADD A
	MOV L,A
	
	MOV M,E
	INX H
	MOV M,D
	
	RET
	
GotoSub:
	CALL ExpEvaluate
	CALL GetLineNum
	JNZ Error
	
GosubSub: ; Depth = 1
	CALL ExpEvaluate
	POP H	; Preserve return address
	PUSH B
	PUSH H
	CALL GetLineNum
	JNZ Error
	
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

InputSub:
	; TODO there is no check for var token
	; nor for integer input
	
	LXI H,INPUT_BUFFER-1
	CALL GetLine
	DCX H
	MOV A,M
	ORI 128
	MOV M,A
	
	LXI D,INPUT_BUFFER
	
	PUSH B
	CALL ParseInteger
	XCHG
	POP B
	
	LDAX B
	INX B
	
	JMP AssignToVar
	

LeftBraceSub:
	LDAX B
	; Is current operator a right brace?
	CPI RightBraceToken&0ffh
	CNZ Error ; expecting right brace
	INX B
	
; This is a dummy label
; to make sure that right brace token value is between LeftBraceSub and LTSub
RightBraceToken:

	RET

GTSub:
	; Swap operands and fall through
	XCHG
LTSub:
	DCX D
LTESub:
	; Swap operands and fall through
	XCHG
GTESub:
	CALL SubSub

GTESub_1:
	; SubSub will have been executed prior to this
	; If hi bit of D is clear return 1, else 0
	MVI A,80h
	ANA D
	JMP EqualSub_1
	
EqualSub:
	RST 3 ; returns Z iff HL=DE
EqualSub_1:
	LXI D,1
	RZ
	DCX D
	RET
	
NotEqualSub:
	RST 3 ; returns Z iff HL=DE
	LXI D,0
	RZ
	INX D
	RET

SubSub:
	CALL NegateDE
AddSub:
	;Add DE to top of stack and keep in DE
	DAD D
	XCHG
	
	RET

MulSub:
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
;Remainder in HL
;Result in DE

DivideHL:
;Divide HL by DE

	; Make HL +ve
MOV A,H
ORA A
PUSH PSW
XCHG
CM NegateDE
XCHG
	
	; Make DE -ve
MOV A,D
ORA A
PUSH PSW
CP NegateDE
;CALL NegateDE
	
;Divide HL by DE
;Assuming that HL is +ve and DE is -ve

	PUSH B
	LXI B,0ffffh ; Accumulator starts at -1
	
DivLoop:
	INX B
	DAD D
	JC DivLoop

; Assume we want +ve remainder
 	CALL NegateDE
 	DAD D


	PUSH B
	POP D
	
	POP B
	
	POP PSW
	CP NegateDE
	POP PSW
	CP NegateDE
	
	RET
