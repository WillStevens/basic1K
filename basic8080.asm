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
; 2023-04-09 About 970 bytes long. 
;		 more code size reductions
;		 first draft of memory rotate function added
; 2023-04-12 About 995 bytes long
;		 line deletion function more complete
;		 looking for better way of decreasing 
;		 PROG_PTR after line deletion
; 2023-04-16 About 986 bytes long
;    line deletion apparently working
;    some code size reductions
; 2023-04-16 About 963 bytes long
;			further code size reductions
; 2023-04-17 About 930 bytes long
;			looked for subroutine code sharing and
;			LXI trick optimisations.
;			Likely that some bugs will have been
;			introduced when doing this
; 2023-04-18 About 900 bytes long
;			greatly reduced CharClass size
; 2023-04-20 About 880 bytes long
;			more code size reduction
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
EndProgram equ 30

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
	JMP Ready
	
; 5 bytes free

.macro RST_PutChar
RST 1
.endm
ORG 08h

; PutChar is called frequently
; it can be called using RST 1

PutChar:
	; Assume that port 0 is for char I/O
	; And port 1 is for char ready status
	
	OUT 0
	RET

; Space for 5 byte subroutine(s)

.macro RST_CompareHLDE
RST 3
.endm
ORG 18h

CompareHLDE:
; compare HL and DE, return
; Z equal, NZ if not equal
; C equal, NC if not equal
; A will be zero if Z is set
	MOV A,L
	XRA E
	RNZ
	MOV A,H
	XRA D
	RNZ
	STC
	RET

;TODO - making this return C or NC would enable
; savings elsewhere

.macro RST_NegateDE
RST 4
.endm
ORG 20h
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
	
.macro RST_NewLine
RST 6
.endm
ORG 30h
NewLine:
	MVI A,10
	RST_PutChar
	RET

; 4 bytes free

.macro RST_ExpEvaluate
RST 7
.endm
ORG 38h

; BC points to program
; DE contains value
; operator stack pointer stored in mem
; in OPERATOR_STACK_PTR
; Operand stack - SP

ExpEvaluate:
	LXI H,OPERATOR_STACK_BASE

ExpEvaluateNum:
	; Expecting ( var integer or - sign
	LDAX B
	INX B
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
	
	POP H
	JMP ExpEvaluateOp

ExpInteger:
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
	INX B
	
	CPI RightBraceToken&0ffh ; operators or right bracket
	; Is it the end of the expression?
	RC
	
	; The sequence below was shared with ExpNegate
	; so use a CPI to mop up the initial
	; LXI in ExpNegate, saving 5 bytes
	
	; Push onto the operator stack
	;MOV M,A
	;INX H
	; move onto next token
	;INX B
	;because we are
	;expecting another value, push DE onto stack
	;PUSH D
	;JMP ExpEvaluateNum
	
	DB 0feh ; OpCode for CPI to mop up LXI
ExpNegate:
	; Put 0 onto stack and - onto
	; operator stack
	LXI D,0
	PUSH D
	
ExpLeftBrace:
	; push it onto operator stack
	MOV M,A
	INX H
	
	JMP ExpEvaluateNum
	
GetLine:
	IN 1
	ANI 1
	JZ GetLine
	IN 0
	
	MOV M,A
	CPI 10
	RZ
	
	INX H
	MOV A,L
	CPI INPUT_BUFFER_END&0ffh
	JNZ GetLine
	
	; Fall through to Error

;Display error code and go back to line entry
Error:
	RST_NewLine
	MVI A,'E'
	RST_PutChar
	POP PSW		; discard return address and
						; get error code
	ANI 0fh
	ADI 'A'
	RST_PutChar
	RST_NewLine
	
Ready:
	LHLD PROG_PTR
	SHLD PROG_PARSE_PTR

	; Set stack pointer to just below input buffer
	; Do this every time to guard against
	; GOSUB with no RETURN errors
	
	LXI H,INPUT_BUFFER
	SPHL
	; H is also the initial pointer to input buffer
	PUSH H

	CALL Getline
	
	; Now we have a line terminated by chr(10)
	; Get location of input buffer into HL
	POP H

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
	; LinenumToken and add the line to the program
	
	MVI M,LinenumToken
	LHLD PROG_PARSE_PTR
	SHLD PROG_PTR
	
	JMP Ready

DeleteProgramLine:
	INX H
	MOV E,M
	INX H
	MOV D,M
	
	DCX H
	DCX H
	
	CALL GetLineNum
	JNZ Ready		; if line not found, do nothing

	PUSH B
	
	CALL AdvanceToNextLineNum
	
	; GetLineNum goes to the program statement after line number, so go back to line num
	POP D
	DCX D
	DCX D
	DCX D
	PUSH D
	
	; Both DE and (SP) contain 'first' ptr
	; HL contains PROG_PTR
	; HL was set to PROG_PTR at call, and also
	; may bave been set in AdvanceToNextLineNum
	
	; B-(SP) is the amount we need to set PROG_PTR back by...
	XTHL	; DE=first, HL = first, (SP)=PROG_PTR
	RST_NegateDE
	XCHG	; DE = first, HL=-first
	DAD B	; HL = middle-first
	
	; Now HL contains the amount to decrease PROG_PTR by and (SP) contains PROG_PTR
	; we want to put PROG_PTR into DE and 
	; PTOG_PTR-HL into PROG_PTR
	
	XCHG	; DE=middle-first, HL=first
	RST_NegateDE ; DE=first-middle
	XTHL	; HL=PROG_PTR, (SP)=first
	XCHG	; DE=PROG_PTR, HL=first-middle
	DAD D 
	
	; Now DE contains PROG_PTR and HL contains
	; what we want to put into PROG_PTR
	
	SHLD PROG_PTR

	; Z will still be as returned from
	; AdvanceToNextLineNum
	; if Z was clear it means that line to delete
	; is last line, so no need to do MemoryRotate
	JNZ Ready
	
	; (SP) = first
	; DE = last
	; BC = middle
	
	JMP MemoryRotate
	; TODO could save a bye by setting HL=BC
	; and falling through?

; MemoryRotate is the entry point for the memory rotate algorithm
;(SP) = first
;DE = last
;BC = middle
;HL is used as the next pointer

;DE is preserved, no other registers are

MR_CompareBC_atSP:
	XTHL
	MOV A,L
	SUB C
	MOV A,H
	SBB B
	; now carry is set if middle > first
	; and will be clear when middle = first
	XTHL
	JC MR_Loop

MR_SetMiddleNext:
	; middle = next
	MOV B,H
	MOV C,L

MR_SetNextMiddle:
MemoryRotate:
	; next = middle
	MOV H,B
	MOV L,C

MR_Loop:
	;while (first != next)
	;Compare (SP) with HL and return if equal
	XCHG
	XTHL
	RST_CompareHLDE
	XTHL
	XCHG
	
	JZ Ready

	;swap (*first++,*next++)
	MOV A,M
	XTHL
	PUSH D
	MOV E,M
	MOV M,A
	MOV A,E
	POP D
	INX H
	XTHL
	MOV M,A
	INX H

	; if (next==last) next=middle

	RST_CompareHLDE
	JZ MR_SetNextMiddle

	; else if (first == middle) middle = next

	JMP MR_CompareBC_atSP

ExecuteDirect: ; Depth = 0
	
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
	CPI 0c0h
	JZ Integer
	CPI '"'
	JZ String
	LDAX D
	SUI 'A'+128	; if hi bit is set then length=1
	JNC Var			; and it must be a var
	
NotVar:
	PUSH H
	
	LXI H,TokenList
	CALL LookupToken
	
	; HL points to the last char of the token we've found
	; Advance past this then get the token value
	INX H
	MOV A,M
	
	DB 21h ; opcode of LXI H to skip 2 bytes
	
Var:
	; DE points to the single-char varname, so inc to get a pointer to the next char to look at and put on stack to get back after updating PROG_PARSE_PTR
	
	INX D
	PUSH D
	
	; Store token in program
	LHLD PROG_PARSE_PTR
	MOV M,A
Var_Share_Entry:
	INX H
Var_Share_Entry2:
	SHLD PROG_PARSE_PTR
	
	POP H
	
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
	
	XCHG  ; both places where this is called
				; have XCHG afterwards
	
	RET
	
Integer:
	CALL ParseInteger
	
	; At this point DE contains the integer
	; HL points to tbe char after end of token
	
	PUSH H
	
	; Store integer in program
	LHLD PROG_PARSE_PTR
	MVI M,IntegerToken
	INX H
	MOV M,E	; TODO code in common with var assign
	INX H
	MOV M,D

	JMP Var_Share_Entry

; DE points to first double quote
; HL-1 is last double quote
String:
	INR E
	MOV A,L
	SUB E
	DCR A
	;A contains the length, Z set if zero length
	
	PUSH H
	
	;Store token ID, length and string
	LHLD PROG_PARSE_PTR
	MVI M,StringToken
	INX H
	MOV M,A
	INX H
	CNZ StrCpy ; only call if not zero length
	JMP Var_Share_Entry2
	
LookupTokenNext:
	INX H
	
	PUSH D
	CM StrcmpEntry
	POP D
	RZ
	
; DE points to start of token
; HL points 1 char after
LookupToken:
	MOV A,M
	ORA A

  JNZ LookupTokenNext
	CALL Error

StrcmpEntry:
	INX H
	
; DE points to hi-bit terminated string
; HL points to hi-bit terminated string
; Returns Z set if match
; Can't have 128 as a character in D
; (so assume that strings must be ASCII)
Strcmp: ; Depth = 3
	LDAX D
	CMP M
	RNZ	; On return Z will be clear
	
	CMA
	ANI 128
	RZ	; On return Z will be set 
	
	INX D
	INX H
	JMP Strcmp

; DE points to start
; HL points to dest
; Use hi-bit to detect end, which will be quote
; so don't copy it
; StrCpy doesn't get called for zero length

; Leaves HL pointing to char after string
StrCpy:
	LDAX D
	ORA A
	RM
	MOV M,A

	INX D
	INX H
	
	JMP Strcpy

; Return the class of a character for tokenizing
; Digit
; Alphabetical
; All others are distinct classes

CharClass:
; each character less than 0 is a 
; distinct class.
; 0-9 is a class (class C)
; : to > is a class (class 8)
; >= @ is a class (class 9)
	CPI '0'
	RC	 			; LT '0' then return
	CPI '9'+1
	RAR
	STC
	RAR
	ANI 0f0h ; top 4 bits give the class
					 ; hi bit must be set to signify that
					 ; these are characters that group
					 ; together
	
	RET

PrintStringToken:
	INX B
	LDAX B
	MOV E,A ; Put length into E
	INX B
	
	CALL OutputString
	JMP PrintSubEndTest
	
OutputString:
;Pointer in B
;Length in E
  DCR E
  RM

	LDAX B
	; TODO putting ANI 07fh here would make this
	; usable for LIST statement
	RST_PutChar
	
	INX B
	
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
	INX B
	LDAX B
	INX B		; advance
	JNZ GetLineNumNext
	SUB D
	RZ
	
GetLineNumNext:
	CALL AdvanceToNextLineNum
	RNZ
	JMP GetLineNumLoop
	

ATNLN_Loop:
	
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
	
	; TODO save a byte with ADC B, SUB C, MOV B,A
	MVI A,0
	ADC B
	MOV B,A
	
ATNLN_NotInt:
	INX B

AdvanceToNextLineNum:
; BC is a pointer to somewhere in the program
; move onto the next line number
; return with Z set if successful
; Z clear if fell off end of program

	LDAX B
	CPI EndProgram
	JNZ AdvanceToNextLineNum 
	
	; fell off end of program
	
	INR A
	; Z flag will be clear at this point

	RET

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
	
; TokenList must be on same page and index to subroutine address must not overlap with other token values
ORG 02e0h

TokenList:
	; First 2 bytes make sure that P is the first
	; char compared
	DB 128,1
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
	DB 0	; 0 can only occur at the end

ExecuteProgram: ; Depth = 0
	; HL contains PROG_PTR
	; Put the end program marker there
	MVI M,EndProgram
	
	; Point BC to first line
	; Skip over the line number
	LXI B,PROG_BASE+3

ExecuteProgramLoop:
	; Check that we haven't reached end of program
	LDAX B
	CPI EndProgram
	JZ Ready
	
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
	; ExecuteProgramLoop must be on the same page
	; page as PrintSub so that we don't have to
	; update H
	
	; Jump to it
	PCHL
	
PrintSub:
	LDAX B
	CPI StringToken
	JZ PrintStringToken
	RST_ExpEvaluate
	PUSH B
	CALL PrintInteger
	POP B

PrintSubEndTest:
	LDAX B
	INX B
	CPI CommaToken
	JZ PrintSub
	DCX B
	RST_NewLine
	RET

LetSub:
	; TODO test that we have var and equal sign
	LDAX B
	PUSH PSW
	INX B
	INX B
	
	RST_ExpEvaluate
	
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
	
GosubSub: ; Depth = 1
	RST_ExpEvaluate
	POP H	; Preserve return address
	PUSH B
	PUSH H
	
	DB 03eh ; opcode for MVI A to eat next byte
GotoSub:
	RST_ExpEvaluate
	CALL GetLineNum
	RZ
	CALL Error
	
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
	RST_ExpEvaluate
	MOV A,E
	ORA D
	RNZ

	; If DE zero then fall through to next line
	JMP AdvanceToNextLineNum

InputSub:
	; TODO there is no check for var token
	; nor for integer input
	
	LXI H,INPUT_BUFFER
	PUSH H
	CALL GetLine
	DCX H
	MOV A,M
	ORI 128
	MOV M,A
	
	POP D
	
	PUSH B
	CALL ParseInteger
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
	
; This is a dummy label that is never called
; to make sure that right brace token value is between LeftBraceSub and GTSub
RightBraceToken:

	; Because tbis operator doesn't consume an
	; operand, the thing in HL isn't an
	; operand and needs to be exchanged with
	; the return address on the stack

	XTHL	; exchange with return address
	PCHL	; jump to return address

GTSub:
	; Swap operands and fall through
	XCHG
LTSub:
	DCX D
LTESub:
	; Swap operands and fall through
	XCHG
GTESub:
	RST_NegateDE
	DAD D
	
	DB 11h ; LXI D opcode to swallow next 2 bytes
	
EqualSub:
	RST_CompareHLDE ; returns Z iff HL=DE
	CMC
	DB 3eh ; MVI A opcode to swallow next byte
	
NotEqualSub:
	RST_CompareHLDE; returns Z iff HL=DE
	LXI D,1
	RNC
	DCX D
	RET

AddSub:
	DB 3eh	; opcode for MVI A, to eat next byte
SubSub:
	RST_NegateDE
	;Add DE to HL and keep in DE
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
 	RST_NegateDE
 	DAD D


	PUSH B
	POP D
	
	POP B
	
	POP PSW
	CP NegateDE
	POP PSW
	RM
	RST_NegateDE
	
	RET
