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
; 2023-04-23 About 970 bytes long
;			first draft of code for LIST added
; 2023-04-25 About 970 bytes long
;			LIST command working
; 2023-04-27 About 950 bytes long
;			More code size reduction
; 2023-04-28 About 950 bytes long
;			used RST_CompareJump to save
;			2 bytes for every CPI JZ where
;			the jump is to same page
; 2023-04-28 Free space: 78 bytes
; 2023-04-29 Free space: 84 bytes
;			Initialise PROG_PTR at start
;			Added NEW and END
;			Added direct statement handling
; 2023-04-30 Free space: 86 bytes
;			Fixed bugs with deleting first and
;			last program lines
; 2023-05-02 Free space: about 60 bytes
;			added code to allow out-of-order
;			line number entry (first draft)
;
; Memory map:
; system vars
; var space : 52 bytes
; input buffer : 73 bytes
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

org RAM_TOP-(64+9)
STACK_INIT:
INPUT_BUFFER:
OPERATOR_STACK_PTR: ; Lo byte must be B7
										; or a similar opcode
										; that doesn't do much
	DW 0
OPERATOR_STACK_BASE:
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB 0,0,0,0,0,0,0,0,0

INPUT_BUFFER_END:

ORG 0400h

; this must be on a 256 byte boundary
VAR_SPACE:
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0
	
PROG_PTR:
	DW 0
PROG_PARSE_PTR:
	DW 0

PROG_BASE:

ORG 00h
	; put PROG_BASE into PROG_PTR and
	; jump to Ready
	LXI H,PROG_BASE
	XRA A 
	INR A ; make sure that Z is not set
	JMP SetProgPtrReady
	
.macro RST_CompareJump
RST 1
.endm
ORG 08h
; byte after RST is compared with A
; if equal then jump to address on same page.
;
; only use where performance is not
; important (parsing, printing)
	XTHL
	CMP M
	INX H
	JNZ CompareJump_Skip
	MOV L,M
	
CompareJump_Skip:
	
	; Overflow 3 instructions into next RST, which
	; is short, so can interleave to fit extra
	; 3 instructions in
	
	DB 0feh ; Opcode for CPI eats next byte
	
.macro RST_NewLine
RST 2
.endm
ORG 10h
NewLine:
; can only be called from places where the
; value in DE can be overwritten
	DB 11h ; Opcode LXI D eats next 2 bytes
	INX H
	DB 0feh ; Opcode for CPI eats next byte
	DB 11h ; Opcode LXI D eats next 2 bytes
	XTHL
	RET
	
	MVI A,10

	; fall through

.macro RST_PutChar
RST 3
.endm
ORG 18h

; PutChar is called frequently
; it can be called using RST 1

PutChar:
	; Assume that port 0 is for char I/O
	; And port 1 is for char ready status
	
	OUT 0
	RET

; Space for 5 byte subroutine(s)

.macro RST_GetDEatBC
RST 4
.endm
ORG 20h
	LDAX B
	MOV E,A
	INX B
	LDAX B
	MOV D,A
	INX B
	RET

.macro RST_CompareHLDE
RST 5
.endm
ORG 28h

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


.macro RST_NegateDE
RST 6
.endm
ORG 30h
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
	
	RST_CompareJump
	DB LeftBraceSub&0ffh,(ExpLeftBrace&0ffh)-1
	RST_CompareJump
	DB IntegerToken,(ExpInteger&0ffh)-1
	RST_CompareJump
	DB SubSub&0xff,(ExpNegate&0ffh)-1
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
	
	; fall through to ExpEvaluateOp
	db 3eh ; opcode for MVI A eats next byte

ExpInteger:
	RST_GetDEatBC
	
	; fall through to ExpEvaluateOp
	db 3ah ; opcode for LDA eats 2 bytes
				 ; and 3rd byte of instruction
				 ; is RLC, so has no effect
	
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
	RST_CompareJump
	DB OPERATOR_STACK_BASE&0ffh,(SkipExpApplyOp&0ffh)-1
	
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
	
	INX B
	
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

; This 9 byte routine must reside in page 0
; so that the last byte of a call to it is NOP
OutputString:
;Pointer in B points to string token marker
	; we can get the length into D using
	; RST_GetDEatBC
	; we just ignore E
	RST_GetDEatBC
	
OutputString_Loop:
;length is in D
;pointer to string is in B
  DCR D
  RM

	LDAX B
	RST_PutChar
	
	INX B
	
	JMP OutputString_Loop
	
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
	
Ready:
	; Set stack pointer to just below input buffer
	; Do this every time to guard against
	; GOSUB with no RETURN errors
	
	LXI H,INPUT_BUFFER
	SPHL
	; H is also the initial pointer to input buffer
	PUSH H
	
	RST_NewLine
	CALL Getline
	
	LHLD PROG_PTR
	SHLD PROG_PARSE_PTR
	
	; Now we have a line terminated by chr(10)
	; Get location of input buffer into HL
	; and put PROG_PTR onto stack because
	; we will needed it after parsing
	XTHL

	CALL NextToken
	
	POP H ; get PROG_PTR back
	
	MOV A,M
	RST_CompareJump
	DB IntegerToken,(LineStartsWithInt&0ffh)-1

ExecuteDirect: ; Depth = 0
	; HL contains PROG_PTR
	; Put the end program marker there
	; This overwrites the token to execute,
	; but we've already got that in A
	MVI M,EndProgram
	
	MOV B,H
	MOV C,L
	
	JMP ExecuteProgramNotLineNum
	
LineStartsWithInt:
	; Get the line number into DE
	PUSH H
	POP B
	INX B
	RST_GetDEatBC

	; Put this in so that
	; GetLineNum works
	MVI M,EndProgram
	
	; Is it an integer all by itself? 
	; If so then delete the line
	
	LDA PROG_PARSE_PTR
	SUB L
	RST_CompareJump
	DB 3,(DeleteProgramLine&0ffh)-1

	; If first token was an int, change it to a
	; LinenumToken and add the line to the program
	

	; call GetLineNum to find either the line, or
	; pointer to next location in program after it
	
	; save line number for later
	PUSH D
	
	CALL GetLineNum
	
	; if GetLineNum returns a match then this is
	; an error, user must delete line first
	CZ Error
	
	MVI M,LineNumToken
	
	; do a memory rotate with
	; first = GetLine/ATNLN address
	; middle = PROG_PTR
	; last = PROG_PARSE_PTR
	
	; DE=last
	LHLD PROG_PARSE_PTR
	XCHG
	
	; (SP)=first
	PUSH B
	
	; BC=middle
	LHLD PROG_PTR
	MOV B,H
	MOV C,L
	
  ; then set PROG_PTR to PROG_PARSE_PTR
	XRA A ; make sure we don't execute JNZ Ready
	
	LHLD PROG_PARSE_PTR
	
SetProgPtrReady: ; shared code
	SHLD PROG_PTR
	
	JNZ Ready
	
	JMP MemoryRotate
	
DeleteProgramLine:
	CALL GetLineNum
	JNZ Ready		; if line not found, do nothing

	PUSH B
	
	CALL ATNLN_NotInt
	
	; GetLineNum goes to the second byte of line number, so go back to line num
	POP D
	DCX D
	DCX D
	PUSH D
	
	; Both DE and (SP) contain 'first' ptr
	; HL contains PROG_PTR
	; (HL was set to PROG_PTR at call)
	
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

	; (SP) = first
	; DE = last
	; BC = middle

	; Z will still be as returned from
	; AdvanceToNextLineNum
	; if Z was clear it means that line to delete
	; is last line, so no need to do MemoryRotate

	JNZ Ready
	
MemoryRotate:

MR_SetNextMiddle:
	; next = middle
	MOV H,B
	MOV L,C
	
; MemoryRotate is the entry point for the memory rotate algorithm
;(SP) = first
;DE = last
;BC = middle
;HL is used as the next pointer

;DE is preserved, no other registers are

	; fall through - doesn't matter which branch is 
	; taken at JC below
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
	MOV A,M
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

;NextToken, Integer, String
;all need to be on the same page
; (currently all on page 1)
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
	
	; These all need to be on the same page
	; as this block
	RST_CompareJump
	DB ' ',(NextToken&0ffh)-1
	RST_CompareJump
	DB 0c0h,(Integer&0ffh)-1
	RST_CompareJump
	DB '"',(String&0ffh)-1
	
	LDAX D
	SUI 'A'+128	; if hi bit is set then length=1
	JNC Var			; and it must be a var
	
NotVar:
	PUSH H
	
	LXI H,TokenList
	CALL LookupToken
	
	; C contains the token value
	MOV A,C
	
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
	MVI M,EndProgram ; will get overwritten by
									 ; next token
	SHLD PROG_PARSE_PTR
	
	POP H
	
	JMP NextToken

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
	
	ANI 0fh
	
	MVI B,0
	MOV C,A
	DAD B
	
	LDAX D ; test hi bit
	ORA A
	
	INX D
	JP ParseIntegerNext
	
	XCHG  ; both places where this is called from
				; require XCHG
	
	RET

; DE points to start of token
; HL points 1 char after
; C contains the token value
LookupToken:

	MOV C,M	
	
	PUSH D
	CM StrcmpEntry
	POP D

	RZ 

	MOV A,M
	INR A
	INX H
  JNZ LookupToken
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
	
GetLineNum:
	; Line number is in DE, look it up in the program and set BC to the second byte of line num
	; preserves DE
	; HL is not preserved
	; return with Z set if successful
	;
	; Z clear if not successful, and BC points
	; to the first byte of the line with number
	; greater than the request
	
	LXI B,PROG_BASE

GetLineNumLoop:
	CALL AdvanceToNextLineNum
	RNZ
	
	INX B
	
	; Test for DE <= (BC), and return if true
	; TODO - check that all of the logic works
	; in all cases
	LDAX B
	SUB E
	MOV L,A
	INX B
	LDAX B
	INX B
	SBB D ; C set if DE > (BC), and Z not set
				; C clear if DE <= (BC)
	JC GetLineNumLoop
	
	; Now we want Z set if DE=(BC), clear
	; otherwise 
	ORA L
	
	RET
	

ATNLN_Loop:
	
	SUI LinenumToken
	RZ
	
	INR A
	RST_CompareJump
	DB (IntegerToken-LinenumToken+1),(ATNLN_Int&0ffh)-1
	CPI (StringToken-LinenumToken+1)
	JNZ ATNLN_NotInt 
	
ATNLN_String:
	INX B
	LDAX B
ATNLN_Int:
	; Add A onto BC
	ADD C
	MOV C,A
	
	; save a byte with ADC B, SUB C, MOV B,A
	;MVI A,0
	;ADC B
	;MOV B,A
	
	ADC B
	SUB C
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
	JNZ ATNLN_Loop
	
	; fell off end of program
	
	INR A
	; Z flag will be clear at this point

	RET

; List statement main loop
ListLoop:
	MVI A,' '
	RST_PutChar
	
	LDAX B
	CPI EndProgram
	RZ
	
  LXI H,ListLoop	; so that we can loop using RET
  PUSH H

	LXI H,TokenList

	; These need to be on same page
	; currently on page 2
	RST_CompareJump
	DB StringToken,(List_String&0ffh)-1
	RST_CompareJump
	DB IntegerToken,(List_Integer&0ffh)-1
  CPI LinenumToken
  JNZ List_Token
  
List_LineNum:
	RST_NewLine
 
List_Integer:
  INX B
  RST_GetDEatBC
 
 	; fall through to PrintInteger
 	
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
 
List_Token_Loop:
  MOV A,M
  INR A
  JZ List_Var
  INX H
  JP List_Token_Loop

List_Token:
  LDAX B
  CMP M
  INX H
  JNZ List_Token_Loop

List_Token_String_Loop:
  MOV A,M
  ANI 07fh
  RST_PutChar
  ORA M
  INX H
  JP List_Token_String_Loop
  
  INX B
  RET

List_String:
	MVI A,34
	RST_PutChar
	CALL OutputString
	MVI A,34-'A'
	DB 11h ; opcode for LXI D to eat next 2 bytes

List_Var:
  LDAX B
  INX B
  ADI 'A'
  RST_PutChar
  RET


; Index to subroutine address must not overlap with other token values
ORG 02d9h

TokenList:
	DB PrintSub&0ffh
	DB "PRIN",'T'+128
	DB LetSub&0ffh
	DB "LE",'T'+128
	DB GotoSub&0ffh
	DB "GOT",'O'+128
	DB GosubSub&0ffh
	DB "GOSU",'B'+128
	DB ReturnSub&0ffh
	DB "RETUR",'N'+128
	DB IfSub&0ffh
	DB "I",'F'+128
	DB InputSub&0ffh
	DB "INPU",'T'+128
	DB ExecuteProgram&0ffh
; Before this are keywords allowed at run-time
	DB "RU",'N'+128
	DB ListSub&0ffh
	DB "LIS",'T'+128
	DB NewSub&0ffh
	DB "NE",'W'+128
	DB EndSub&0ffh
	DB "EN",'D'+128
	DB CommaToken
	DB ','+128
	DB LeftBraceSub&0ffh
	DB '('+128
;After this label, only expect things which count as operators
  DB RightBraceToken&0ffh
	DB ')'+128
	DB LTSub&0ffh
	DB '<'+128
	DB GTSub&0ffh
	DB '>'+128
	DB GTESub&0ffh
	DB ">",'='+128
	DB LTESub&0ffh
	DB "<",'='+128
	DB EqualSub&0ffh
	DB '='+128
	DB NotEqualSub&0ffh
	DB "<>"+128
	DB AddSub&0ffh
	DB '+'+128
	DB SubSub&0ffh
	DB '-'+128
	DB MulSub&0ffh
	DB '*'+128
	DB DivSub&0ffh
	DB '/'+128
	DB 255; 255 can only occur at the end

ExecuteProgram: ; Depth = 0
	
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
	RST_CompareJump
	DB StringToken,(PrintStringToken&0ffh)-1

	RST_ExpEvaluate
	PUSH B
	CALL PrintInteger
	POP B
	
	DB 11h ; Skip over first 2 bytes of call
				 ; instruction to fall through
				 ; OutputString is in page 0 so
				 ; last byte is NOP
PrintStringToken:
	CALL OutputString

PrintSubEndTest:
	LDAX B
	INX B
	
	RST_CompareJump
	DB CommaToken,(PrintSub&0ffh)-1
	
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
	INX B
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
	
ListSub:
  LXI B,PROG_BASE
  JMP ListLoop

NewSub:
	RST 0
	
EndSub:
	JMP Ready
	
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
	
	MOV A,H
	RAL
	
	DB 11h; LXi D opcode to swallow next byte
	
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
	
