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
; 2023-05-02 Free space: about 54 bytes
;			all basic functionality now implemented
;			items to improve:
;			division
;			syntax checking
; 2023-05-04 Free space: about 79 bytes
;			more code size reduction
;			partly through handling EndProgram
;			and LineNum better in threaded code
;			likely to have introduced bugs
; 2023-05-07 Free space: about 69 bytes
;			Improved expression evaluation by
;			making it recursively callable
;			and no longer requiring operator stack
;			and about 20 bytes shorter.
;			Used freed space for more syntax checks
; 2023-05-08 Free space: about 44 bytes
;			First draft of support for array var @
; 2023-05-17 Free space: About 100 bytes
;			First draft of new parser from
;			experiments/parsing3.asm.
;			Still need to modify string representation
;			and change how INPUT parses integer,
;			and check string token doesn't interfere
;			after TokenList, and check order in
;			TokenList.
;			Seems likely that enough space has been
;			freed to be able to implement FOR...NEXT
; 2023-05-19 Free space: About 101 bytes
;			Issues listed above have been addressed now
;			Testing needed to iron out problems

; For development purposes assume we have
; 1K ROM from 0000h-03FFh containing BASIC
; 1K RAM from 0400h-07FFh

RAM_TOP equ 0800h ; 1 more than top byte of RAM

; Token values
; 0-31 are variables (0 = @)

; IntegerToken must be one greater than last var
IntegerToken equ 32 ; followed by 16-bit integer
StringToken equ 34 ; followed by string, followed by end quote

; Callable tokens are low byte of subroutine to call

; Errors are display as Ex where x is an error
; code which is tbe address on the stack when
; Error subroutine is called.

; Input buffer is just 8 bytes long
; used by input statement to get an integer.
; If there is a buffer overflow because user
; enters too much, the behaviour is system
; dependent - e.g. if writes above RAM
; space do nothing then its not a problem.
; If memory space repeats and lower 1K 
; is ROM then also not much of a problem.

; Stack is just below input buffer to save code
; when initialising

org RAM_TOP-8
INPUT_BUFFER:
STACK_INIT:

ORG 0400h

; this must be on a 256 byte boundary
VAR_SPACE:
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0
	DW 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	
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
	
ExpApplyOp: ; shared code
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

; Space for 5 bytes - likely to need for
; any real implementation of PutChar

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
; Stack is used for both operands and
; operators

ExpEvaluate:

; This puts a marker on the stack to
; detect when there are operators on the
; stack - operators all have 3 as the hi byte
; but this call puts hi byte 0 on the stack

	CALL ExpEvaluateNum
	CNC Error
	RET

ExpEvaluateOptional:
	CALL ExpEvaluateNum
	RC
	DCX B
	RET
	
ExpEvaluateNum:
	; Expecting ( var integer or - sign
	LDAX B
	INX B
	
	RST_CompareJump
	DB LeftBraceToken&0ffh,(ExpLeftBrace&0ffh)-1
	RST_CompareJump
	DB SubSub&0xff,(ExpNegate&0ffh)-1
	CPI IntegerToken
	JZ ExpInteger
	; Integer token is 27, so if carry is set then it is a var
	RNC : return with carry clear if error

	; Fall through to ExpVar
ExpVar:
	CALL GetVarLocation
	
ExpVarGetValue:
	MOV E,M
	INX H
	MOV D,M
	
	; fall through to ExpEvaluateOp
	db 3eh ; opcode for MVI A eats next byte

ExpInteger:
	RST_GetDEatBC
	
	; fall through to ExpEvaluateOp
	
ExpEvaluateOp:
	;Expecting operator or right bracket or
	;end of expression
	
	;Are there operators on the stack?
	POP H
	
	;H will be 0 if no operators on
	; stack (i.e. high byte of return address)
	
	MOV A,H
	RST_CompareJump
	DB 0,(SkipExpApplyOp&0ffh)-1
	
	LDAX B
	
	; No longer needed since case below
	; includes this
	;CPI Operators&0ffh 
	; Is it the end of the expression?
	;JC ExpApplyOp
	
	; Does operator on stack have GTE precedence? 
	; (or end of expression, when A < operators)
	DCR A
	CMP L
	
	JC ExpApplyOp ; yes, dont apply op
	
SkipExpApplyOp:
	PUSH H		; put operator that was on stack
						; back onto stack
	
	LDAX B
	
	CPI Operators&0ffh ; operators or right bracket
	; Is it the end of the expression?
	RC
	
	INX B
	
	; Code shared with ExpNegate
	; so use a CPI to mop up the initial
	; LXI in ExpNegate
	
	DB 0feh ; OpCode for CPI to mop up LXI
ExpNegate:
	; Put 0 onto stack and - onto
	; operator stack
	LXI D,0
	
	LXI H,ExpEvaluateOp ; address to return to
											; after operator is called
	PUSH H
	
	PUSH D							; operand

	MOV L,A							; operator address
	MVI H,PrintSub/256
	PUSH H
	
	JMP ExpEvaluateNum
	
ExpLeftBrace:
	RST_ExpEvaluate
	
	LDAX B
	INX B
	
	RST_CompareJump
	DB RightBraceToken&0ffh,(ExpEvaluateOp&0ffh)-1
	
	CALL Error
	
; This 9 routine must start in page 0
; so that the last byte of a call to it is NOP
OutputString:
;Pointer in B points to string token marker
OutputStringLoop:
	INX B
	LDAX B
	CPI StringToken
	RZ
OutputString_WithQuote:
	RST_PutChar
	JMP OutputStringLoop
	


;Display error code and go back to line entry
Error:
	RST_NewLine
	MVI A,'E'
	RST_PutChar
	POP D
	CALL PrintInteger
	
Ready:
	; Set stack pointer
	; Do this every time to guard against
	; GOSUB with no RETURN errors
	
	LXI H,STACK_INIT
	SPHL
	
	RST_NewLine
	
	LHLD PROG_PTR
	PUSH H ; push it because we need it after 
				 ; GetLine

	CALL GetLine
	
	MVI M,EndProgram&0ffh
	
	SHLD PROG_PARSE_PTR
	POP H
	
	MOV A,M
	; Regardless of which branch taken
	; we need this marker here.
	; This overwrites the token to execute,
	; but we've already got that in A
	MVI M,EndProgram&0ffh
	
	PUSH H
	POP B
	
	CPI IntegerToken
	JNZ ExecuteDirect
	
LineStartsWithInt:
	; Get the line number into DE
	INX B
	RST_GetDEatBC
	
	; Is it an integer all by itself? 
	; If so then delete the line
	
	LDA PROG_PARSE_PTR
	SUB L
	RST_CompareJump
	DB 3,(DeleteProgramLine&0ffh)-1

	; call GetLineNum to find either the line, or
	; pointer to next location in program after it
	
	CALL GetLineNum
	
	; if GetLineNum returns a match then this is
	; an error, user must delete line first
	CZ Error
	
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
	MVI M,LineNumSub&0ffh; undo what we did earlier
	MOV B,H
	MOV C,L
	
  ; then set PROG_PTR to PROG_PARSE_PTR
	XRA A ; make sure we don't execute JNZ Ready
				; in a moment
	
	LHLD PROG_PARSE_PTR
	
	JMP SetProgPtrReady
	
DeleteProgramLine:
	PUSH H
	CALL GetLineNum
	POP H
	JNZ Ready		; if line not found, do nothing

	PUSH B
	
	INX B
	CALL ATNLN_Int
	
	POP D
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
	
SetProgPtrReady: ; code shared by three things
	SHLD PROG_PTR

	; (SP) = first
	; DE = last
	; BC = middle

	; If calling from DeleteProgramLine then
	; Z will still be as returned from
	; AdvanceToNextLineNum
	; if Z was clear it means that line to delete
	; is last line, so no need to do MemoryRotate

	; If calling from insert then Z will be set
	
	; If calling from RST 0 Z will be clear
	
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

; GetLine sits entirely in page 1
; good - it uses RST_CompareJump in two
; places, so be careful if moving it

NLTestTrue:
	POP H
	RET
	
GetLine:
	; HL points where we want the line to be
	; paraed to.
	; On return HL points to byte adter what we've 
	; got.
	
	PUSH H
	MOV B,H ; B won't be 10

FreshStart:

  LXI H,NoCharClass
	
NLTest:
	MOV A,B; check for newline
	RST_CompareJump
	DB 10,(NLTestTrue&0ffh)-1
	
NextCharLoop:

	IN 1
	ANI 1
	JZ NextCharLoop
	IN 0
	
	MOV B,A
	
  ; Do we have the same class as before?
  PUSH H
	MVI L,(ClassLookup&0ffh)-1
	LookupClassLoop:
	INR L
	CMP M
	INR L
	JC LookupClassLoop
	MOV C,M
	POP H
  ; are L and C equal?
  MOV A,L
  XRA C
  ; Z if they are equal, NZ if not
  
  PCHL ; Jump based on previous CharClass pointer 

DigitClass:
  JNZ DigitClassEnd
 
 	PUSH H
  ; A is zero at this point
  
  ; Accumulate the value into D

	; Muliply by 10
	MOV H,D
	MOV L,E
	
	DAD H
	DAD H
	DAD D
	DAD H
	
	; Add in the new digit
	
	MOV D,A
	MOV A,B
	ANI 0fh
	MOV E,A
	DAD D
	
	XCHG
	
	POP H
	
  JMP NextCharLoop

DigitClassEnd:
  ; Write token into program
  ; need to preserve DE, don't care about HL
  
  XTHL
  MVI M,IntegerToken
  INX H
  MOV M,E
  INX H
  DB 36h ; opcode for MVI M eats next byte
Write_Shared_AtSP:
  POP D
Write_Shared:
  MOV M,D
  INX H
  XTHL

NoCharClass:
  MOV L,C
  XRA A ; set Z
  MOV D,A ; reset state information
  MOV E,A

  PCHL
  
QuoteClassExpEnd:
	
	; if C=QuoteClass&0ffh then NZ
	MVI A,QuoteClass&0ffh
	SUB C
	
	; wanted to do this, but no bitwise xor in 
	; ASM80
	;SBI (QuoteClass^QuoteClassExpEnd) & 0ffh
	
	; somehow invert Z
	SBI 1 ; iff it was zero will C be set
	SBB A ; iff it was zero will it
				; now be non-zero
	
QuoteClass:
	; first time through Z is set and A is zero
	; on fall through Z is set unless its the last quote
	MVI L,(QuoteClassExpEnd&0ffh)-1
	
LT0Class:
	INX H
	NOP
	
CompClass:
	NOP
AlphaClass:
	
	XTHL
	MOV M,B
	INX H
	XTHL
	
	DCX D ; increase char count
	
	; if NZ then we will just
	; have written a different class char:
	; good, this ensures no spurious
	; strcmp matches
	
	; now we nerd to decide whether to jump to:
	; FreshStart - if its the last quote in
	;							 a string
	; NLTest		 - if part way through string or 
	;								token
	; TokenClassEnd - if end of token
	
	JZ NLTest
	
	MOV A,L
	RST_CompareJump
	DB QuoteClassExpEnd&0ffh,(FreshStart&0ffh)-1
	
TokenClassEnd:

	XTHL
	DAD D
	
	; it's a var if bits 7,6,5 are 010 and
  ; E=-2

	MOV A,M
	XRI 040h
	MOV D,A
	ANI 0e0h
	XRA E
	
	RST_CompareJump
	DB 0feh,(Write_Shared&0ffh)-1
	
	LXI D,TokenList

LookupToken_Loop:
	LDAX D
	PUSH PSW
	PUSH H
StrCmp:
	INX D
	LDAX D
	XRA M
	; iff match then A is either 00h or 80h
	; (80h if last char)
	INX H
	JZ Strcmp ; match and not last char
	
	; equal to 080h iff match and last char
	XRI 080h
	; equal to Z iff match and last char

	POP H
	
	JZ Write_Shared_AtSP
	
	POP PSW
	
LookupToken:
	LDAX D
	INR A
	INX D
  JM LookupToken_Loop
  JNZ LookupToken
	
  ; didn't find it
	
Error1:
	JMP Error1

; TODO
; alphaclass, compclass and lt0class can
; all be combined, ans call LookupToken
; after every char
; requires changes to lookup routine to
; take account of coming to end of buffer
ClassLookup:
DB 64,AlphaClass&0ffh
DB 58,CompClass&0ffh
DB 48,DigitClass&0ffh
DB 35,LT0Class&0ffh
DB 34,QuoteClass&0ffh
DB 33,LT0Class&0ffh
DB 0,FreshStart&0ffh

GetLineNum:
	; Line number is in DE, look it up in the program and set BC to the line num token
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
	
	DCX B
	DCX B
	DCX B
	; Now we want Z set if DE=(BC), clear
	; otherwise 
	
ATNLN_RetNZ: ; shared code. Returns NZ if we know
						 ; that A is non-zero
	ORA L
	
	RET
	

AdvanceToNextLineNum:
; BC is a pointer to somewhere in the program
; move onto the next line number
; return with Z set if successful
; Z clear if fell off end of program

	LDAX B
	RST_CompareJump
	DB EndProgram&0ffh,(ATNLN_RetNZ&0ffh)-1
	; fell off end of program
	
	CPI LinenumSub&0ffh
	RZ
	
	INX B
	
	RST_CompareJump
	DB IntegerToken,(ATNLN_Int&0ffh)-1
	CPI StringToken
	JNZ AdvanceToNextLineNum
	
ATNLN_String:
	LDAX B
	INX B
	CPI StringToken
	JNZ ATNLN_String
	
	DB 0c2h ; opcode for JNZ eats 2 bytes
ATNLN_Int:
	INX B
	INX B

	JMP AdvanceToNextLineNum

; List statement implementation
ListSubImpl:
	LXI B,PROG_BASE
ListLoop:
	MVI A,' '
	RST_PutChar
	
	LDAX B
	CPI EndProgram&0ffh
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
  CPI LinenumSub&0ffh
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
	CALL OutputString_WithQuote
	RST_PutChar
	RET

List_Var:
  LDAX B
  INX B
  ADI '@'
  RST_PutChar
  RET

GetVarLocation:
; A should contain a var token
; and B points to tbe location after
; the var token
; return with var address in HL
; and B pointing to next char

	; Test that we have a var
	CPI 27
	CNC Error
	
	MVI H,VAR_SPACE/256
	ADD A
	MOV L,A
	
	RNZ
	
	; fall through if it is array var
	
	LDAX B
	INX B
	CPI LeftBraceToken&0ffh
	CNZ Error
	RST_ExpEvaluate
	INX B
	CPI RightBraceToken&0ffh
	CNZ Error
	
	; Now DE contains the array index
	; Add it twice to get the offset
	
	LHLD PROG_PARSE_PTR
	INX H ; up 1 byte to avoid EndProgram marker
	DAD D
	DAD D
	
	RET
	
; To large to fit after TokenList
LetSubImpl:
	LDAX B
	INX B
	
	PUSH PSW
	
	; Test that we have an equals sign
	LDAX B
	CPI EqualSub&0ffh
	CNZ Error
	
	INX B
	
	RST_ExpEvaluate
	
	POP PSW
	
GetVLAndAssignToVar:
	CALL GetVarLocation
	
AssignToVar:
	; Put DE into var (HL)
	
	MOV M,E
	INX H
	MOV M,D
	
	RET
	
; Index to subroutine address must not overlap with other token values
ORG 02e0h

; order in this list must make sure that a
; token A that is a left subatring of another
; token B appears later in the list than B
; e.g. < is after <=

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
	DB EndSub&0ffh
	DB "EN",'D'+128
; Before this are keywords allowed at run-time
	DB ExecuteProgram&0ffh
	DB "RU",'N'+128
	DB ListSub&0ffh
	DB "LIS",'T'+128
	DB NewSub&0ffh
	DB "NE",'W'+128
	DB CommaToken
	DB ','+128
	DB LeftBraceToken&0ffh
	DB '('+128
  DB RightBraceToken&0ffh
	DB ')'+128
	DB EqualSub&0ffh
	DB '='+128
	DB NotEqualSub&0ffh
	DB "<>"+128
	DB GTESub&0ffh
	DB ">",'='+128
	DB LTESub&0ffh
	DB "<",'='+128
	DB LTSub&0ffh
	DB '<'+128
	DB GTSub&0ffh
	DB '>'+128
	DB AddSub&0ffh
	DB '+'+128
	DB SubSub&0ffh
	DB '-'+128
	DB MulSub&0ffh
	DB '*'+128
	DB DivSub&0ffh
	DB '/'+128
	DB 255; 255 can only occur at the end

LineNumSub:
	INX B
	INX B
	RET

PrintSubLoop:
	INX B
PrintSub:
	LDAX B
	RST_CompareJump
	DB StringToken,(PrintStringToken&0ffh)-1

	CALL ExpEvaluateOptional
	
	PUSH B
	CC PrintInteger
	POP B
	
	RNC	; return without printing newline
	
	DB 11h ; Skip over first 2 bytes of call
				 ; instruction to fall through
				 ; OutputString is in page 0 so
				 ; last byte is NOP
PrintStringToken:
	CALL OutputString
	INX B
	
PrintSubEndTest:
	LDAX B
	
	RST_CompareJump
	DB CommaToken,(PrintSubLoop&0ffh)-1
	
	RST_NewLine
	RET

LetSub:
	JMP LetSubImpl
	
GosubSub: ; Depth = 1
	RST_ExpEvaluate
	
	POP H
	PUSH B
	PUSH H
	
	DB 03eh ; opcode for MVI A to eat next byte
GotoSub:
	RST_ExpEvaluate
	CALL GetLineNum
	RZ
	CALL Error
	
ReturnSub:
	
	; TODO can save a byte
	
	POP H	; Get return address first
	POP B ; Get pointer to program loc to return to
	PUSH H
	
	; Expect stack size to be 4 or more
	; any less and we have return without gosub
	LXI H,-(STACK_INIT-4)-1
	DAD SP
	CC Error
	
	RET

IfSub:
	RST_ExpEvaluate
	MOV A,E
	ORA D
	RNZ

	; If DE zero then fall through to next line
	JMP AdvanceToNextLineNum

InputSub:
	
	LXI H,INPUT_BUFFER
	MVI M,0
	PUSH H
	CALL GetLine
	POP H 
	
	MOV A,M
	CPI IntegerToken
	CNZ Error ; TODO not user friendly
	
	INX H
	MOV E,M
	INX H
	MOV D,M
	
	LDAX B
	
	INX B
	
	;JMP GetVLAndAssignToVar
	; use the fact that this is in page
	; 2 to save a byte by having EndProgram
	; as the last byte of this instruction
	; which is the opcode for STAX B
	; and harmless in this comtext

	DB 0c3h ; opcode for JMP
	DB GetVLAndAssignToVar&0ffh
EndProgram:
	DB GetVLAndAssignToVar/256
EndSub:
	JMP Ready

ListSub:
  JMP ListSubImpl

NewSub:
	RST 0
	
ExecuteProgram:
	
	; Point BC to first line
	; Skip over the line number
	LXI B,PROG_BASE+3

ExecuteProgramLoop:
	LDAX B

	; Check that it is a token less than
	; ListSub
	CPI (ListSub)&0ffh
	CNC Error
	
ExecuteDirect:

	; Check that it is a token between
	; LinenumSub and ExecuteProgram
	CPI (ExecuteProgram+1)&0ffh
	CNC Error

	CPI LineNumSub&0ffh
	CC Error
	
	INX B
	
	; Put return address onto stack
	LXI H,ExecuteProgramLoop

; ( ) , tokens must have values between keywords
; and operators
LeftBraceToken:
	PUSH H
	
RightBraceToken:
	; Put pointer to call address into HL
	MOV L,A
	; ExecuteProgramLoop must be on the same page
	; page as PrintSub so that we don't have to
	; update H
	
CommaToken:
	; Jump to it
	PCHL
	
; Token values >= this are all operators
Operators:

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
	
