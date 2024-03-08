; Will Stevens
; 25th Feb 2023
; 1K 8080 BASIC
;
; Post-assembly checklist
;
; 1. LineNumSub is >= address 223h
; 2. DivSub is at address 2xxh (i.e. <= 2ffh)
; 3. DivJzError is at address 3xxh
; 4. In LineStartsWithInt, the jump to
;    DeleteProgramLine is on the same
;    page as DeleteProgramLine
; 5. Program does not exceed 1k
; 6. In ClassLookup, check that QuoteClass
;    has LSB different from othet class subs
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
; 2023-05-26 Free space: About 84 bytes
;			various bug fixes
;			PRINT allows comma at end to suppress
;			newline.
;			Show > prompt symbol when ready.
; 2023-05-27 Free space: About 17 bytes
;			Added FOR NEXT (no STEP yet)
;			Need more space 
; 2023-05-27 Free space: About 36 bytes
;			Made a few small byte savings, and put
;			token subs onto page 2 so that last one
;			can flow onto page 3, freeing some space
;			in page 2 to avoid having to jump out.
;			One TODO to action
; 2023-05-28 Free space: About 42 bytes
; 2023-05-29 Free space: About 53 bytes
;			Made some changes to * and / which I hope
;			are improvements (efficienxy + code size)
;			but testing needed to confirm this.
; 2023-06-02 Free space: About 12 bytes
;			Added support for STEP to FOR loops
;			Noted where Z flag is in known state 
;			in JMP instructions, because there is
;			potential space saving by having
;			2-byte in-page JMP, JNZ or JZ, 
;			code shared with RST_CompareJump
; 2023-06-03 Free space: About 10 bytes
;			STEP works in +ve direction only
;			Fixing will require more space
;			Would also like to add ABS, RND, USR
;			But probably need about 60 bytes for that
; 2023-06-03 Free space: About 19 bytes
;			Added in-page JZ to free up space
;			Likely to have introduced errors
; 2023-06-04 Free space: About 25 bytes
;			Shortened PrintSub
; 2023-06-04 Free space: about -21 bytes
;			Implementing ABS and USR and skeleton 
;		  of RND makes it 21 bytes over budget.
;			So it seems reasonable to think that
;			space can be made for these.
; 2023-06-05 Free space: about -15 bytes
; 2023-06-19 Free space: about 6 bytes
;			Replaced memory rotate with triple reversal
;			algorithm. Back below size limit, but need
;		  to rearrange things to realize this.
; 2023-06-22 Free space: about 10 bytes
;			All free space is in the RST area, which I
;     am reluctant to use because I expect that
;			when I try to target actual hardware I will
;			need to extend PutChar, and maybe have
;			some initialization code for e.g. UART.
;			So discounting this I am 2 bytes over
;			budget, and haven't implemented RND
;			function yet
; 2023-06-23 Free space : 18 bytes
;			Saved space with more sharing between
; 		LET and INPUT
;			Ready to do a lot of testing
; 2023-06-28 Free space : 20 bytes
; 2023-06-28 Free space : 19 bytes
;			Fixed enough bugs that lunar lander works
;			Function calls don't work yet
; 2023-07-01 Free space : 24 bytes
;			ABS function works
;			RND function currently does nothing
;			need to make implementation of RND
;			that fits in 17 bytes
; 2023-07-04 Free space : 20 bytes
;			Implemented simple lookup-based RND
;			replaced newline RST with LDAX B, INX B
;			saved a few bytes in LIST
; 2023-07-05 Free space : 11 bytes
;			Implemented XORSHIFT RND function
; 2023-07-08 Free space: 10 bytes
; 2023-07-12 Free space: 15 bytes
;			Fixed forgotten issue where STEP in FOR
;			loop didn't work if negative
; 2023-07-13 Free space: 13 bytes
;			Extended variable range up to 32
;			So that user has 31 variables and array
;			var 30 can be used to work out
;			remaining memory
;			var 31 is RNG seed
; 2023-07-15 Noticed bug where -32768 isn't
;     displayed
; 2023-07-15 When playing REVERSE, saw corrupted
;			array, which implies that bug where stack 
;			continually growing
; 2023-07-16 Above two issues fixed. Former
;     required change to PrintInteger. Latter
;			was due to a GOTO from within FOR loop,
;			in REVERSE and not necessarily a problem
;			with this interpreter
; 2024-01-01 Fixed bug where parse error wasn't 
;     displayed as ? during LIST
; 2024-01-28 Fixed bug where @ was displayed as M 
;     during LIST
; 2024-02-07 Working on corrections to comparison
;     operators. Not in working state. Made I/O 
;     compatible with Dick Whipple's Front Panel 
;     8080 simulator
; 2024-02-08 May have fixed comparison operator
;     problem. Need to save 2 bytes to be able to
;     test it
; 2024-02-08 Reclaimed some space so that 3FEh is
;     the last byte uses. Free space 5 bytes.
; 2024-02-18 Worked towards reclaiming 4 bytes in
;     the tokenizer. Good chance of being 
;     incorrect, will require debugging.
; 2024-02-20 Debugged above changes and
;     they seem okau. Free space 9 bytes
; 2024-02-22 Rearranged RSTs and added CPI to end
;     of RST_LDAXB_INXB_CPI to save memory.
;     Free space still 9 bytes but now
;     7 of those are at the end of 1K, so are
;     easy to make use of.
;     Need to check movement and alignment
;     of subroutines. 
;     It would be useful to have a checklist of
;     all dependencies that need to be checked
;     when there are large movements in memory.
;
;     Next things to do:
;     - unterminated string check
;     - forbidding excess chars in tokens
;     - correct operator precedence for * /
;     - error on divide by zero
;     not sure whether all 4 can be done in
;     only 7 bytes
; 2024-02-25 Added unterminated string check and
;     didvide by zero error. 2 bytes over budget.
; 2024-02-25 Realised that removing reatriction
;     that RUN, LIST and NEW only allowed in
;     direct mode would will probably save a
;     sufficient number of bytes to finish all
;     outstanding work
; 2024-02-28 Sveral changes related to issues
;     listed above, Divide by zero and 
;     unterminated string now generate error
;     messages. 2 bytes free which should be 
;     enough to make * and / equal precedence, 
;     but will test everything else first. Issue 
;     about tokens with excess chars not being 
;     detected as errors will remain unfixed in 
;     first release.
; 2024-03-01 Found bug where recent changes
;     caused DeleteProgramLine to move page.
;     In the course of fixing it, may have saved
;     5 bytes. Need to test that fix is correct.
; 2024-03-01 Fixed a bug introduced on 28 Feb 
;     where ExecuteDirect was called without
;     setting B. Wrote 'game of life' example
;     program. When printing newline, added CR
;     before after discovering that some
;     terminals need this. Need to free up
;     1 byte to fix operator precedence issue
; 2024-03-02 This version correcly runs
;     lander.bas, reverse.bas, life.bas, 
;     operatortests.bas, operatortests2.bas, 
;     looptests.bas
; 2024-03-02 Freed up 1 byte by removing 
;     redundant STC
; 2024-03-03 Added code to make * same
;     precedence as / (needs testing).
;     Changed RNG from XORSHIFT to LCG.
;     This saved 3 bytes.
;     Need to experiment with LCG constant
;     for best RNG performance.
;     Behaviour of RNG function changed so
;     that max valid input parameter is 256,
;     because low order bits of RNG have
;     low period. Only high ordet bits of
;     RNG are used for return value.
; 2024-03-04 Altered EndProgram address.
;     Temporarily tried making RAM start at 1000h
;     to check that this doesn't cause problems
; 2024-03-06 Changed IO to support Stefan Tramms
;     8080 emulator.
; 2024-03-08 Branches code to make Altaid 8800
;     version.
; For development purposes assume we have
; 1K ROM from 0000h-03FFh containing BASIC
; 1K RAM from 1000h-13FFh

RAM_BASE equ 1000h
RAM_TOP equ 1400h ; 1 more than top byte of RAM

; Token values
; 0-31 are variables (0 = @)

; IntegerToken must be one more than last var
IntegerToken equ 32 ; followed by 16-bit integer
QuestionMarkToken equ 33 ; indicates syntax error
StringToken equ 34 ; followed by string, followed by end quote

; Callable tokens are low byte of subroutine to call

; Errors are displayed as Ex where x is an error
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

INPUT_BUFFER equ RAM_TOP-8
STACK_INIT equ RAM_TOP-8

; this must be on a 256 byte boundary
VAR_SPACE equ RAM_BASE
	; 30 words, first of which is not
	; accessible to user, so can be
	; used for PROG_PTR
PROG_PTR equ RAM_BASE
	; 2 words accessible to user as variables
	; 30 and 31 (^ and _)
PROG_PARSE_PTR equ RAM_BASE+60
RNG_SEED equ RAM_BASE+62

PROG_BASE equ RAM_BASE+64

ORG 00h
  ; I would like this to be:
  ; LXI H,PROG_BASE
	; SHLD PROG_PTR
	; JMP Ready
	;
	; But this doesn't fit in 8 bytes.
	; Instead we find a place in the program
	; that has LXI B,PROG_BASE, and set SP to that 
	; address, the POP H from the stack and 
	; store it in PROG_PTR, then INX SP means
	; that when we fall through to PutChar,
	; the RET will jump to Ready
	; 
	; (It means that on reset and NEW a char
	; will be output that depends on the value of
	; A at the time, but worth it to save several
	; bytes)

	LXI SP,ExecuteProgram+2
	POP H
	SHLD PROG_PTR
	INX SP

.macro RST_PutChar
RST 1
.endm
ORG 08h

; PutChar is called frequently
; PutChar must return with Z set

PutChar:
	; port 1 is for char I/O
	OUT 1
PutCharWaitLoop: ; address 000ah
  ; TODO change these fee instructions
  ; if targetting hardware
  
  XRA A 
  RET

	;IN 1
	ANI 040h
	RZ
	db 0c3h ; opcode for JMP
	        ; the following two bytes are 
	        ; 0ah and 00h, so this jumps to
	        ; PutCharWaitLoop
	
.macro RST_LDAXB_INXB_CPI
RST 2
.endm
ORG 10h
	LDAX B ; opcode 0ah
	NOP    ; opcode 00h
	INX B
	XTHL
	CMP M
	INX H
	XTHL
	RET
	
.macro RST_CompareJump
RST 3
.endm
ORG 18h
; byte after RST is compared with A
; if equal then jump to address on same page.
;
; only use where performance is not
; important (parsing, printing)

	XTHL
	CMP M
	INX H
	JMP CompareJump_Entry
	
	; 2 bytes free

.macro RST_JZPage
RST 4
.endm
ORG 20h
	XTHL
CompareJump_Entry:
	JNZ JZPage_Skip
	MOV L,M
JZPage_Skip:
	INX H
ExpApplyOp: ; shared code
	XTHL
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
	
	; decrement and invert so that we end
	; up with D in A - sometimes handy
	DCX D
	MOV A,E
	CMA
	MOV E,A
	MOV A,D
	CMA
	MOV D,A
	RET

ORG 38h
  ; Leave space for JMP to ISR
  db 0,0,0

ExpEvaluate:

; BC points to program
; DE contains value
; Stack is used for both operands and
; operators

; This puts a marker on the stack to
; detect when there are operators on the
; stack - operators all have 2 as the hi byte
; but this call puts hi byte 0 on the stack

	CALL ExpEvaluateNum
	CNC Error
	RET

; ExpEvaluateNum must always be called
; from page 0
ExpEvaluateNum:
	; Expecting ( var integer or - sign
	; or function call
	RST_LDAXB_INXB_CPI
	DB LeftBraceToken&0ffh
	RST_JZPage
	DB (ExpLeftBrace&0ffh)-1
	RST_CompareJump
	DB SubSub&0xff,(ExpNegate&0ffh)-1
	
	; last function
	CPI (RndSub+1)&0ffh
	RNC ; if its greater than this, its an error
	; first function
	CPI AbsSub&0ffh
	JNC FunctionCall ; between RndSub and AbsSub
	
	; can't use RST_CompareJump below
	; because it doesn't preserve Carry after
	; comparison.
	; TODO - this assumption isn't correct - INX H
	; doesnt affect flags
	
	CPI IntegerToken
	JC ExpVar
	
	; Integer token is one more than last var
	; token so if carry is set then it is a var
	
	RNZ : return with carry clear if error

	; Fall through to ExpInteger
ExpInteger:
	MOV H,B
	MOV L,C
	INX B
	INX B
	
	; fall through with carry clear
ExpVar:
	; carry set if jumped to here
	
	CC GetVarLocation
ExpVarGetValue:
	MOV E,M
	INX H
	MOV D,M

ExpEvaluateOp:
	;Expecting operator or right bracket or
	;end of expression
	
	;Are there operators on the stack?
	POP H
	
	; H will be 0 if no operators on
	; stack (i.e. high byte of return address)
	
	MOV A,H
	RST_CompareJump
	DB 0,(SkipExpApplyOp&0ffh)-1
	
	; if L is equal to MulSub then apply it.
	; this gives * same precedence as /
	MOV A,L
	RST_CompareJump
	DB (MulSub&0ffh),(ExpApplyOp&0ffh)-1
	
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
	
	JC ExpApplyOp ; apply the operator
								; that was on the stack
	
SkipExpApplyOp:
	PUSH H		; put operator that was on stack
						; back onto stack
	
	LDAX B
	
	CPI Operators&0ffh
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


PrintSubString:
	CALL OutputString ; carry is clear on return
PrintSubInteger:  ; carry is set on jump to here
	CC PrintInteger ; carry is clear on return
	
	DB 11h ; LXI D eats 2 bytes
PrintSubLoop:
	STC
	INX B
	POP D ; discard, since we are about to push again
	
PrintSubImpl:
	; First time called, carry is clear
	; Subsequent times carry is clear unless
	; last token was a comma
	PUSH PSW
	
	LDAX B
	
	RST_CompareJump
	DB StringToken,(PrintSubString&0ffh)-1
	RST_CompareJump
	DB CommaToken,(PrintSubLoop&0ffh)-1

	; must be called from page 0
	CALL ExpEvaluateNum 
	JC PrintSubInteger
	DCX B
	
	; Finished, we want to print a newline unless
	; last one was a comma
	POP PSW
	RC ; return without newline if it was comma
CRLF:
	MVI A,13
	RST_PutChar
	MVI A,10
	RST_PutChar
	RET

GetVarLocationBVar:
  RST_LDAXB_INXB_CPI
  
	; Test that we have a var
	db 32
	CNC Error

GetVarLocation:
; A should contain a var token
; and B points to tbe location after
; the var token
; return with var address in HL
; and B pointing to next char
; A will never be 255 on return

	MVI H,VAR_SPACE/256
	ADD A
	MOV L,A
	
	RNZ
	
	; fall through if it is array var
	
	CALL ExpBracketedB
	
	; Now DE contains the array index
	; Add it twice to get the offset
	
	LHLD PROG_PARSE_PTR
	INX H ; up 1 byte to avoid EndProgram marker
	DAD D
	DAD D

OutputStringRet: ; shared code, nearest RET
	RET

; This 9 byte routine can be moved anywhere to
; fill holes
OutputString:
;Pointer in B points to string token marker
	INX B
OutputStringLoop:
	RST_LDAXB_INXB_CPI
	DB StringToken
	RST_JZPage
	DB (OutputStringRet&0ffh)-1
OutputString_WithQuote:
	RST_PutChar
	RST_JZPage
	DB (OutputStringLoop&0ffh)-1

ExpLeftBrace:
	DCX B
FunctionCall:
	; push return address
	LXI D,ExpEvaluateOp
	PUSH D
	; A contains the address to call on page 2
	; push function address
	MOV L,A
	MVI H,PrintSub/256
	PUSH H
	
	; fall through

  ; This must be before Error so that it
  ; can fall through
ExpBracketedB:
  RST_LDAXB_INXB_CPI
	DB LeftBraceToken&0ffh
	CNZ Error

	CALL ExpEvaluate
	
	RST_LDAXB_INXB_CPI
	DB RightBraceToken&0ffh
	RZ
	
	; fall through

;Display error code and go back to line entry
Error:
	CALL CRLF
	MVI A,'E'
	RST_PutChar
	POP D
	CALL PrintInteger
	
	; fall through
	
Ready:
	; Set stack pointer
	; Do this every time to guard against
	; GOSUB with no RETURN errors
	
	LXI SP,STACK_INIT
	
	CALL CRLF
	
	LHLD PROG_PTR
	PUSH H ; push it because we need it after 
				 ; GetLine

	CALL GetLine
	
	MVI M,EndProgram&0ffh
	
	SHLD PROG_PARSE_PTR
	POP H
	
	PUSH H
	POP B
	
	MOV A,M
	; Regardless of which branch taken
	; we need this marker here.
	; This overwrites the token to execute,
	; but we've already got that in A
	MVI M,EndProgram&0ffh
	
	CPI IntegerToken
	JNZ ExecuteDirect
	
LineStartsWithInt:
	; Get the line number into DE
	INX H
	MOV E,M
	INX H
	MOV D,M
	INX H
	
	; Is it an integer all by itself? 
	; If so then delete the line
	
	; call GetLineNum to find either the line, or
	; pointer to next location in program after it
	
	MOV H,M ; preserve M (GetLineNum doesn't touch H)
	CALL GetLineNum
	MOV A,H
	LHLD PROG_PTR
	PUSH PSW 
	
	RST_CompareJump
	DB EndProgram&0ffh
	DB (DeleteProgramLine&0ffh)-1
	
	POP PSW
	; if GetLineNum returns a match then this is
	; an error, user must delete line first
	CZ Error
	
	; do a memory rotate with
	; first = GetLine/ATNLN address
	; middle = PROG_PTR
	; last = PROG_PARSE_PTR
	
	MVI M,LineNumSub&0ffh; undo what we did earlier
	XCHG
	LHLD PROG_PARSE_PTR
	
	PUSH H ; last
	PUSH B ; first
	
	PUSH D ; middle
	
	XRA A
	
	RST_JZPage
	DB (Entry&0ffh)-1

DeleteProgramLine:
; 25 bytes
	POP PSW
	
	JNZ Ready		; if line not found, do nothing

	PUSH H
	PUSH B ; first
	PUSH H ; last
	
	DAD B ; HL=PROG_PTR+first
	
	INX B
	CALL ATNLN_Int ; Z is set when this is called
	
	;set HL to what we want PROG_PTR to be
	MOV D,B
	MOV E,C
	RST_NegateDE
	
	DAD D ; HL=PROG_PTR+first-middle
	
	; because DAD D above always causes HL
	; to decrease, it must set carry
	; so STC below is not needed

	;STC ; skip first reverse in memory rotate
			; because we don't care about the
			; line being deleted
	
Entry:
	; carry is clear if coming from insert
	
	PUSH B ; middle (or first)

	SHLD PROG_PTR
	
MemoryRotate:
; 27 bytes
; stack must contain (from top down)
; first, middle, first, last
; DE = middle
; HL = Last

  CNC Reverse
  CALL ReverseDH
  
  LXI B,Ready
  PUSH B

ReverseDH:
	POP H
  POP D
  XTHL
  
Reverse:
; HL = last (i.e 1 after the last byte to swap)
; DE = first

ReverseLoop:
	RST_CompareHLDE
	RZ
	DCX H
	RST_CompareHLDE
	RZ
	
	MOV B,M
	LDAX D
	MOV M,A
	MOV A,B
	STAX D
	INX D
	
	JMP ReverseLoop

POPHAssignToVar_Prefix:

	PUSH H
	
	CALL GetLine

  POP B
  CALL ExpEvaluate
  POP B
  
	; fall through
POPHAssignToVar:

	POP H
	
	; Put DE into var (HL)
	
	MOV M,E
	INX H
	MOV M,D
	
	RET 

ListLoop:
	MVI A,' '
	RST_PutChar
	
	RST_LDAXB_INXB_CPI
	DB EndProgram&0ffh
	RZ
	
  LXI H,ListLoop	; so that we can loop using RET
  PUSH H
  
  ; H is already set to the correct page
  MVI L,(TokenList-1)&0ffh

	; These need to be on same page
	; currently on page 3
	RST_CompareJump
	DB StringToken,(List_String&0ffh)-1
	RST_CompareJump
	DB LinenumSub&0ffh,(List_Linenum&0ffh)-1
  RST_CompareJump
	DB IntegerToken,(List_Integer&0ffh)-1
  
  JC List_Var
  
  ; No need to check for end of TokenList
  ; impossible not to be a token value in A
  
List_Token_Loop:
  MOV D,M
  INR D
  INX H
  JP List_Token_Loop
  
List_Token:
	; on entry, A contains the token
  ; so must not use A during this loop
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
  
  RET
	
List_LineNum:
	CALL CRLF
 
List_Integer:
  LDAX B
  INX B
	MOV E,A
	LDAX B
	INX B
	MOV D,A
 	; fall through to PrintInteger
 	
;Output the value in DE
PrintInteger:
	XRA A
	PUSH PSW			; end marker is Z flag
	
	ORA D			; S is set if -ve
	RST_NegateDE
	
	JP PrintIntegerLoop
	MVI A,'-'
	RST_Putchar
	RST_NegateDE
	
PrintIntegerLoop:
	; need HL to be -ve here, so that it can
	; handle -32768
	
	XCHG
	LXI D,10
	
	CALL DivideHL
	; HL contains remainder after / 10
	; DE contains the quotient

	MVI A,'0'
	SUB L
	PUSH PSW ; push onto stack
	
	; if DE is zero we are done
	MOV A,D
	ORA E
	JNZ PrintIntegerLoop
	
PrintIntegerLoop2:
	POP PSW
	RZ
	RST_PutChar
	RST_JZPage
	db (PrintIntegerLoop2&0ffh)-1

List_String:
	CALL OutputString_WithQuote

	DB 011h ; LXI D skips 2 bytes
List_Var:
  ADI '@'
  RST_PutChar
  RET ; byte before TokenList must have high bit set
  

; Index to subroutine address must not overlap with other tokens
; Currently TokenList starts toward the end
; of page 1, and DivSub begins towards the end
; of page 2 and the subroutine extends into page 3

; order in this list must make sure that a
; token A that is a left substring of another
; token B appears later in the list than B
; e.g. < is after <=
	
TokenList:
	DB QuestionMarkToken&0ffh
	DB '?'+128
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
	DB InputSub&0ffh
	DB "INPU",'T'+128
	DB ForSub&0ffh
	DB "FO",'R'+128
	DB NextSub&0ffh
	DB "NEX",'T'+128
  DB IfSub&0ffh
	DB "I",'F'+128
	DB EndSub&0ffh
	DB "EN",'D'+128
	
; Before this are keywords allowed at run-time
  DB ExecuteProgram&0ffh
	DB "RU",'N'+128
	DB ListSub&0ffh
	DB "LIS",'T'+128
	DB NewSub&0ffh
	DB "NE",'W'+128
	
	
	
; before operators are non-statement
; non-operator tokens

	DB AbsSub&0ffh
	DB "AB",'S'+128
	DB UsrSub&0ffh
	DB "US",'R'+128
	DB RndSub&0ffh
	DB "RN",'D'+128
	
	DB ToToken&0ffh
	DB "T",'O'+128
	DB StepToken&0ffh
	DB "STE",'P'+128
	DB CommaToken
	DB ','+128
	DB LeftBraceToken&0ffh
	DB '('+128
  DB RightBraceToken&0ffh
	DB ')'+128
	DB EqualSub&0ffh
	DB '='+128
	DB NotEqualSub&0ffh
	DB "<",'>'+128
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
	DB 255 ; 255 can only occur at the end
	
LineNumSub:
	INX B
	INX B
	RET
	
PrintSub:
	JMP PrintSubImpl
	
GosubSub:
	CALL ExpEvaluate
	POP H
	
	PUSH B
	PUSH H
	
	DB 03eh ; opcode for MVI A to eat next byte
GotoSub:
	CALL ExpEvaluate
	CALL GetLineNum
	RZ
	CALL Error
	; TODO in place of call error, is there
	; a two or three byte inst that will
	; cause C to be set when DAD SP is called below?
	
ReturnSub:
	; Expect stack size to be 6 or more
	; any less and we have return without gosub
	LXI H,-(STACK_INIT-6)-1
	DAD SP
	CC Error
	
	POP H	; Get return address first
	POP B ; Get pointer to program loc to return to
	PCHL ; instead of RET

InputSub:

	CALL GetVarLocationBVar
	PUSH H
  
  LXI H,INPUT_BUFFER
  PUSH B
	
	JMP POPHAssignToVar_Prefix

ForSub:
	LXI H,ForSubImpl
	PUSH H
	; fall through to LetSub
	; First part is just like let statement
LetSub:
	CALL GetVarLocationBVar
	PUSH H
	
	; Test that we have an equals sign
	RST_LDAXB_INXB_CPI
	
	DB EqualSub&0ffh
	CNZ Error
	
	CALL ExpEvaluate
	
	JMP POPHAssignToVar
	
NextSub:
	POP H ; discard return address
	; stack contains VL+1,S,-T,LS,EPL
	POP H	; get VL+1
	MOV D,M
	DCX H
	MOV E,M
	
	XTHL		; step is in HL, VL is in (SP)
	XCHG		; step is in DE, var value in HL
	DAD D		; add step onto var
	XCHG		; result is in DE, step is in HL
	XTHL		; step is in (SP), VL is in HL
	
	MOV M,E ; put back into VL
	INX H		; H = VL+1
	MOV M,D	
	
	POP PSW ; get step so that hi bit of A has
					; sign of step
	POP H		; get -T
	
	DAD D 	; HL now has LV-T
	
	XRA H		; xor sign of step with
					; sign of result
	
					; if result of xor above is 1
					; then keep looping, or if HL
					; is zero then keep looping
					
	POP D ; this is LoopStart
	
	JM NextSubLoop
	
	MOV A,H
	ORA L
	RNZ
	
NextSubLoop:
	
	MOV B,D
	MOV C,E
	LXI H,-10
	DAD SP
	SPHL
	
	RET
	
IfSub:
	CALL ExpEvaluate
	MOV A,D
	ORA E
	RNZ

	; If DE zero then fall through to next line
	JMP AdvanceToNextLineNum 

ExecuteProgram:
	STC ; skip over JNC Ready in a minute
	
EndProgram: ; executes the JNC Ready
					  ; don't care what happens to BC
					  ; if we are jumping to Ready
					  
	; Point BC to first line
	; Don't skip over the line number
	; because we need the constant PROG_BASE
	; at this location in memory
	LXI B,PROG_BASE

EndSub:
	JNC Ready

ExecuteProgramLoop:
	LDAX B
	
ExecuteDirect:
	
	SUI LineNumSub&0ffh
	
	; Check that it is a token between
	; LinenumSub and ListSub
	CPI (ListSub-LineNumSub+1)&0ffh
	CNC Error
	
	INX B

	ADI LineNumSub&0ffh
	
	; Carry is clear now
	
	; Put return address onto stack
	LXI H,ExecuteProgramLoop
	PUSH H
	
	; Put pointer to call address into HL
	MOV L,A
	; ExecuteProgramLoop must be on the same page
	; page as PrintSub so that we don't have to
	; update H

	; Jump to it
	; Carry is clear when we do this
	PCHL

NewSub:
	RST 0
	
ListSub:
	LXI B,PROG_BASE
  JMP ListLoop
	
; ( ) , TO STEP tokens must have values between 
; statements and functions

ToToken equ ListSub+1
StepToken equ ListSub+2
RightBraceToken equ ListSub+3
CommaToken equ ListSub+4

AbsSub:
	; A = right brace token, which has high bit
	; set, so no need to negate DE if XRA with D
	; still leaves high bit set
	XRA D
	RM
	RST_NegateDE
	
	; shared code. okay for this to go here
	; because in ExpEvaluateNum, test for
	; left brace is before test for token
	; between first and last function
LeftBraceToken:
	RET
	
UsrSub:
	XCHG
	PCHL


RndSub:
; LCG 
; don't use low byte in return value.
; Multiplier 47989 is mentioned here:
; https://groups.google.com/g/prng/c/evszGs76o1w?pli=1

	PUSH D
	LHLD RNG_SEED
	LXI D,47989
	CALL MulSub ; A is zero after this
	XCHG
	INX H
	SHLD RNG_SEED
	; Use only the high byte to get a value
	; between 0 and 255
	MOV L,H
	MOV H,A
	POP D
	
	CALL DivideHL
  XCHG
  RET

; XORSHIFT taken from here
; https://wikiti.brandonw.net/index.php?title=Z80_Routines:Math:Random
	;LHLD RNG_SEED
	;MOV A,H
  ;RAR
  ;MOV A,L
  ;RAR
  ;XRA H
  ;MOV H,A
  ;MOV A,L
  ;RAR
  ;MOV A,H
  ;RAR
  ;XRA L
  ;MOV L,A
  ;XRA H ; clears carry
  ;MOV H,A
  ;SHLD RNG_SEED
  
  ; carry is clear at this point
  ;RAR
  ;MOV H,A
  
  ; above 2 bytes give us a value between
  ; 0 and 32767
  
  ;CALL DivideHL
  ;XCHG
  ;RET


; Token values >= this are all operators
Operators:
	
LTESub:
	; Swap operands and fall through
	XCHG
GTESub:
	RST_CompareHLDE
	RST_JZPage
	DB (BinReturn&0ffh)-1
GTSub:
	; Swap operands and fall through
	XCHG
LTSub:
	MOV A,L
	SUB E
	MOV A,H
	SBB D
	RAR
	XRA H
	XRA D
	RAL
  
  DB 3eh ; MVI A opcode to swallow next byte
EqualSub:
	RST_CompareHLDE ; returns Z iff HL=DE
BinReturn:
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
; 20 bytes
; multiple HL and DE into DE, preserving B
	PUSH B
	MOV B,H
	MOV C,L

Multiply:
;multiply BC and DE into DE
	MVI A,16
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
; 31 bytes
;Divide HL by DE
;Remainder in HL 
;Result in DE

DivideHL:
;Divide HL by DE
	; Make HL and DE different signs
  MOV A,H
  CALL AbsSub
	PUSH PSW
	
;Divide HL by DE
;Assuming that HL and DE are different signs

	PUSH B
	LXI B,0ffffh
	
; Do the test for zero here because we want the
; CZ to be on page 3
; This means that divide by zero and unterminated
; string both have tbe same error code, but kt
; will be obvious to the programmer which is
; intended
	MOV A,D
  ORA E
DivJZError:
  CZ Error
	
DivLoop:
	INX B
	DAD D
	RAR   ; look for mismatch between carry and
				; bit 7 of D to detect overflow/underflow
	XRA D
	JP DivLoop

	; if HL is zero then it must have been a negative number originally, and the remainder is zero, so don't make any change to HL, but increment quotient by 1
	
	MOV A,H
	ORA L
	RST_JZPage ; assume it is on same page
						 ; because DivSub will
						 ; be right at end of page 2
	DB (DivNoRestore&0ffh)-1
	
 	RST_NegateDE
 	DAD D
	DCX B
	
DivNoRestore:
	INX B
	MOV D,B
	MOV E,C
	
	POP B
	
	POP PSW
	RP
	RST_NegateDE
	
	RET
	
; GetLine sits entirely in page 3
; good - it uses RST_CompareJump in two
; places, so be careful if moving it
; Also it assumes ClassLookup on same page
; as NoCharClass

NLTestTrue:
	; A contains 13 at this point
	; we want to ooutput line feed (10)
	; because H is 3, we can subtract this from A
	SUB H
	RST_PutChar
	
	; error if we are in the middle
	; of a string
	MOV A,L
  RST_CompareJump
	DB QuoteClassExpEnd&0ffh
	DB (DivJZError-1)&0ffh
	
	POP H
	
	RET

GetLine:
	; HL points where we want the line to be
	; parsed to.
	; On return HL points to byte adter what we've 
	; got.
	
	MVI A,'>'
	RST_PutChar
	
	PUSH H
	
	; A is zero at this point
	; (needs to be <>10 on fall to NLTest)

FreshStart:

  LXI H,NoCharClass
	
NLTest:
  MOV A,B
	; check for newline
	RST_CompareJump
	DB 13,(NLTestTrue&0ffh)-1
	
NextCharLoop:
	; This code is compatable with Stefan Tramm's
	; 8080 emulator
	IN 0
	ANA A
	RST_JZPage
	db (NextCharLoop&0ffh)-1
	IN 1
	MOV B,A
	OUT 1 ; echo
	
  ; Do we have the same class as before?
  PUSH H
	MVI L,(ClassLookup&0ffh)-1
	; Test for quote first
	; This doesn't save spave, but takes 3 bytes
	; away from class lookip and puts them here
	; so can be used to change odd/even of
	; ...Class subroutines
	RST_CompareJump
	DB 34,(LC_QuoteTestTrue-1)&0ffh
LookupClassLoop:
	INR L
	CMP M
	INR L
	JC LookupClassLoop
LC_QuoteTestTrue:
	MOV C,M
	POP H
	
  ; are L and C equal?
  MOV A,L
  XRA C
  ; Z if they are equal, NZ if not
  
  PCHL ; Jump based on previous CharClass pointer 

DigitClass:
	RST_JZPage
  DB (DigitClassNotEnd&0ffh)-1
  
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
Write_Shared_Written:
  INX H
  XTHL

NoCharClass:
  MOV L,C
  XRA A ; set Z
  MOV D,A ; reset state information
  MOV E,A

  PCHL
  
DigitClassNotEnd:
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
  
QuoteClassExpEnd:
  
  ; A is equal to:
	; char class (C) XOR QuoteCharClassExpEnd
	
	; so long as QuoteCharClass is the only class
	; with an odd address or the only one
	; with an even address then A will only
	; have LSB=1 if current char class
	; is QuoteCharClass - i.e. end of string
	
	db 0e6h ; opcode for ANI eats next byte
	        ; (which is 2dh lsbits are 01)
	
QuoteClass:
  
  DCR L ; set to QuoteClassExpEnd
  
  ; first time through A is zero 
	; on fall A is even unless C is QuoteClass
	
	ANA H ; H is 3 
	
	; Now Z is set if this was first Quote, or if
	; we are in a string and haven't reached 
	; last quote
	
	; carry is clear here
	db 0dah ; opcode for JC eats next 2 bytes
LT0Class:
	INX H; next char should always count as 
	      ; different class
	NOP

CompClass:
	NOP
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
	; strcmp matches from leftover
	; buffer contents
	
	; now we need to decide whether to jump to:
	; FreshStart - if its the last quote in
	;							 a string
	; NLTest		 - if part way through string or 
	;								token
	; TokenClassEnd - if end of token
	
	RST_JZPage
	DB (NLTest&0ffh)-1
	
  MOV A,L
	RST_CompareJump
	DB QuoteClassExpEnd&0ffh,(FreshStart&0ffh)-1
	
TokenClassEnd:

	; Make H point to the start of the token
	; to be looked up
	XTHL
	DAD D
	
	; it's a var if bits 7,6,5 are 010 and
  ; E=-2
  ; TODO These aren't the only conditions that 
  ; could lead to the test below passing - 
  ; e.g. if 7,6,5=001 and E=10011110.
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
	RST_JZPage
	DB (Strcmp&0ffh)-1 ; match and not last char
	
	; equal to 080h iff match and last char
	XRI 080h
	; equal to Z iff match and last char

	POP H
	
	RST_JZPage
	DB (Write_Shared_AtSP&0ffh)-1
	
	POP PSW
	
LookupToken:
	LDAX D
	INR A
	INX D
  JM LookupToken_Loop
  JNZ LookupToken
	
  ; didn't find it
  
  ; if (HL)>=64 and (HL+1)<64 then its a var
  ; could do the var test here
  ; if it can be done in few bytes
	
	MVI M,QuestionMarkToken&0ffh
	RST_JZPage
	DB (Write_Shared_Written&0ffh)-1

DB QuoteClass&0ffh
ClassLookup:
DB 64,AlphaClass&0ffh
DB 58,CompClass&0ffh
DB 48,DigitClass&0ffh
DB 33,LT0Class&0ffh
DB 0,FreshStart&0ffh

GetLineNum:
	; Line number is in DE, look it up in the program and set BC to the line num token
	; DE is preserved
	; H is preserved
	; L is not preserved
	; 
	; return with Z set if successful
	;
	; Z clear if not successful, and BC points
	; to the first byte of the line with number
	; greater than the request
	
	LXI B,PROG_BASE-1 ; 1 bytes before PROG_BASE

GetLineNumLoop:
	CALL ATNLN_INXB ; has one INX B preceeding
	RNZ
	
	INX B
	
	; Test for DE <= (BC), and return if true
	LDAX B
	INX B
	SUB E
	MOV L,A
	LDAX B
	SBB D ; C set if DE > (BC), and Z not set
				; C clear if DE <= (BC)
	JC GetLineNumLoop
	
	DCX B
	DCX B
	; Now we want Z set if DE=(BC), clear
	; otherwise 
	
ATNLN_RetNZ: ; shared code. Returns NZ if we know
						 ; that A is non-zero
	ORA L
	
	RET

ATNLN_String:
	RST_LDAXB_INXB_CPI
	DB StringToken
	JNZ ATNLN_String
	
	DB 0c2h ; opcode for JNZ eats 2 bytes
ATNLN_Int: ; Z is always set when we reach here
	INX B
ATNLN_INXB:
	INX B
	
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
	RST_CompareJump
	DB StringToken,(ATNLN_String&0ffh)-1
	JMP AdvanceToNextLineNum

ForSubImpl:
	; Stack contains return address:
	; ExecuteProgramLoop - EPL
	; Keep it there even though it isn't used by 
	; ForSub, it will be used by NextSub
	
	
	PUSH H ; stack has var addr + 1 (VL+1), EPL
	
	RST_LDAXB_INXB_CPI
	
	DB ToToken&0ffh
	CNZ Error
	
	CALL ExpEvaluate
	RST_NegateDE
	
	PUSH D ; stack contains -T,VL+1, EPL
				 ; T is target
				 
	LXI D,1
	LDAX B
	
	RST_CompareJump
	DB StepToken&0ffh,(ForWithStep&0ffh)-1

	DB 21h ; LXI H opcode eats the next 2 bytes
ForWithStep:
	; we have step token
	INX B
	CALL ExpEvaluate
	
	POP H
	POP H
	PUSH B	; stack contains -T,LS,EPL
	DCX SP
	DCX SP
	PUSH D	; stack contains S,-T,LS,EPL
	PUSH H ; stack contains VL+1,S,-T,LS,EPL
	
	JMP ExecuteProgramLoop
	
