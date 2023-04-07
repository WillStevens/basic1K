; An example showing how a shorter parser
; could be constructed.
;
; A single pointer into TokenList
; tracks the parse state. Tokens with common
; prefixes are handled using a 'choice' symbol.
; Integers and strings are parsed in a similar 
; way to other tokens, but with special hooks
; for getting the value
;
; 2023-04-03
; 133 + 97 bytes, not including string or
; var parsing, or writing output. So it will
; end up being about 300 bytes long, saving
; about 200 bytes. 
;
; 2023-04-04
; 154 + 89, not including var parsing or
; writing string output
;
; maybe 30-40  more o could be saved using top 2 
; bits in tokenlist rather than ] | ^ [

org 0h

JMP Start

OutputBuffer:
	DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

TestLine:
DB "10 PRINT 3<A PRINT 123",0

OUTPUT_PTR:
	DW OutputBuffer
	
INT_VALUE:
	DW 0
Start:
	LXI B,TestLine
	
	LXI H,TokenList
	
StartLoop:
	
	LDAX B
	PUSH B
	CPI 0
	JZ Stop
	MOV E,A
	CNZ TokenParse
	POP B
	
	INX B
	
	JMP StartLoop
	
Stop:
	JMP Stop
	

org 100h

TP_Output:
	; output what we have, then goto
	; initial state
	
	MOV A,L
	LHLD OUTPUT_PTR
	CPI (TL_SpaceEnd&0ffh)
	JZ TP_SpaceToken
	CPI (TL_IntEnd&0ffh)
	JNZ TP_NotIntToken
	
	MOV M,A
	INX H
	LDA INT_VALUE
	MOV M,A
	INX H
	LDA INT_VALUE+1
TP_NotIntToken:
	MOV M,A
	INX H
	
	SHLD OUTPUT_PTR
	
	LXI H,0
	SHLD INT_VALUE

TP_SpaceToken:
	LXI H,TokenList
	
	; fall through to TokenParse

; E contains fhe char to parse
; HL is a pointer into TokenList
; or TokenList to identify initial parse state

; HL always points to the position after the
; character we've dealt with
TokenParse:
	MOV A,M
	CPI '['
	JNZ TP_NotChoice
	
TP_ChoiceTestNext:
	INX H
	; If we have reached the end of choices and we
	; are in the test next state, then output
	MOV A,M
	CPI ']'
	JZ TP_Output
	
	INX H
	
	; deal with special characters
	CPI '0'
	JZ TP_MatchZero
	CPI '1'
	JZ TP_MatchOne
	CPI '.' 
	JZ TP_MatchDot
	CPI 'a'
	JZ TP_MatchAlpha
	CMP E
	RZ ; if match and in test next state return
	
TP_ChoiceLoop:
	MOV A,M
	CPI ']'
	JZ TP_SyntaxError
	CPI '|'
	JZ TP_ChoiceTestNext
	CPI '['
	CZ TP_SkipNest ; Z must be set on return
	JZ TP_ChoiceTestNext
	INX H
	JMP TP_ChoiceLoop

TP_SkipNest:
	INX H
	MOV A,M
	CPI ']'
	RZ
	CPI '['
	CZ TP_SkipNest
	JMP TP_SkipNest
	
TP_NotChoice:
	CPI '|'
	JZ TP_Output
	CPI ']'
	JZ TP_SyntaxError
	
	; does char match?
	CMP E
	INX H
	RZ
	
	; otherwise no match
TP_SyntaxError:
	RET

; returns carry set if digit, clear otherwise
TP_IsDigit:
	MOV A,E
	CPI '0'
  CMC
	RNC
	CPI '9'+1
	RET

; returns carry set if alpha, clear otherwise
TP_IsAlpha
	MOV A,E
	CPI 'A'
  CMC
	RNC
	CPI 'Z'+1
	RET
	
TP_MatchZero:
; if we have a digit then accumulate the value
	CALL TP_IsDigit
	JNC TP_ChoiceLoop
	
TP_Digit:
	PUSH H
	LHLD INT_VALUE
	
; Multiply by 10
	PUSH H
	POP D
	
	DAD H
	DAD H
	DAD D
	DAD H

	SUI '0'
	
	MVI D,0
	MOV E,A
	DAD D
	
	SHLD INT_VALUE
	POP H
	
	RET
	
TP_MatchOne:
	CALL TP_IsDigit
	JNC TP_ChoiceLoop
	DCX H
	DCX H
	JMP TP_Digit
	
TP_MatchDot:
	; TODO this is where we
	; copy string contents to PROG_PTR
	DCX H
	DCX H
	RET

TP_MatchAlpha:
	CALL TP_IsAlpha
	JNC TP_ChoiceLoop
	RET


	
org 200h

TokenList:
; 0 means any digit
; 1 means digit repeated any number of times
	DB "[ "
TL_SpaceEnd:
	DB "|"
	DB "0[1|"
TL_IntEnd:
	DB "]"
; . means any character repeated
; any number of times
	DB 34
	DB "["
	DB 34,"|.]"
	DB "P[RINT|]"
	DB "L[ET|]"
	DB "G[O[TO|SUB]|]"
	DB "R[ETURN|UN|]"
	DB "I[NPUT|F|]"
	DB "E[ND|]"
	DB "L[IST|]"
	DB "N[EW|]"
; a means any alphabetic char
	DB "a|"
	DB ",|"
	DB "(|)|"
	DB "<[=|>|]"
	DB ">[=|]"
	DB "=|"
	DB "+|-|*|/]"
TokenListEnd:
	DB 0	
	
