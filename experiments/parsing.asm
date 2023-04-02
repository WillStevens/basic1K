; An example showing how a shorter parser
; could be constructed.
;
; A single pointer into TokenList
; tracks the parse state. Tokens with common
; prefixes are handled using a 'choice' symbol.
; Integers are parsed in a similar way to other
; tokens, but special hooks for getting the
; value

org 0h

JMP Start

TestLine:
DB "10 IF 3<A PRINT 123",0


INT_VALUE:
	DW 0
Start:
	LXI B,TestLine
	
	LXI H,TokenListEnd
	
StartLoop:
	
	LDAX B
	CPI 0
	JZ Stop
	MOV E,A
	CNZ TokenParse
	INX B
	
	JMP StartLoop
	
Stop:
	JMP Stop
	

org 100h


; E contains fhe char to parse
; HL is a pointer into TokenList
; or TokenListEnd to identify initial parse state

; HL always points to the position after the
; character we've dealt with
TokenParse:
	; First of all, if we are part way through an 
	; int then point back to choice symbol
	MOV A,L
	CPI (TL_IntPartial&0ffh)
	JNZ TP_NotInt
	
	DCX H
	
TP_NotInt:

	; if we have a digit then accumulate the value
	MOV A,E
	CPI '0'
	JC TP_NotDigit
	CPI '9'+1
	JNC TP_NotDigit
	
TP_Digit:
	PUSH H
	LHLD INT_VALUE
	
; Muliply by 10
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
	
	; From this point treat all
	; digits like 0
	MVI A,'0'
	MOV E,A
	
	
TP_NotDigit:

	; Are we in the initial parse state?
	MOV A,L
	CPI (TokenListEnd&0ffh)
	
	JZ TP_InitialState
	
	; No, not in initial parse state
	
	MOV A,M
	CPI '['
	JNZ TP_NotChoice
	
TP_ChoiceTestNext:
	INX H
	; If we have reached the end of choices and we
	; are in the test next state, then output
	MOV A,M
	CPI '#'
	JZ TP_Output
	CPI '^'
	JZ TP_Output
	CMP E
	INX H
	RZ ; if match and in test next state then return
	
TP_ChoiceLoop:
	MOV A,M
	CPI '#'
	JZ TP_InitialState
	CPI '|'
	JZ TP_ChoiceTestNext
	INX H
	JMP TP_ChoiceLoop
	
TP_NotChoice:
	CPI '#'
	JZ TP_Output
	CPI '^'
	JZ TP_Output
	
TP_NotEnd:
	; does char match?
	CMP E
	INX H
	RZ
	
	; otherwise no match, revert to initial state
	
TP_InitialState:
	; In initial parse state, so lookup char
	; in TokenList
	
	LXI H,TokenList
TP_Loop1:
	MOV A,L
	CPI (TokenListEnd&0ffh)
	
	RZ	; if not found then we are in initial
			; parse state
	
	; compare to input char
	MOV A,M
	CMP E
TP_LookForEnd:
	MOV A,M ; to use in 
	INX H
	
	RZ	; if found then return
	
	; if its an end marker then compare next char
	CPI '#'
	JZ TP_Loop1
	
	; otherwise keep looking
	JMP TP_LookForEnd

TP_Output:
	; output what we have, then goto
	; initial state
	
	MOV A,L
	CPI (TL_IntEnd&0ffh)
	JNZ TP_NotIntToken
	
	LXI H,0
	SHLD INT_VALUE
	
	JMP TP_InitialState
	
TP_NotIntToken:
	
	; output token value
	; when testing, put breakpoint here
	
	JMP TP_InitialState
	
org 200h

TokenList:
	DB "0[0"
TL_IntPartial:
	DB "|"
TL_IntEnd:
	DB "#"	; This assists with integer parsing
	DB "PRINT#"
	DB "LET#"
	DB "GO[TO^|SUB#"
	DB "R[ETURN^|UN#"
	DB "I[NPUT^|F#"
	DB "END#"
	DB "RUN#"
	DB "LIST#"
	DB "NEW#"
	DB ",#"
	DB "(#)#"
	DB "<[=^|>^|#"
	DB ">[=^|#"
	DB "=#"
	DB "+#-#*#/#"
TokenListEnd:
	DB 0	; zero can only occur at the end
	
