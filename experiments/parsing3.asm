; Idea behind this parser is that no input
; buffer is needed - area after program
; is used to parse tokens as they are
; entered

; GetLine uses registers as follows:
;
; HL contains the address of subroutine to call
; for current char class 
;
; DE is used for parse state

org 0400h

PROG_PARSE_PTR:
	DW PROG_BASE
PROG_BASE:

org 0000h
	
GetLine:

	LHLD PROG_PARSE_PTR
	PUSH H ; now we can use XTHL to get 
				 ; PROG_PARSE_PTR

FreshStart:

  LXI H,NoCharClass
	
NLTest:
	MVI A,10	; check for newline
	CMP B
	RZ				; return if found

	MOV B,H
	
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
  MVI M,28
  INX H
  MOV M,E
  INX H
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
	; will be 3 bytes
	CPI QuoteClassExpEnd&0ffh
	JZ FreshStart
	
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
	
	; TODO will be 3 bytes
	CPI 0feh
	JZ Write_Shared
	
	LXI D,TokenList

LookupToken_Loop:
	LDAX D
	MOV B,A
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
	
	MOV D,B
	
	JZ Write_Shared
	
LookupToken:
	LDAX D
	INR A
	INX D
  JM LookupToken_Loop
  JNZ LookupToken
	
  ; didn't find it
	
Error:
	JMP Error

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



	
org 0300h

TokenList:
	DB (PrintSub&0ffh)
	DB "PRIN",'T'+128
	DB 255
	
PrintSub:
	NOP