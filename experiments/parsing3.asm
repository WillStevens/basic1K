; Idea behind this parser is that no input
; buffer is needed - area after program
; is used to parse tokens as they are
; entered

; GetLine uses registers as follows:
;
; HL contains the address of subroutine to call
; for current char class 
;
; BC is used for parse state

org 0400h

PROG_PARSE_PTR:
	DW PROG_BASE
PARSE_STATE:
	DW 0
PROG_BASE:

org 0000h
	
GetLine:

	LHLD PROG_PARSE_PTR
	PUSH H ; now we can use XTHL to get 
				 ; PROG_PARSE_PTR

FreshStart:

  LXI H,NoCharClass


NextCharLoop:

	IN 1
	ANI 1
	JZ NextCharLoop
	IN 0
	
	MOV D,A
	
  ; Do we have the same class as before?
  CALL CharClass
  ; are L and E equal?
  MOV A,L
  XRA E
  ; Z if they are equal, NZ if not
  
  PCHL ; Jump based on previous CharClass pointer 

DigitClass:
  JNZ DigitClassEnd
 
  ; Accumulate the value into B
	XCHG ; presevre HL
	; Muliply by 10
	MOV H,B
	MOV L,C
	
	DAD H
	DAD H
	DAD B
	DAD H
	
	; Add in the new digit
	ANI 0fh

	MVI B,0
	MOV C,D
	DAD B
	
	MOV B,H
	MOV C,L
	
	XCHG ; get back HL
	
  JMP NextCharLoop

DigitClassEnd:
  ; Write token into program
  ; need to preserve DE, don't care about HL
  
  XTHL
  MVI M,28
  INX H
  MOV M,C
  INX H
Write_Shared:
  MOV M,B
  INX H
  XTHL

NoCharClass:
  MOV L,E
  XRA A ; set Z
  MOV B,A ; reset state information
  MOV C,A

  PCHL
  
QuoteClassExpEnd:
	
	; update the length
	
	POP H
	PUSH H ; preserve original value
	DAD B
	MOV M,C ; put in -ve length
	
	DCX B

	XTHL
	
	; Z is not set, so skip over 2 chars using JZ opcode. 27 is the opcode for DCX D, so
	; this will affect E only and not D
	DB 0cah
	
QuoteClass:
	; first time through Z is set and A is zero
	; on fall through Z is not set and A 
	; equal to XOR of lo bytes if char is a quote
	
	XTHL
	MVI M,27
	INX H
	MOV M,D
	XTHL
	
	MVI L,QuoteClassExpEnd&0ffh
	
	; will be 3 bytes 
	CPI (QuoteClass^QuoteClassExpEnd) & 0ffh
	JZ FreshStart
	
	JMP NextCharLoop
	
LT0Class:
	INX H	; make sure that succesive LT0Class
				; chars count as separate tokens
	NOP
CompClass:
	NOP
	; If the relative position of CompClass and 
	; AlphaClass change then change CharClass
AlphaClass:
	
	XTHL
	PUSH H
	DAD B
	MOV M,D
	POP H
	XTHL
	
	; if NZ then we will just
	; have written a different class char:
	; good, this ensures no spurious
	; strcmp matches
	JNZ TokenClassEnd 
	
	INX B ; increase count of number of characters
	
	JMP NextCharLoop

TokenClassEnd:
	
	LXI D,TokenList
	
LookupToken:

	MOV B,M ; B contains the token value
					; if we get a match
	
	PUSH H
	CM Strcmp
	POP H

	JZ FoundToken

	LDAX D
	
	INR A
	INX D
  JNZ LookupToken
  
  ; didn't find it, check whether it is a var
	
	; If C=1 it might be a var
	DCR C
	JNZ Error

	XTHL 
	MOV A,M
	SBI '@'
	JC Error
	
	MOV B,A
	XTHL
	
	; fall through
	
FoundToken:
	XTHL
	
	JMP Write_Shared
	
; DE points to hi-bit terminated string
; HL points to string
; Returns Z set if match
; Can't have 128 as a character in D
; (so assume that strings must be ASCII)
Strcmp: 
	INX D
	LDAX D
	CMP M
	RNZ	; On return Z will be clear
	
	CMA
	ANI 128
	RZ	; On return Z will be set 
	
	INX H
	JMP Strcmp


	
CharClass:
; each character less than 0 is a
; distinct class.
; 0-9 is a class (class C)
; : to > is a class (class 8)
; >= @ is a class (class 9)
	MVI E,FreshStart&0ffh
	CPI ' '
	RZ
	
	MVI E,QuoteClass&0ffh
	CPI 34
	RZ
	
	MVI E,LT0Class&0ffh
	CPI '0'
	RC

	MVI E,DigitClass&0ffh
	CPI '9'+1
	RC

	MVI E,CompClass&0ffh
	CPI '@'
	RC

	INR E
	RET

Error:
	JMP Error
	
org 0300h

TokenList:
	DB PrintSub&0ffh
	DB "PRIN",'T'+128
	DB 255
	
PrintSub:
	NOP