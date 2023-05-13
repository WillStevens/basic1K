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

  LXI H,NoCharClass&0ffh

NextCharLoop:

	IN 1
	ANI 1
	JZ NextCharLoop
	IN 0
	
	MOV B,A
	
  ; Do we have the same class as before?
  CALL CharClass
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
	
	; update the length
	
	POP H
	PUSH H ; preserve original value
	DAD D
	MOV M,E ; put in -ve length
	
	DCX D

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
	MOV M,B
	XTHL
	
	LXI H,QuoteClassExpEnd
	
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
	MOV M,B
	INX H
	XTHL
	
	DCX D ; increase char count
	
	; if NZ then we will just
	; have written a different class char:
	; good, this ensures no spurious
	; strcmp matches
	
	JZ NextCharLoop

TokenClassEnd:

	LXI B,TokenList
	
	XTHL
	DAD D
	
LookupToken_Loop:
StrCmp:
	PUSH H
	LDAX B
	INX B
	XRA M
	; iff match then A is either 00h or 80h
	; (80h if last char)
	INX H
	JZ Strcmp ; match and not last char
	
	; equal to 080h iff match and last char
	XRI 080h
	; equal to Z iff match and last char

	POP H
	
	JZ Write_Shared

LookupToken:
	LDAX B
	MOV D,A
	INR A
	INX B
  JM LookupToken_Loop
  JNZ LookupToken
  
  ; didn't find it, check whether it is a var
	
	; If E=2 it might be a var
	DCR E
	DCR E
	JNZ Error

	MOV A,M
	SBI '@'
	JC Error
	
	MOV D,A
	
	JMP Write_Shared
	
; HL points to string
; BC points to string
; One of them mist be hi-bit terminated
; Returns Z set if match
; Can't have 128 as a character in D
; (so assume that strings must be ASCII)

; 27 bytes for 6 classes - can it be simplified?
; bit 6 set : alpha
; bit 5 and 4 set:
;  bits3 and (2 or 1): comp
;  else digit
; else
;  check for space and quote

; bits 6,5,4:
; 000 - not allowed
; 001
; 010 LT0, space or quote
; 011 digit or comp
; 100 alpha
;
; Could be done with 24 bytes using lookup
; table:
;
; ! Space
; " LT0
; 35 quote
; 0 LT0
; 58 digit
; 64 comp
; 255 alpha
;
; PUSH H
; MVI L,LookupClass&0ffh
; LookupClassLoop:
; CMP M
; INR L
; JNC LookupClassLoop
; MOV C,M
; POP H

CharClass:
; each character less than 0 is a
; distinct class.
; 0-9 is a class (class C)
; : to > is a class (class 8)
; >= @ is a class (class 9)
	MVI C,FreshStart&0ffh
	CPI ' '
	RZ
	
	MVI C,QuoteClass&0ffh
	CPI 34
	RZ
	
	MVI C,LT0Class&0ffh
	CPI '0'
	RC

	MVI C,DigitClass&0ffh
	CPI '9'+1
	RC

	MVI C,CompClass&0ffh
	CPI '@'
	RC

	INR C
	RET

Error:
	JMP Error
	
org 0300h

TokenList:
	DB (PrintSub&0ffh)
	DB "PRIN",'T'+128
	DB 255
	
PrintSub:
	NOP