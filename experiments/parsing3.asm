; Idea behind this parser is that no input
; buffer is needed - area after program
; is used to parse tokens as they are
; entered

; GetLine uses registers as follows:
;
; HL contains the address of subroutine to call
; for current char class 
;
; BC is used to store state information about
; the token currently being parsed

org 0400h

PROG_PARSE_PTR:
	DW PROG_BASE
PROG_BASE:

org 0000h

GetLine:

  LXI H,NoCharClass


NextCharLoop:

	IN 1
	ANI 1
	JZ NextCharLoop
	IN 0
  
	PUSH PSW
	
  ; Do we have the same class as before?
  CALL CharClass
  CALL CompareHLDE
  ; Z,C if it is the same class, NZ,NC otherwise

  PCHL ; Jump based on previous CharClass pointer 

NoCharClass:
  XCHG ; Put new class into HL
  LXI B,0 ; reset state information
  PCHL

DigitClass:
  JNZ DigitClassEnd

  POP PSW
 
  ; Accumulate the value into B
	
	; Muliply by 10
	XCHG 	; preserve HL in DE
	MOV H,B
	MOV L,C
	
	DAD H
	DAD H
	DAD B
	DAD H
	
	; Add in the new digit
	ANI 0fh
	
	MVI B,0
	MOV C,A
	DAD B 
	
	MOV B,H
	MOV C,L
	
	XCHG ; Get HL back
  JMP NextCharLoop

DigitClassEnd:

  ; Write token into program
  ; need to preserve DE, don't care about HL
  
  LHLD PROG_PARSE_PTR
  MVI M,27 ; IntegerToken
  INX H
  MOV M,C
  INX H
WriteToken:
  MOV M,B
  INX H
  SHLD PROG_PARSE_PTR

  JMP NoCharClass

AlphaClass:
	JNZ AlphaClassEnd
CompClass:
	NOP
LT0Class:
	JNZ TokenClassEnd
	
	POP PSW
	
	XCHG
	LHLD PROG_PARSE_PTR
	DAD B
	MOV M,A
	XCHG
	
	INX B ; increase count of number of characters
	
	JMP NextCharLoop

AlphaClassEnd:
	; If BC=1 then it's a var
	DCX B
	JZ VarToken
	
	; otherwise fall through and look up the token

TokenClassEnd:
	; Need to preserve DE, don't care about HL
	PUSH D
	LHLD PROG_PARSE_PTR
	; set the high bit of the last char 
	DAD B
	MOV A,M
	ORI 080h
	MOV M,A
	
	XCHG ; put start of token into D
	LXI H,TokenList
	
LookupToken:

	MOV B,M ; B contains the token value
					; if we get a match
	
	PUSH D
	CM StrcmpEntry
	POP D

	JZ FoundToken

	MOV A,M
	INR A
	INX H
  JNZ LookupToken
	CALL Error
	
FoundToken:
	POP D
	LHLD PROG_PARSE_PTR
	JMP WriteToken
	
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
	
	JMP NoCharClass

VarToken:
	; The char at PROG_PARSE_PTR - 1 is a var name
	; need to preserve DE, don't care about HL
	
	LHLD PROG_PARSE_PTR
	DCX H
	
	MOV A,M
	SBI '@'
	MOV M,A
	
	INX H
	SHLD PROG_PARSE_PTR
	
	JMP NoCharClass
	
CharClass:
; each character less than 0 is a
; distinct class.
; 0-9 is a class (class C)
; : to > is a class (class 8)
; >= @ is a class (class 9)
        LXI D,LT0Class
CPI '0'
RC

        LXI D,DigitClass
CPI '9'+1
        RC

        LXI D,CompClass
CPI '@'
RC

LXI D,AlphaClass
RET

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

Error:
	JMP Error
	
org 0300h

TokenList:
	DB PrintSub&0ffh
	DB "PRIN",'T'+128
	DB 255
	
PrintSub:
	NOP