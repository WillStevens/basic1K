;Basic interpreter

;Can a basic interpreter fit in 1K?
;Program is stored as follows:

;<type> [<value>]

B_End equ 0
B_LineNo equ 1
B_If equ 2
B_Let equ 3
B_Goto equ 4
B_Print equ 5
B_Input equ 6
;Every token with bit 7 set can be part of an expression
B_Integer equ 128
B_Plus equ 129
B_Minus equ 130
B_Times equ 131
B_Divide equ 132
B_EQ equ 133
B_NEQ equ 134
B_LT equ 135
B_GT equ 136
B_LTE equ 137
B_GTE equ 138
B_VarA equ 139
B_VarZ equ 164

org 0000h

RunProgram:
lxi h,ProgStart

RP1:
mov a,m
inx h
cpi B_LineNo
jnz RP2
inx h
inx h
jmp RP1

RP2:
cpi B_Goto
jnz RP3
inx h ; No error checkthat this is integer
mov c,m
inx h
mov b,m
call FindLineNo
jnc RPError
jmp RP1

RP3:
cpi B_Print
jnz RP4

call Evaluate
call PrintHex4
jmp RP1

RP4:
RPError:
ret

;Evaluate an expression
;Termination is indicated by a token that can’t be part of an expression (bit 6 not set)

;HL points to the program 
;Value returned in DE

Evaluate:
mov a,m

jmp EvaluateSkip
EvaluateLoop:
mov a,m

ora a ; sign bit clear if we have moved past expression

rp
push d

EvaluateSkip:

inx h

cpi B_Integer
jnz EVAL1

mov e,m
inx h
mov d,m
inx h

jmp EvaluateLoop

EVAL1:
cpi B_VarA
jc EVAL2

sbi B_VarA
add a
push h
lxi h,VarSpace
mov l,a

mov e,m
inx h
mov d,m
pop h

jmp EvaluateLoop 


EVAL2:
cpi B_Plus
jc EVAL3

pop d
xthl

dad  d ; TODO need to check for overflow
xchg
pop h

EVAL3:
jmp EvaluateLoop


;Find the line number in BC
;Return in HL the address immediately following
;carry flag is set if line number found, clear otherwise

FindLineNo: 
lxi h,ProgStart

FLN1:
mov a,m
cpi B_End
rz
cpi B_LineNo
jnz FLN2

mov a,c
inx h
cmp m
inx h
inx h
jnz FLN1
dcx h
mov a,b
cmp m
inx h
stc
rz
jmp FLN1

FLN2:
cpi B_Integer
inx h
jnz FLN1
inx h
inx h
jmp FLN1

;Output the value in DE
;Trashes A,D
PrintHex4:
call PrintHex2
mov d,e

;Output the value in D
;Trashes A
PrintHex2:
mov a,d
rrc a
rrc a
rrc a
rrc a
ani 0fh
call PrintHex
mov a,d
ani 0fh

;Output single hex value
PrintHex:
adi 48
cpi 58
jc PrintHexSkip
adi 7
PrintHexSkip:
out 0
ret

;VarSpace must start at —00h
org 0400h

VarSpace:
;initialise A and reserve space for B-Z
db 18,52
ds 50

ProgStart: 
db 1,10,0,5,139,128,1,0,129
db 1,20,0,4,128,10,0,255
