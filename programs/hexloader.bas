1 GOTO 5
2 PRINT "0102A0A1AAFF"
4 PRINT "A is even. Num bytes is even"
5 PRINT ^
6 LET A=2048
7 LET A=A/2-(^+1)/2
10 LET X=4160
11 GOSUB 1000
15 LET X=X+1
20 IF Y=34 GOTO 40
30 GOTO 11
40 LET D=0
41 LET C=0
50 GOSUB 1000
55 IF Y=34 GOTO 150
60 LET Y=Y-48
70 IF Y>9 LET Y=Y-7
80 LET D=D*16+Y
90 LET C=C+1
95 LET X=X+1
96 IF C=2 LET E=D
97 IF C=2 LET D=0
100 IF C<4 GOTO 50
110 LET @(A)=D*256+E
115 LET A=A+1
120 GOTO 40
150 PRINT USR(2048)
160 END
1000 LET W=X/2
1020 LET Y=@(-(^+1)/2+W)
1025 PRINT Y
1030 IF X-W-W LET Y=Y/256
1040 LET Y=Y-(Y/256)*256
1050 RETURN
