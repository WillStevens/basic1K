5 PRINT "   An implementation of"
6 PRINT "John Conway's Game of Life"
7 PRINT "    for 1K Tiny BASIC"
10 LET W=10
20 LET H=10
25 LET A=300
26 GOSUB 100
30 LET @(55)=1
40 LET A=400
50 GOSUB 100
60 LET A=500
70 GOSUB 100
80 LET A=700
82 GOSUB 100
83 LET A=400
84 GOSUB 100
85 END
100 FOR Y=0 TO H-1
120 FOR X=0 TO W-1
130 GOSUB A
200 NEXT
210 NEXT
220 RETURN
300 LET @(W*Y+X)=RND(2)
310 RETURN
400 IF @(W*Y+X)=1 PRINT "#",
410 IF @(W*Y+X)<>1 PRINT ".",
420 IF X=W-1 PRINT
430 RETURN
500 LET S=0
510 FOR V=X-1 TO X+1
520 FOR R=Y-1 TO Y+1
530 LET T=V
540 LET U=R
550 IF T<0 LET T=T+W
560 IF T>=W LET T=T-W
570 IF U<0 LET U=U+H
580 IF U>=H LET U=U-H
590 LET C=@(W*U+T)
600 LET S=S+(C=1)+(C=3)
610 NEXT
620 NEXT
630 LET C=@(W*Y+X)
640 LET D=((S=2)+(S=3))*C+(S=3)*(1-C)
650 LET C=C+2*D
660 LET @(W*Y+X)=C
670 RETURN
700 LET @(W*Y+X)=@(W*Y+X)>=2
