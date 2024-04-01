10 PRINT "REVERSE"
30 PRINT "CREATIVE COMPUTING MORRISTOWN, NEW JERSEY"
100 PRINT "REVERSE -- A GAME OF SKILL"
150 LET N=9
210 LET @(1)=RND(N)+1
220 FOR K=2 TO N
230 LET @(K)=RND(N)+1
235 LET D=0
240 FOR J=1 TO K-1
250 IF @(K)=@(J) LET D=1
260 NEXT
265 IF D=1 GOTO 230
270 NEXT
290 PRINT
300 PRINT "HERE WE GO ... THE LIST IS:"
310 LET T=0
320 GOSUB 610
330 PRINT "HOW MANY SHALL I REVERSE"
335 INPUT R
350 IF R=0 GOTO 520
360 IF R<=N GOTO 390
370 PRINT "OOPS! TOO MANY! I CAN REVERSE AT MOST ",N
380 GOTO 330
390 LET T=T+1
410 FOR K=1 TO R/2
420 LET Z=@(K)
430 LET @(K)=@(R-K+1)
440 LET @(R-K+1)=Z
450 NEXT
460 GOSUB 610
479 LET D=0
480 FOR K=1 TO N
490 IF @(K)<>K LET D=1
500 NEXT
505 IF D=1 GOTO 330
510 PRINT "YOU WON IT IN ",T," MOVES!!!"
520 PRINT
530 PRINT "TRY AGAIN (1=YES, 0=NO)"
535 INPUT A
550 IF A=1 GOTO 210
560 PRINT
565 PRINT "O.K. HOPE YOU HAD FUN!!"
570 GOTO 999
610 PRINT
620 FOR K=1 TO N
630 PRINT @(K), " ",
640 NEXT
650 PRINT
660 RETURN