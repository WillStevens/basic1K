5 FOR I=2 TO 12
6 LET @(I)=0
7 NEXT
10 PRINT "Simulating throws of two dice."
20 PRINT "How many throws?"
30 INPUT T
40 FOR I=1 TO T
50 LET X=RND(6)+RND(6)+2
60 LET @(X)=@(X)+1
70 NEXT
80 FOR I=2 TO 12
90 PRINT I," : ",@(I)
100 NEXT
