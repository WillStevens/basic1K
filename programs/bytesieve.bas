0 PRINT "Eratosthenes Sieve Prime Number"
1 PRINT "       Program in BASIC"
2 PRINT "Size must be <= free RAM bytes / 2"
3 PRINT "Size?"
5 INPUT S LET C = 0
6 FOR I = 0 TO S
7 LET @(I) = 1
8 NEXT
9 FOR I = 0 TO S
10 IF @(I) = 0 GOTO 18
11 LET P = I+I + 3
12 LET K = I + P
13 IF K > S GOTO 17
14 LET @(K) = 0
15 LET K = K + P
16 GOTO 13
17 LET C = C + 1
18 NEXT
19 PRINT C," PRIMES"