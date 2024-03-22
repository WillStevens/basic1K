1 PRINT "Eratosthenes Sieve Prime Number Program"
2 PRINT "Count primes <= Limit"
3 PRINT "Limit must be <= free RAM bytes"
4 PRINT "Limit?",
5 INPUT L LET S = (L-3) / 2 LET C = 0
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
19 PRINT 1+C," primes <= ",L