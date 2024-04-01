10 LET A = 1000 
20 LET B = 0 
30 LET F = 150 
40 LET V = 50 
50 LET T = 0 
100 PRINT "T:" , T , " Alt:" , A , " Vel:" , V , " Fuel:" , F , " Thr:" , B 
111 IF F > 30 PRINT "Thrust (0-30)?",
112 IF F < 31 PRINT "Thrust (0-" , F , ")?",
113 INPUT B 
114 IF B >= 0 IF B <= 30 IF B <= F GOTO 120 
115 GOTO 111 
120 LET W = V - B + 5 
121 LET F = F - B 
122 LET A = A - ( V + W ) / 2 
123 LET V = W 
124 LET T = T + 1 
125 IF A > 0 GOTO 100 
126 IF V < 5 GOTO 140 
127 PRINT "You crashed!" 
128 GOTO 160 
140 IF A < 0 GOTO 150 
141 PRINT "Perfect landing!" 
142 GOTO 160 
150 PRINT "Touchdown." 
160 IF A < 0 LET A = 0 
170 PRINT "T:" , T , " Alt:" , A , " Vel:" , V , " Fuel:" , F 
180 END