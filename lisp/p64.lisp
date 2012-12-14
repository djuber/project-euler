#|
Problem 64
27 February 2004

All square roots are periodic when written as continued fractions and can be written in the form:
√N = a0 + 	
1
  	a1 + 	
1
  	  	a2 + 	
1
  	  	  	a3 + ...

For example, let us consider √23:
√23 = 4 + √23 — 4 = 4 +  	
1
	 = 4 +  	
1
  	
1
√23—4
	  	1 +  	
√23 – 3
7

If we continue we would get the following expansion:
√23 = 4 + 	
1
  	1 + 	
1
  	  	3 + 	
1
  	  	  	1 + 	
1
  	  	  	  	8 + ...

It can be seen that the sequence is repeating. For conciseness, we use the notation √23 = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) repeats indefinitely.

The first ten continued fraction representations of (irrational) square roots are:

√2=[1;(2)], period=1
√3=[1;(1,2)], period=2
√5=[2;(4)], period=1
√6=[2;(2,4)], period=2
√7=[2;(1,1,1,4)], period=4
√8=[2;(1,4)], period=2
√10=[3;(6)], period=1
√11=[3;(3,6)], period=2
√12= [3;(2,6)], period=2
√13=[3;(1,1,1,1,6)], period=5

Exactly four continued fractions, for N ≤ 13, have an odd period.

How many continued fractions for N ≤ 10000 have an odd period?

|#


#|
wikipedia gives an algorithm for calculating the CF of a quadratic S
m_0=0
d_0=1
a_0=floor(sqrt(S))

m_{n+1}=d_n*a_n - m_n
d_{n+1}=(S - m_{n+1}^2)/d_n
a_{n+1} =floor((sqrt{S} + m_{n+1}) /d_{n+1}) 
             =floor((a_0 + m_{n+1})/d_{n+1})
|#




(defun sqrt-continued-fraction (S)
  "return a list of repeating terms of the continued fraction expansion,
and the length of the repeating tail."
  (let ((a (isqrt S)))
    (if (= S (square a))
	(values nil 0)
	;; otherwise, there is a fractional part
	(let ((sequence ())
	      (states (make-hash-table :test #'equal))) ;capture our states
	  (do* 
	   ((m-i 0 (- (* d-i a-i) m-i))
	    (d-i 1 (/ (- S (square m-i)) d-i))
	    (a-i a (floor (/ (+ a m-i) d-i)))
	    (state (list m-i d-i a-i) (list m-i d-i a-i))
	    (count 1 (1+ count)))
	   ((gethash state states)
	    (values (rest (nreverse sequence)) ; omit the leading integer a0
		    ; the length of the cycle is the distance from here to where we first saw it
		    (- count (gethash state states)))) 
	    (push a-i sequence) ; store our coefficient in the list
	    (setf (gethash state states) count)))))) ; note our m, d, a state
	    
	    
(defun sqrt-cf-cycle-length (S)
  (multiple-value-bind (list length)
      (sqrt-continued-fraction S)
    (declare (ignore list))
    length))
	      
(defun p64 ()
  (loop for i from 1 to 10000
     when (oddp (sqrt-cf-cycle-length i))
       count 1))




#|
http://oeis.org/A003285
Period of continued fraction for square root of n (or 0 if n is a square). 
We can use this as a test case.
n 		a(n)
1		0
2		1
3		2
4		0
5		1
6		2
7		4
8		2
9		0
10		1
11		2
12		2
13		5
14		4
15		2
16		0
17		1
18		2
19		6
20		2
21		6
22		6
23		4
24		2
25		0
26		1
27		2
28		4
29		5
30		2
31		8
32		4
33		4
34		4
35		2
36		0
37		1
38		2
39		2
40		2
41		3
42		2
43		10
44		8
45		6
46		12
47		4
48		2
49		0
50		1
51		2
52		6
53		5
54		6
55		4
56		2
57		6
58		7
59		6
60		4
61		11
62		4
63		2
64		0
65		1
66		2
67		10
68		2
69		8
70		6
71		8
72		2
73		7
74		5
75		4
76		12
77		6
78		4
79		4
80		2
81		0
82		1
83		2
84		2
85		5
86		10
87		2
88		6
89		5
90		2
91		8
92		8
93		10
94		16
95		4
96		4
97		11
98		4
99		2
100		0
101		1
102		2
103		12
|#
