#|
puzzler said:
#################################
from prime import *
from collections import defaultdict
from xpermutations import *

d = defaultdict(list)

for p in primes():
    if (p>9999):
        break
    if (p>1000):
        key = tuple(sorted(listOfDigits(p)))
        d[key].append(p)

for primeset in d.itervalues():
    if (len(primeset)>=3):
        for pset in xuniqueCombinations(primeset,3):
            if (pset[2]-pset[1]==pset[1]-pset[0]):
                print pset
####################################
i quite like puzzler's code, clean and elegent. so i just translate it to Common Lisp with no hard. sieve is another function for generate primes, timed 0.3s for primes under 10^6.

|#



(defun digits (n)
  (sort (map 'list #'digit-char-p (write-to-string n)) #'<))

(defun p49 ()
  (let ((primes (remove-if-not (lambda (x) (<= 1003 x 9999))
      		               (sieve5 (expt 10 4))))
	;permutation of number are same key
	(dict (make-hash-table :test #'equal)))
  (loop for p in primes do
       (push p (gethash (digits p) dict)))
  (loop for v being the hash-values of dict
     when (>= (length v) 3) do
       (loop with lst = (sort v #'<)
	  for i in lst do
            (loop for j in lst do
		 (loop for k in lst
		    when (and (not (or (eq i j)
				       (eq j k)
				       (eq i k))) 
			      (= (- k j) (- j i)))
		    do (print (list i j k))))))))