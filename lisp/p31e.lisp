#|
28 Jun 2007 07:36 am 
wtwood

CMU Common Lisp, compiled.  I did what amounts to memoization bottom-up.  I built a table of size 201 so that I could index by monetary amounts without offset into zero-based arrays; the entry at index i will have the number of ways the amount i can be made up with the available coins.  The table is initialized to 1 since there is only one way to make up each amount using only pennies (and the value 0 can be made up exactly one way -- none).  Then for each remaining coin c I loop from c to the maximum amount by ones, incrementing each element by the value c spaces below.  When done the value at index i contains the number of ways to make up the value i out of the coins.  Thus I generate the solutions for all values from 1 pence through the desired max.

It isn't perfectly general, but 'tis enough; 'twill serve.

Time:  about 44 microseconds (1000000 repetitions took about 44 sec).

Here's the code:
|#

(defun euler031 ()
  (aref (combination-table '(1 2 5 10 20 50 100 200) 200) 200))

(defun combination-table (coins n)
  (let* ((bound (1+ n))
         (table (make-array bound :initial-element (car coins))))
    (dolist (c (cdr coins))
      (iterate loop ((a c))
        (when (< a bound)
          (incf (aref table a) (aref table (- a c)))
          (loop (1+ a)))))
    table))