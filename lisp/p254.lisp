#|
Problem 254

Define f(n) as the sum of the factorials of the digits of n. For example, f(342) = 3! + 4! + 2! = 32.

Define sf(n) as the sum of the digits of f(n). So sf(342) = 3 + 2 = 5.

Define g(i) to be the smallest positive integer n such that sf(n) = i. Though sf(342) is 5, sf(25) is also 5, and it can be verified that g(5) is 25.

Define sg(i) as the sum of the digits of g(i). So sg(5) = 2 + 5 = 7.

Further, it can be verified that g(20) is 267 and ∑ sg(i) for 1 ≤ i ≤ 20 is 156.

What is ∑ sg(i) for 1 ≤ i ≤ 150?
|#

(defun ! (n)
  (labels ((!-1 (a result)
             (if  (= a 0)
                  result
                  (!-1 (1- a) (* result a)))))
    (!-1 n 1)))

(defun sum-of-factorial-of-digits (number)
  "f(n) as the sum of the factorials of the digits of n"
  (loop for i in (digits number) summing (! i)))

(defun sum-of-digits-of-sum-of-factorial-of-digits (n)
  "sf(n) as the sum of the digits of f(n)"
  (loop for i in (digits (sum-of-factorial-of-digits n)) summing i))

(defun smallest-such-that-sum-is (n)
  (loop for i from 1
     when (= n (sum-of-digits-of-sum-of-factorial-of-digits i))  return i))

(defun sum-of-digits-of-smallest-such-that (n)
  (apply #'+ (digits (smallest-such-that-sum-is n))))

(defun sum-problem-254 (upper)
  (loop for i from 1 to upper summing (sum-of-digits-of-smallest-such-that i)))

(defun problem-254 ()
  (sum-problem-254 150))

(defvar *sf-n* (make-hash-table ))


;; this will take days/weeks, then *sf-n* may have all the minimal values.
(do
 ((i 1 (1+ i)))
 ((gethash 150 *sf-n* ) 'done)
  (let ((c  (sum-of-digits-of-sum-of-factorial-of-digits i)))
    (unless (gethash c *sf-n* )
      (setf (gethash c *sf-n*) i))))

;; to rethink... since we don't need the numbers, only their digits,
;; we can build an n-digit list and find its sum...

;; better still, need to make factorial just yield next
;; (ie, we already computed n-1 factorial)
