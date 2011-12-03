;; Problem 36
;; 31 January 2003

;; The decimal number, 585 = 1001001001_(2) (binary), is palindromic in both bases.

;; Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

;; (Please note that the palindromic number, in either base, may not include leading zeros.)
(defun p36 (&optional (limit 1000000))
  "find the sum of all numbers less than limit which are palindromic in base 10 and base 2"
  (let ((s 0))
    (do ((i 1 (1+ i)))
	((= i limit))
      (if (palindrome-10-and-2 i)
	  (incf s i)))
    s))

(defun digits (n &optional (lst ()))
  "break an integer into a list of its digits"
  (if (zerop n)
      lst
      (digits (floor (/ n 10)) (cons (mod n 10) lst))))

  
(defun palindrome-list (lst)
  "are all elements the same when reversed?"
  (if (null (cdr lst))
      T
      (if (equal (the fixnum (first lst)) (the fixnum (car (last lst))))
	  (palindrome-list (subseq lst 1 (- (length lst) 1)))
	  nil)))
     
(defun palindrome (n)
  (if (palindrome-list (digits n))
      T
      nil))

(defun palindrome-10-and-2 (n)
  (if (and
       (palindrome-list (digits n))
       (palindrome-list (binary-digits n)))
      T
      nil))

(defun binary-digits (n &optional (lst ()))
  "break an integer into a list of its binary digits"
  (if (zerop n)
      lst
      (binary-digits (floor (/ n 2)) (cons (mod n 2) lst))))