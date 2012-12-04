;; If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.

;; Not all numbers produce palindromes so quickly. For example,

;; 349 + 943 = 1292,
;; 1292 + 2921 = 4213
;; 4213 + 3124 = 7337

;; That is, 349 took three iterations to arrive at a palindrome.

;; Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome. A number that never forms a palindrome through the reverse and add process is called a Lychrel number. Due to the theoretical nature of these numbers, and for the purpose of this problem, we shall assume that a number is Lychrel until proven otherwise. In addition you are given that for every number below ten-thousand, it will either (i) become a palindrome in less than fifty iterations, or, (ii) no one, with all the computing power that exists, has managed so far to map it to a palindrome. In fact, 10677 is the first number to be shown to require over fifty iterations before producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).

;; Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.

;; How many Lychrel numbers are there below ten-thousand?

;; NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel numbers.

(defun p55 (&optional (limit 10000))
  (let ((results ()))
    (do ((n 1 (1+ n)))
	((> n limit))
      (if (lychrel n)
	  (push n results)))
    (length results)))

(defun lychrel (n &optional (counter 0))
  (if (> counter 50)
      T
      (let ((next (next-candidate n)))
	(if (palindrome next)
	    nil
	    (lychrel next (1+ counter))))))

(defun next-candidate (n)
  (+ n (digits-to-number (reverse (digits n)))))

(defun digits (n &optional (lst ()))
  "break an integer into a list of its digits"
  (if (zerop n)
      lst
      (digits (floor (/ n 10)) (cons (mod n 10) lst))))

(defun digits-to-number (lst)
  "undo digits transformation"
  (if (null lst)
      0
      (+ (car (last lst))
	 (* 10 (digits-to-number (reverse (rest (reverse lst))))))))

(defun palindrome (n)
  (if (palindrome-list (digits n))
      T
      nil))

(defun palindrome-list (lst)
  "are all elements the same when reversed?"
  (if (null (cdr lst))
      T
      (if (equal (the fixnum (first lst)) (the fixnum (car (last lst))))
	  (palindrome-list (subseq lst 1 (- (length lst) 1)))
	  nil)))
