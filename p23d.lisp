;; rjminniear   (LISP)  rjminniear is from USA
;; I used the limit of 20161 for the largest number that cannot be expressed as the sum of two abundant numbers. I also found that all even n > 46 can be expressed as the sum of two abundant numbers.

;; The sum of all the even numbers that cannot be expressed as the sum of two abundant numbers is 266. Knowing this, I started with a sum of 266 and then looped, only considering odd numbers.

;; The list of odd abundant numbers is very small, and the only way to create an odd sum is to take an even plus an odd, so I split the list of abundant numbers into a list of odds and a list of evens. By doing this, you can limit your work greatly by only looping across the short list of odds. 


(let ((non-sum-evens 266))
  (defun euler23 (n)
    (let ((even-nums (sort (get-even-abundant-numbers n) #'<))
	  (odd-nums (sort (get-odd-abundant-numbers n) #'<))
	  (sum non-sum-evens))
      (loop for i from 1 to n by 2
	 when (not (abundant-sum-odd? i odd-nums even-nums))
	 do (incf sum i))
      sum)))
 
(defun abundant-sum-odd? (n odds evens)
  (loop for num1 in odds
        until (> num1 n)
        do (loop for num2 in evens
           until (> (+ num1 num2) n)
           do (if (= (+ num1 num2) n)
                  (return-from abundant-sum-odd? t))))
  nil)
 
(defun get-even-abundant-numbers (n)
  (loop for i from 12 to n by 2
        when (abundant-number? i)
        collect i))
 
(defun get-odd-abundant-numbers (n)
  (loop for i from 945 to n by 2
        when (abundant-number? i)
        collect i))
 
(defun abundant-number? (n)
  (> (sum-of-divisors n) n))
 
(defun sum-of-divisors (n)
  (reduce #'+ (proper-divisors n)))
 
(defun proper-divisors (n)
  (let ((divs '(1)))
    (do ((i 2 (1+ i)))
        ((> i (sqrt n)) (remove-duplicates divs))
        (if (zerop (mod n i))
            (progn (setf divs (cons i divs))
                   (setf divs (cons (/ n i) divs)))))))
