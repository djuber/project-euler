;; If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

;; If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

;; NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

;; what's to be embarrassed about a giant case statement?
;; the right thing to do is sum over(count (alphap (format nil "~r" n))) and add (* 9 99 3) to cover the missing ands

(defun word (n)
  (cond ((zerop n) 0)
	((= n 1) 3)
	((= n 2) 3)
	((= n 3) 5)
	((= n 4) 4)
	((= n 5) 4)
	((= n 6) 3)
	((= n 7) 5)
	((= n 8) 5)
	((= n 9) 4)
	((= n 10) 3)
	((= n 11) 6) ;eleven
	((= n 12) 6) ;twelve
	((= n 13) 8) ;thirteen
	((= n 14) 8) ;fourteen
	((= n 15) 7) ;fifteen
	((= n 16) 7) ;sixteen
	((= n 17) 9) ;seventeen
	((= n 18) 8) ;eighteen
	((= n 19) 8)
	((< n 30) (+ 6 (word (- n 20)))) ;twenty xxx
	((< n 40) (+ 6 (word (- n 30))))
	((< n 50) (+ 5 (word (- n 40))))
	((< n 60) (+ 5 (word (- n 50))))
	((< n 70) (+ 5 (word (- n 60))))
	((< n 80) (+ 7 (word (- n 70))))
	((< n 90) (+ 6 (word (- n 80))))
	((< n 100) (+ 6 (word (- n 90))))
	((= n 100) 10)
	((< n 200) (+ 13 (word (- n 100))))
	((= n 200) 10)
	((< n 300) (+ 13 (word (- n 200))))
	((= n 300) 12)
	((< n 400) (+ 15 (word (- n 300))))
	((= n 400) 11)
	((< n 500) (+ 14 (word (- n 400))))
	((= n 500) 11)
	((< n 600) (+ 14 (word (- n 500))))
	((= n 600) 10)
	((< n 700) (+ 13 (word (- n 600))))
	((= n 700) 12)
	((< n 800) (+ 15 (word (- n 700))))
	((= n 800) 12)
	((< n 900) (+ 15 (word (- n 800))))
	((= n 900) 11)
	((< n 1000) (+ 14 (word (- n 900))))
	((= n 1000) 11)
	(t 0)))

(defun p17 (limit)
  (let ((s 0))
    (do ((a 1 (1+ a)))
	((> a limit))
      (incf s (word a)))
    s))

	    


