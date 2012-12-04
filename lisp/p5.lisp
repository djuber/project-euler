;; Problem 5
;; 30 November 2001

;; 2520 is the smallest number that can be divided by
;; each of the numbers from 1 to 10 without any remainder.

;; What is the smallest positive number that is evenly divisible
;; by all of the numbers from 1 to 20?

;; Answer:
;; 	232792560

;; this was a pen and paper exercise for me.
;; the trick is to identify the highest multiplicity of each prime factor
;; in the range 2...20 and multiply them together
;; so I took 16 and 9, and crossed off 2,3,4,6,8 (lesser multiplicities)
;; then multiply 5, 7, 11, 13, 17, 19 (primes with multiplicity 1)
(defun p5 ()
  (* (* 2 2 2 2) (* 3 3) 5 7 11 13 17 19))

;; CL-USER> (p5)

;; 232792560


;; the more interesting way to do this is to make a general purpose lcm(lst)
;; or lcm(n) function that returns the least common multiple of all numbers up
;; to n, or of a list of numbers
