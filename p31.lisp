#|
Problem 31
22 November 2002

In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1£1 + 150p + 220p + 15p + 12p + 31p
How many different ways can £2 be made using any number of coins?
|#

;; exercise 1.14 from SICP gives a head start here
;; (define (count-change amount)
;;    (cc amount 5))

;; (define (cc amount kinds-of-coins)
;;    (cond ((= amount 0) 1)
;;          ((or (< amount 0) (= kinds-of-coins 0)) 0)
;;          (else (+ (cc amount
;;                       (- kinds-of-coins 1))
;;                   (cc (- amount
;;                          (first-denomination kinds-of-coins))
;;                       kinds-of-coins)))))

;; (define (first-denomination kinds-of-coins)
;;    (cond ((= kinds-of-coins 1) 1)
;;          ((= kinds-of-coins 2) 5)
;;          ((= kinds-of-coins 3) 10)
;;          ((= kinds-of-coins 4) 25)
;;          ((= kinds-of-coins 5) 50)))


(defun make-change (amount)
  (cc amount 8))

(defun cc (amount kinds-of-coins)
  (cond 
    ((= amount 0) 1)
    ((or (< amount 0)
	(= kinds-of-coins 0)) 0)
    (t  (+ (cc amount (- kinds-of-coins 1))
	   (cc (- amount (first-denomination kinds-of-coins))
	       kinds-of-coins)))))

(defun first-denomination (kinds-of-coins)
  (cond 
    ((= kinds-of-coins 1) 1)
    ((= kinds-of-coins 2) 2)
    ((= kinds-of-coins 3) 5)
    ((= kinds-of-coins 4) 10)
    ((= kinds-of-coins 5) 20)
    ((= kinds-of-coins 6) 50)
    ((= kinds-of-coins 7) 100)
    ((= kinds-of-coins 8) 200)))