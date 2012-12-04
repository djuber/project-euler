;; You are given the following information, but you may prefer to do some research for yourself.

;;     * 1 Jan 1900 was a Monday.
;;     * Thirty days has September,
;;       April, June and November.
;;       All the rest have thirty-one,
;;       Saving February alone,
;;       Which has twenty-eight, rain or shine.
;;       And on leap years, twenty-nine.
;;     * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

;; How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

(defun days-in-month (month year)
  (cond ((and (= month 1)
	      (zerop (mod year 4))
	      (/= year 1900)) 29)
	((= month 1) 28)
	((or (= month 0)  ;January
	     (= month 2)  ;March
	     (= month 4)  ;May
	     (= month 6)  ;July
	     (= month 7)  ;August
	     (= month 9)  ;October
	     (= month 11)) ;December
	 31)
	(t 30)))

;; ploddingly literal, but worked the first time round
(defun p19 ()
  (let ((day-of-week 1) ; monday
	(year 1900) ; year
	(month 0) ; month as integer mod 12
	(day-of-month 0) ; day mod length of month
	(count 0)) ; number of sundays the first of a month
    (do ((date 0)) ; date as integer
	((and (= year 2000)
	      (= month 11)
	      (= day-of-month 2)))
      (if (and (= day-of-month 1) ; is it the first?
	       (= (mod day-of-week 7) 0) ; is it Sunday
	       (> year 1900)) ; don't count until 1901 Jan 1st
	  (incf count))
      (incf date)
      (setf day-of-week (mod (1+ day-of-week) 7))
      (setf day-of-month (mod (1+ day-of-month) (days-in-month month year)))
      (if (= day-of-month 0)
	  (setf month (mod (1+ month) 12)))
      (if (and (= month 0)
	       (= day-of-month 0))
	  (incf year)))
    count))

;; how someone who knows lisp does the same
;; runs in 1/5 the time (10M cycles vs 55M cycles for above)
(defun euler19 (&optional
                (b-year 1901)
                (e-year 2000)
                (day 6))
  (let ((total 0))
    (do ((y b-year (+ y 1))) ((> y e-year) total)
      (do ((m 1 (+ m 1))) ((> m 12) 'done)
        (if (= day (seventh (multiple-value-list
                             (decode-universal-time
                              (encode-universal-time 1 0 0 1 m y)))))
            (incf total))))))
