#|
22 Mar 2006 07:46 am 
JonRock
LISP 
USA	
   Quote    0

I thought I was being clever by keeping track of exactly how often I see each partial fraction, in case there was a non-repeating section ahead of the repeated digits, but it turns out that the correct answer has every digit repeating.
|#

(defun fill-denom (d)
  (setq denomhash (make-hash-table))
  (setq frac (/ 1 d))
  (loop while (> frac 0)
	do (multiple-value-bind (i2 f2) (floor frac)
	     (if (= (gethash f2 denomhash 0) 2)
		 (return)
	       (incf (gethash f2 denomhash 0)))
	     (setq frac (* 10 f2)))))

(defun count-cycle (d)
  (fill-denom d)
  (loop for v being each hash-value of denomhash
	counting (= v 2)))

(defun find-maximizing-item (list &key (key #'identity) (test #'<))
  (let ((max-item (first list))
	(max-value (funcall key (first list)))
	(max-position 0))
    (loop for item in (rest list)
	  and position from 1
	  for value = (funcall key item)
	  when (funcall test max-value value)
	  do (setf max-item item max-value value max-position position))
    (values max-item max-value max-position)))

(defun seq-list (min max)
  (loop for i from min to max collect i))

(defun euler26 ()
  (find-maximizing-item (seq-list 1 999) :key #'count-cycle)) 