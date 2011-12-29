#|
essentially the same thing I did, but remhash is better than my sloppy subtraction at the end

28 Oct 2005 08:55 pm 
JonRock
|#
	

(defun fill-ratio-hash (limit)
  (loop for d from 1 to limit
        do (loop for n from (ceiling (/ d 3))
                       to (floor (/ d 2))
                 do (incf (gethash (/ n d) ratiohash 0)))))

(defun euler73 ()
  (setq ratiohash (make-hash-table))
  (fill-ratio-hash 12000)
  (remhash (/ 1 3) ratiohash)
  (remhash (/ 1 2) ratiohash)
  (hash-table-count ratiohash))