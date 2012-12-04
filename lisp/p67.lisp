;; By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

;; 3
;; 7 4
;; 2 4 6
;; 8 5 9 3

;; That is, 3 + 7 + 4 + 9 = 23.

;; Find the maximum total from top to bottom in triangle.txt (right click and 'Save Link/Target As...'), a 15K text file containing a triangle with one-hundred rows.


(let ((a (make-array '(100 100) :initial-element 0 :element-type 'fixnum)))

  (defun fill-array ()
    (with-open-file (stream "triangle.txt" )
		    (do ((line (read-line stream nil)
			       (read-line stream nil))
			 (c 0 (1+ c)))
			((null line))
		      (do ((i 0 (1+ i)))
			  ((> i c))
			(setf (aref a c i) (string-to-number
					       (subseq line (* i 3)
						       (+ 2 (* i 3)))))))))
  (defun add-path ()
    (fill-array)
    (do ((i (- (first (array-dimensions a)) 2) (1- i)))
	((< i 0))
      (do ((j 0 (1+ j)))
	  ((> j i))
	(setf (aref a i j)
	      (+ (aref a i j)
		 (max (aref a (1+ i) j)
		      (aref a (1+ i) (1+ j)))))))
    (aref a 0 0))  )

      
(defun string-to-number (s)
  "grabs all digits in string, skipping whitespace and alpha"
  (let ((l (length s))
	(result 0))
    (do ((count 0 (1+ count)))
	((= count l))
      (if (and (> (char-code (elt s count)) 47)
	       (< (char-code (elt s count)) 58))
	  (setf result (+ (- (char-code (elt s count)) 48)
			  (* result 10)))))
    result))
