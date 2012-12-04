#|
Problem 81
22 October 2004

In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by only moving to the right and down, is indicated in bold red and is equal to 2427.


131	673	234	103	18
201	96	342	965	150
630	803	746	422	111
537	699	497	121	956
805	732	524	37	331

Find the minimal path sum, in matrix.txt (right click and 'Save Link/Target As...'), a 31K text file containing a 80 by 80 matrix, from the top left to the bottom right by only moving right and down.

|#
;; this is very similar to another problem (triangle path?) and amounts to adding up from the bottom.
;; overview : start at bottom right, work backwards by rows 

(defvar *maze* (make-array '(80 80) :element-type 'fixnum))

(defun load-maze (filename)
  "load matrix.txt from file into *maze*. Commas were substituted for spaces
in an editor"
  (with-open-file (file filename :direction :input)
    (do ((line (read-line file nil 'eof) 
	       (read-line file nil 'eof))
	 (linum 0 (1+ linum)))
	((eql linum 80)
	 (eql line 'eof))
      (with-input-from-string (lin line)
	(loop for i from 0 to 79 do
	     (setf (aref *maze* linum i) (read lin))))))) 


(defun random-square (dimension)
  "generate a square array of dimension with random integers"
  (let ((square (make-array (list dimension dimension)
			    :element-type 'fixnum)))
    (loop for i from 0 to (1- dimension) do
	 (loop for j from 0 to (1- dimension) do
	      (setf (aref square i j) (1+ (random (* dimension dimension))))))
    square))

(defun path+ (array row col)
  "determine the lowest cost to this place from its lower or right neighbor"
  (let ((max-row (1- (car (array-dimensions array)))))
    (cond 
      ((= row col max-row) (aref array row col))
      ((= row max-row) 
       (+ (aref array row col) 
	  (aref array row (1+ col))))
      ((= col max-row)
       (+ (aref array row col)
	  (aref array (1+ row) col)))
      (t (+ (aref array row col)
	    (min (aref array (1+ row) col)
		 (aref array row (1+ col))))))))
			 

(defun minpath (maze)
  "fill a temporary maze with minimal costs, and return the cost to the top corner"
  (let ((maze maze)
	(rows (1- (car (array-dimensions maze))))
	(cols (1- (cadr (array-dimensions maze)))))
    (loop for i from rows downto 0 do
	 (loop for j from cols downto 0 do
	      (setf (aref maze i j) (path+ maze i j))))
    (aref maze 0 0)))

(defun p81 ()
  (load-maze "~/src/project-euler/matrix.txt")
  (minpath *maze*))