;; Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

;; For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

;; What is the total of all the name scores in the file?

;; sort into alphabetical order!
(defun split-strings (line)
  (let ((lst ())
	(first 0))
    (do ((index 0 (1+ index)))
	((= index (length line)))
	(if (= (base26-value (char line index)) 0)
	    (progn 
	      (setf lst (cons (subseq line first index) lst))
	      (setf first (1+ index)))))
    (setf lst (cons (subseq line first) lst))
    lst))

(defun alphabetize (line)
  (sort (split-strings line) 'string<))

(defun p22 ()
  (let ((result 0))
    (with-open-file (stream "names.txt")
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(setf line (read-name-strip-quotes line))
	(setf result (p22a (alphabetize line)))))
    result))

(defun p22a (lst)
  (let ((s 0) 
	(word-sum 0))
    (do ((element 0 (1+ element)))
	((= element (length lst)))
      ;(format t "in p22 do 1 element ~d name ~a~%" element (nth element lst))
      (let ((line (nth element lst)))
	(do ((index 0 (1+ index)))
	    ((= index (length line)))
	  (incf word-sum (base26-value (char line index)))))
      (incf s (* (1+ element) word-sum)) ; catch the last word
      (setf word-sum 0))
    s))

	
(defun between (n lower upper)
  "is n in interval [lower,upper]"
  (and (>= n lower)
       (<= n upper)))

(defun alphap (a)
  "is a a letter?"
  (or (between (char-code a) (char-code #\A) (char-code #\Z))
      (between (char-code a) (char-code #\a) (char-code #\z))))

(defun upperp (a)
  "is a uppercase?"
  (between (char-code a) (char-code #\A) (char-code #\Z)))
  
;; didn't need a function once I learned to do it
(defun read-name-strip-quotes (line)
  (remove #\" line))

;; didn't need lowercase support here, but it's the right thing to do
(defun base26-value (a)
  (if (not (alphap a))
      0
      (let ((alphabet (string "abcdefghijklmnopqrstuvwxyz")))
	(if (upperp a)
	    (- (char-code a) 64)
	    (- (char-code a) 96)))))
