
#|
Problem 42
25 April 2003

The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?
|#

;; to-numbers returns 0-25, we need 1-26, and to add them
(defun sum-for-word (string)
  (reduce #'+ (mapcar #'1+ (to-numbers string))))

(defun triangle-p (n)
  "is n triangular?"
  (do* ((i 1 (1+ i))
	(sum i (+ sum i)))
       ((or (= n sum) (> sum n))
	(if (= n sum) t nil))))

(defun slurp-strings (file)
  "grab strings from a comma or space delimited text file"
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char #\, #\Space)
    (with-open-file (stream file)
      (loop for word = (read stream nil)
	   while word
	   collect word))))

(defun count-triangular-words (list)
  (loop 
     for i in list
     when (triangle-p (sum-for-word i))
     count i))

(defun p42 ()
  (count-triangular-words (slurp-strings "~/src/project-euler/words.txt")))


