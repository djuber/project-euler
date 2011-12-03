;; JonRock version

(defun digits (num)
  (map 'list 
       #'(lambda (char) (read-from-string (string char)))
       (prin1-to-string num)))
 
(defun digit-fact (n)
  (aref #(1 1 2 6 24 120 720 5040 40320 362880 3628800) n))
 
(defun fact-sum (num)
  (apply #'+ (mapcar #'digit-fact (digits num))))
 
(defun euler34 ()
  (loop for n from 10 to (* 6 (digit-fact 9))
        when (= n (fact-sum n))
          collect n))