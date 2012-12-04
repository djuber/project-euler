;; peatey

(defconstant coins #(200 100 50 20 10 5 2 1))
(defun euler31 (&optional (goal 200) (coin 0) (ways 0))
  (if (= 7 coin)
      (incf ways)
      (do ((i coin (+ i 1))) ((> i 7) ways)
        (let ((hand (- goal (svref coins i))))
          (when (zerop hand)
            (incf ways))
          (when (plusp hand)
            (incf ways (euler31 hand i 0)))))))