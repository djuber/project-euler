;; oluf

(let ((table (make-hash-table :test #'equal)))
  (defun num-changes (value denominations)
    (cond ((gethash (cons value denominations) table))
          (t (setf (gethash (cons value denominations)
                            table)
               (cond ((< value 0) 0)
                     ((= value 0) 1)
                     ((null denominations) 0)
                     (t (+ (num-changes
                             (- value (first denominations)) 
                             denominations)
                           (num-changes 
                             value 
                             (rest denominations))))))))))

(defun euler31 ()
  (num-changes 200 '(200 100 50 20 10 5 2 1)))
