#|
21 Sep 2012 09:44 am 
kerson 
Taiwan	
Common Lisp:
|#

(defun find-m (a b k n)
  "Return the m such that (a+b*m)/k and (m^2-n)/k are integers and |(m^2-n)/k| is minimum."
  (let* ((ms (isqrt n))
         (mb (+ ms 1))
         (ds (abs (- (* ms ms) n)))
         (db (abs (- (* mb mb) n)))
         (m (if (> ds db) mb ms))
         (d (min ds db))
         (found))
    (do () (found m)
      (if (and (= 0 (mod d k)) (= 0 (mod (+ a (* b m)) k)))
          (setf found t)
        (if (= m ms)
            (progn
              (decf ms)
              (setf ds (abs (- (* ms ms) n)))
              (setf m mb)
              (setf d db))
          (progn
            (incf mb)
            (setf db (abs (- (* mb mb) n)))
            (setf m ms)
            (setf d ds)))))))

(defun chakravala (n)
  "Return solution of Pell's equation x^2-n*y^1=1 using Chakravala method."
  (let (a b k m)
    (setf b 1)
    (setf a (ceiling (sqrt n)))
    (setf k (- (* a a) n))
    (do () ((= k 1) (values a b))
      (setf m (find-m a b k n))
      (psetf a (/ (+ (* a m) (* n b)) (abs k))
             b (/ (+ a (* b m)) (abs k))
             k (/ (- (* m m) n) k)))))
