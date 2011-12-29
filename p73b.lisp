#|
28 Jan 2007 06:24 am 
dktrudgett
|#

(defun euler-073 ()
  "How many fractions, n/d, lie between 1/3 and 1/2 in the sorted set
of reduced proper fractions for d <= 12,000"
  (loop for d from 1 to 12000
        sum (loop for n from 1 to (1- d)
                  for frac = (/ n d)
                  when (and (= n (numerator frac))
                            (< 1/3 frac 1/2))
                  sum 1 into count
                  finally (return count)) into count
        finally (return count)))