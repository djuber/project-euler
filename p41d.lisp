 ;V Convert a list of digits to a base 10 integer V
 (defun digits-to-int (lis)
   (loop for digit in lis
	 for x = digit then (+ (* 10 x) digit)
	 finally (return x)))

 ;V Create a "nest" of closures which V
 ;V returns a new permutation with    V
 ;V every call to the outermost one   V
 (defun permunest (lis) 
   (when (endp lis);-> No infinite recursion 
     (return-from permunest #'(lambda () nil)))
   (let* ((head (list (pop lis)))
	  ;^ Head of this list ^
	  (htail head);-> Tail of 'head
	  ;V Closure reponsible for permutations V
	  ;V of a list one element smaller       V
	  (subperm (permunest (copy-list lis)))
	  subval);-> Temporary value from 'subperm
     #'(lambda () 
	 ;V Check if the 'subperm closure V
	 ;V can handle more permutations  V
	 (if (endp (setq subval (funcall subperm)))
	   ;V Check if this closure can V
	   ;V handle more permutations  V
	   (if (endp lis) nil;-> if not, return nil
	     (progn
	       ;V      | Before  | After
	       ;V head | (1 2 3) | (5 2 3 4)
	       ;V tail | (4)     | (1)
	       ;V lis  | (5 6 7) | (6 7)
	       (setf (cdr htail) (list (car head))
		     (car head) (pop lis)
		     htail (cdr htail))
	       ;V Set temporary variable for return V
	       (setq subval (append head lis)
		     ;V Set new subordinate closure V
		     subperm (permunest (copy-list (cdr subval))))
	       subval));-> return (append head lis)
	   ;V Return the first element of 'head consed  V
	   ;V with the permutation returned by 'subperm V
	   (cons (car head) subval)))))

 (print
   (loop
     ;V Descending digit list from V
     ;V which permutations start   V
     with plis = '(9 8 7 6 5 4 3 2 1)
     ;V Permutation closure V
     with p = (permunest plis)
     ;V Result of each purmutation V
     for diglis = plis then (funcall p)
     ;V When closure p returns nil, V
     when (endp diglis) do
     (setq plis (cdr plis) diglis plis
	   p (permunest plis))
     ;^ begin process again with ^
     ;^ one less digit in 'plis  ^
     when (isPrime (digits-to-int diglis))
     ;^ When a prime is found ^
     ;V return it             V
     return (digits-to-int diglis)))