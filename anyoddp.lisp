(defun anyoddp (list)
  (if (eq (car list) nil)
      nil
      (if (eq (oddp (car list)) t)
	  t
	  (anyoddp (cdr list)))))
