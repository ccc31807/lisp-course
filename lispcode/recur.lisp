(defun fac1 (n)
  ;(format t "calling (fac ~a)~%" n)
  (cond
    ((<= n 1) 1)
    (t (* n (fac1 (1- n))))))

(defun fac2 (n &optional (acc 1))
  ;(format t "calling (fac ~a ~a)~%" n acc)
  (cond
    ((<= n 1) acc)
    (t (- n 1)
       (fac2 (1- n) (* n acc)))))

(defun print-name (first &optional (last 'NONE))
  ;(format t ("my name is ~a ~a~%" first last)))
  (format t ("my name is ~a ~%" first )))
