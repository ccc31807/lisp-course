;;;add-test.lisp

(print "this is add-lisp")

(defun start-test ()
  (defparameter count 10)
  (defparameter correct 0)
  (format t "Starting the addition test, you have ~a questions.~%" count)
  (run-test))

(defun addition-problem ()
  (let* ((a (random 11))
         (b (random 11))
         (c (+ a b))
         (d (read (format t "What is ~a + ~a? " a b))))
    (cond ((= c d)
           (format t "Correct~%")
            1)
          (t (format t "The answer is ~a~%" c)
              0))))

(defun run-test ()
    (cond 
      ((zerop count)
       (format t "you got ~a correct and made a ~a.~%" correct (* 100 (/ correct 10.0))))
      (t (format t "Question ~a. " count)
         (decf count)
         (incf correct (addition-problem))
         (run-test))))

