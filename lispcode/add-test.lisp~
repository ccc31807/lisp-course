;;;add-test.lisp

(print "This is add-lisp. Evaluate (start-test) to start the test.")

(defun start-test ()
  (defparameter number-of-questions 10)
  (defparameter number-correct 0)
  (defparameter question-counter 1)
  (format t "Starting the addition test, you have ~a questions.~%" number-of-questions)
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
      ((zerop number-of-questions)
       (format t "You got ~a correct and made a ~a.~%" number-correct (* 100 (/ number-correct 10.0)))
       T)
      (t (format t "Question ~a. " question-counter)
         (decf number-of-questions)
         (incf number-correct (addition-problem))
		 (incf question-counter)
         (run-test))))

