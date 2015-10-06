;;;testerC.lisp

(defparameter aques "Who was the first president of the United States?")
(defparameter aanswer '("George Washington" "John Adams" "Thomas Jefferson" "Barack Obama"))
(defparameter a (list aques aanswer))
(defparameter bques "Who was the first black president of the United States?")
(defparameter banswer '("Barack Obama" "George Washington" "John Adams" "Thomas Jefferson"))
(defparameter b (list bques banswer))
(defparameter cques "Who was the president during the Civil War?")
(defparameter canswer '("Abraham Lincoln" "George Washington" "Theodore Roosevelt" "Barack Obama"))
(defparameter c (list cques canswer))
(defparameter dques "Who was the president that purchased Louisiana?")
(defparameter danswer '("Thomas Jefferson" "Abraham Lincoln" "George Washington" "Theodore Roosevelt"))
(defparameter d (list dques danswer))
(defparameter eques "Who was the president that said 'Tear down this wall!'?")
(defparameter eanswer '("Ronald Reagan" "Richard Nixon" "John Kennedy" "Harry Truman"))
(defparameter e (list eques eanswer))
(defparameter fques "Who was the president that was a famous general?")
(defparameter fanswer '("Dwight Eisenhower" "Ronald Reagan" "Richard Nixon" "John Kennedy"))
(defparameter f (list fques fanswer))
(defparameter gques "Who was called 'the Father of the Constitution'?")
(defparameter ganswer '("James Madison" "Thomas Jefferson" "George Washington" "Andrew Jackson"))
(defparameter g (list gques ganswer))
(defparameter hques "Which president was NOT from the South?")
(defparameter hanswer '("Barack Obama" "George W. Bush" "Bill Clinton" "Jimmy Carter"))
(defparameter h (list hques hanswer))
(defparameter iques "Which president wrote the Declaration of Independence?")
(defparameter ianswer '("Thomas Jefferson" "George Washington" "James Madison" "James Monroe"))
(defparameter i (list iques ianswer))
(defparameter jques "Who was NOT a president from the following list?")
(defparameter janswer '("Alexander Hamilton" "Millard Filmore" "James Polk" "Benjamin Harrison"))
(defparameter j (list jques janswer))
(defparameter kques "Who was a president from the following list?")
(defparameter kanswer '("John Tyler" "Benjamin Franklin" "Alexander Hamilton" "Aaron Burr"))
(defparameter k (list kques kanswer))

(defparameter items (list a b c d e f g h i j k))

(defun list-answers (answers)
  (cond
    ((null answers) nil)
    (t (let* ((len (length answers))
             (num (random len)))
         (format t "    ~a~%" (nth num answers))
         (list-answers (remove (nth num answers) answers))))))

(defun ask-question (item)
  (format t "~%Question ~a. ~a~%" (incf current-question)(car item)) ;B change to increment current-question
  (list-answers (cadr item))
  (incf total-correct (get-answer (caadr item)))) ;C calls get-answer and increments total-correct

;;part B adds three variables, total-questions, current-question, and total-correct
(defun start-test (number items)
  (defparameter total-questions number)
  (defparameter current-question 0)
  (defparameter total-correct 0)
  (give-test items))

;; checks to see if the current question is greater than total-questions
(defun give-test (items)
      (cond
 ;B change to add condition
 ;C change to print the number correct after the test
        ((or (>= current-question total-questions) (null items)) (format t "Test over, you got ~a correct.~%" total-correct) nil)
        (t (let* ((len (length items))
                  (num (random len)))
             (ask-question (nth num items))
             (give-test (remove (nth num items) items))))))

;; get-answer from student
;; part A
;; returns 1 if answer is correct, 0 otherwise
(defun get-answer (correct)
  (format t "Enter your answer: ")
  (let ((answer (read-line)))
    (cond 
      ((string-equal correct answer) (format t "~%Correct~%") 1)
      (t (format t "~%The correct answer is: ~a~%" correct) 0))))
