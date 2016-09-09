;;;test-q-and-a.lisp

;; item a
(defparameter aques "Who was the first president of the United States?")
(defparameter aanswer '("George Washington" "John Adams" "Thomas Jefferson" "Barack Obama"))
(defparameter a (list aques aanswer))
;; item b
(defparameter bques "Who was the first black president of the United States?")
(defparameter banswer '("Barack Obama" "George Washington" "John Adams" "Thomas Jefferson"))
(defparameter b (list bques banswer))
;; item c
(defparameter cques "Who was the president during the Civil War?")
(defparameter canswer '("Abraham Lincoln" "George Washington" "Theodore Roosevelt" "Barack Obama"))
(defparameter c (list cques canswer))
;; item d
(defparameter dques "Who was the president that purchased Louisiana?")
(defparameter danswer '("Thomas Jefferson" "Abraham Lincoln" "George Washington" "Theodore Roosevelt"))
(defparameter d (list dques danswer))
;; item e
(defparameter eques "Who was the president that said 'Tear down this wall!'?")
(defparameter eanswer '("Ronald Reagan" "Richard Nixon" "John Kennedy" "Harry Truman"))
(defparameter e (list eques eanswer))
;; item f
(defparameter fques "Who was the president that was a famous general?")
(defparameter fanswer '("Dwight Eisenhower" "Ronald Reagan" "Richard Nixon" "John Kennedy"))
(defparameter f (list fques fanswer))
;; item g
(defparameter gques "Who was called 'the Father of the Constitution'?")
(defparameter ganswer '("James Madison" "Thomas Jefferson" "George Washington" "Andrew Jackson"))
(defparameter g (list gques ganswer))
;; item h
(defparameter hques "Which president was NOT from the South?")
(defparameter hanswer '("Barack Obama" "George W. Bush" "Bill Clinton" "Jimmy Carter"))
(defparameter h (list hques hanswer))
;; item i
(defparameter iques "Which president wrote the Declaration of Independence?")
(defparameter ianswer '("Thomas Jefferson" "George Washington" "James Madison" "James Monroe"))
(defparameter i (list iques ianswer))
;; item j
(defparameter jques "Who was NOT a president from the following list?")
(defparameter janswer '("Alexander Hamilton" "Millard Filmore" "James Polk" "Benjamin Harrison"))
(defparameter j (list jques janswer))
;; item k
(defparameter kques "Who was a president from the following list?")
(defparameter kanswer '("John Tyler" "Benjamin Franklin" "Alexander Hamilton" "Aaron Burr"))
(defparameter k (list kques kanswer))

(defparameter items (list a b c d e f g h i j k))
