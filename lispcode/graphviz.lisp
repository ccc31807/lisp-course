;;; graphviz.lisp
;;; modification of Barski code my Charles Carter

;-----------------------create NODES----------------------------------------
(defparameter aplace 'living-room)
(defparameter adesc '(you are in the living-room. a wizard is snoring loudly on the couch.))
(defparameter a (list aplace adesc))

(defparameter bplace 'garden)
(defparameter bdesc '(you are in a beautiful garden. there is a well in front of you.))
(defparameter b (list bplace bdesc))

(defparameter cplace 'attic)
(defparameter cdesc '(you are in the attic. there is a giant welding torch in the corner.))
(defparameter c (list cplace cdesc))

(defparameter dplace 'bedroom)
(defparameter ddesc '(you are in the bedroom. there is an unmade bed in the corner.))
(defparameter d (list dplace ddesc))

(defparameter eplace 'kitchen)
(defparameter edesc '(You are in the kitchen. there are strange green things growing from dirty plates on the table.))
(defparameter e (list eplace edesc))

(defparameter *nodes* (list a b c d e))

;-----------------------create EDGES--------------------------------------
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

(defparameter qedge 'living-room)
(defparameter qpath-1 '(garden west door))
(defparameter qpath-2 '(attic upstairs ladder))
(defparameter qpath-3 '(kitchen south arch))
(defparameter qpath-4 '(bedroom east door))
(defparameter q (list qedge qpath-1 qpath-2 qpath-3 qpath-4))

(defparameter redge 'garden)
(defparameter rpath-1 '(living-room east door))
(defparameter r (list redge rpath-1))

(defparameter sedge 'attic)
(defparameter spath-1 '(living-room downstairs ladder))
(defparameter s (list sedge spath-1))

(defparameter tedge 'bedroom)
(defparameter tpath-1 '(kitchen west door))
(defparameter tpath-2 '(living-room west door))
;; can't use t as a variable, so use t* instead
(defparameter t* (list tedge tpath-1 tpath-2))

(defparameter uedge 'kitchen)
(defparameter upath-1 '(living-room north arch))
(defparameter upath-2 '(bedroom east door))
(defparameter u (list uedge upath-1 upath-2))

(defparameter *edges* (list q r s t* u))

;----------------------------new code for graphs-----------------------
(defun lg () (load "graphviz.lisp"))

(defun dot-name (exp)
  (substitute-if-not #\_ #'alphanumericp (prin1-to-string exp)))

 (defparameter *max-label-length* 30)

(defun dot-label (expression)
  (let ((str (write-to-string expression :pretty nil)))
    (cond
      ((null expression) "")
      ((> *max-label-length* (length str)) str)
      (t (concatenate 'string (subseq str 0 (- *max-label-length* 3)) "...")))))
        
(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (format t "~&~a[label=\"~a\"];" (dot-name (car node)) (dot-label node)))
        nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (format t "~&~a->~a[label=\"~a\"];" (dot-name (car node)) (dot-name (car edge)) (dot-label (cdr edge)))) (cdr node)))
        edges))

(defun graph->dot (nodes edges)
  (format t "digraph~%{")
  (format t "~%~a~%~a~%~%}~%" (nodes->dot nodes) (edges->dot edges)))

(defun graph2dot (nodes edges stream)
  (format stream "digraph~%{")
  (format stream "~%~a~%~a~%~%}~%" (nodes->dot nodes) (edges->dot edges)))
