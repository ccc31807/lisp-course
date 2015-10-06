;;wiz-1.lisp
;;=====================homework A============================

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

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;=====================homework B============================

(defparameter *objects* '(whiskey bucket frog chain knife dirty-clothes-pile))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)
                                   (knife kitchen)
                                   (dirty-clothes-pile bedroom)))

(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;;=====================homework C============================

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

;;=====================homework D============================

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

