;; trace-test.lisp

(setf num-list '(1 2 3 4 5 6 7 8 9))

(setf alpha-list '(a b c d e f g))

(defun count-em (lst)
  (cond
    ((null lst) 0)
    (t (+ 1 (count-em (cdr lst))))))

(defun add-em (l)
  (cond
    ((null l) 0)
    (t (+ (car l) (add-em (cdr l))))))

(defun multiply-em (l)
  (cond
    ((null l) 1)
    (t (* (car l) (multiply-em (cdr l))))))
