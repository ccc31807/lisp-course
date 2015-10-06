(defun hello ()
    (format t "hello"))
	
(defun hello-name ()
    (format t "hello, ~A" "Charles"))

(defun hello-var ()
        (let ((a "Charles")
              (b "Carter"))
             (format t "Hello ~A ~A" a b)))
             
    