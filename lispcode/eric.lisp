;eric.lisp

(defun get-message ()
  (mapcar #'char-code (coerce (remove-if-not #'alpha-char-p (string-upcase (read-line (format t "Please enter your message: ")))) 'list)))

(defun get-key ()
  (mapcar (lambda (n) (- n 64)) (mapcar #'char-code (coerce (remove-if-not #'alpha-char-p (string-upcase (read-line (format t "Please enter your key: ")))) 'list))))
