;; cipher.lisp

;;;;;;;;;;;;;;;encryption;;;;;;;;;;;;;;;;;;;;;
(defun encrypt-1 ()
  (let ((plain-text (mapcar #'char-code (coerce (string-upcase (remove-if-not #'alpha-char-p (read-line (format t "Enter a line of text to be encrypted: ")))) 'list)))
        (key (mapcar (lambda (n) (decf n 64)) (mapcar #'char-code (coerce (string-upcase (read-line (format t "Enter text to be your key: "))) 'list))))
        (continuous (y-or-n-p (format t "Continuous key? (Y|N): "))))
    (transform-to-code plain-text key continuous)))

(defun transform-to-code (txt key continuous)
  (let ((full-key (loop repeat (ceiling (length txt) (length key)) appending key))
        (cont-key (append key (mapcar (lambda (x) (- x 64)) txt))))
  (format t "Text: [~a], Full-Key: [~a], Continuous: ~a, Cont-Key: ~a~%" txt full-key continuous cont-key)
  (coerce (mapcar #'code-char (mapcar #'normalize-in txt (if continuous cont-key full-key))) 'string)))

(defun normalize-in (n k)
  (let ((rv (+ n k)))
    (if (> rv 90) (- rv 26) rv)))

;;;;;;;;;;;;;;;decryption;;;;;;;;;;;;;;;;;;;;;
(defun decrypt-1 ()
  (let ((jumbled-text (mapcar #'char-code (coerce (string-upcase (remove-if-not #'alpha-char-p (read-line (format t "Enter a line of text to be decrypted: ")))) 'list)))
        (key (mapcar (lambda (n) (decf n 64)) (mapcar #'char-code (coerce (string-upcase (read-line (format t "Enter text that is your key: "))) 'list))))
        (continuous (y-or-n-p (format t "Continuous key? (Y|N): "))))
    (transform-to-plain jumbled-text key continuous)))

(defun transform-to-plain (txt key continuous)
  (let ((full-key (loop repeat (ceiling (length txt) (length key)) appending key)))
  (format t "Text: [~a], Full-Key: [~a], Continuous: ~a, Cont-Key: ~a~%" txt full-key continuous key)
  (cond
    ((not continuous)
     (coerce (mapcar #'code-char (mapcar #'normalize-out txt full-key)) 'string))
    (t
     (setf rv (rolling-decrypt txt key nil))
     (coerce (mapcar #'code-char (reverse rv)) 'string)))))

(defun rolling-decrypt (txt key plain)
  (cond
    ((null txt)
     plain)
    (t
      (setf E (car txt))
      (setf K (car key))
      (setf D (- E K))
      (if (< D 65) (incf D 26))
      (rolling-decrypt (cdr txt) (append (cdr key) (list (- D 64))) (cons D plain)))))

(defun normalize-out (n k)
  (let ((rv (- n k)))
    (if (< rv 65) (+ rv 26) rv)))
