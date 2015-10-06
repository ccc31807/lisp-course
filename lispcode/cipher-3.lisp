;; cipher-3.lisp

(defun encrypt ()
  (let ((text (coerce (read-line (format t "Enter a line of text to be encrypted: ")) 'list))
        (key (- (char-code (char-upcase (read-char (format t "Enter one character as a key: ")))) 64)))
    (get-crypt key text)))

;;input is a plain text line to be encrypted and a plain text line to be used as a key
;;output is the ACSII code of the upcased plain text with non-alphas removed, and 
;;  the upcased key with non-alphas removed minus 64
(defun encrypt-1 ()
  (let ((plain-text (mapcar #'char-code (coerce (string-upcase (remove-if-not #'alpha-char-p (read-line (format t "Enter a line of text to be encrypted: ")))) 'list)))
        (key (mapcar (lambda (n) (decf n 64)) (mapcar #'char-code (coerce (string-upcase (remove-if-not #'alpha-char-p (read-line (format t "Enter text to be your key: ")))) 'list))))
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


(defun get-crypt (key text)
  (let* ((purified-text (mapcar #'char-code (mapcar #'char-upcase (remove-if-not #'alpha-char-p text))))
         (cy-text (substitute-cyph purified-text key)))
    (format t "cy-text is [~a]~%" cy-text)
    (coerce (mapcar #'code-char cy-text) 'string)))

(defun substitute-cyph (cs k)
  (format t "in substitute-cyph: list is [~a] and key is [~a]~%" cs k)
  (mapcar (lambda (ch) (if (> (+ k ch) 90) (- (+ k ch) 26) (+ k ch))) cs))

(defun decrypt ()
  (let ((text (coerce (read-line (format t "Enter a line of text to be decrypted: ")) 'list))
        (key (- (char-code (char-upcase (read-char (format t "Enter one character as a key: ")))) 64)))
    (format t "decrypt, text is [~a] and key is [~a]~%" text key)
    (coerce (mapcar #'code-char (substitute-plain text key)) 'string)))

(defun substitute-plain (text key)
  (format t "substitute-plain text [~a] key [~a]~%" text key)
  (let ((num-list (mapcar #'char-code text)))
    (format t "num-list [~a]~%" num-list)
    (mapcar (lambda (nl) (if (< 64 (- nl key)) (- nl key) (+ (- nl key) 26))) num-list)))

