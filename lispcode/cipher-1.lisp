;; cipher-1.lisp

(defun encrypt ()
  (let ((text (coerce (read-line (format t "Enter a line of text to be encrypted: ")) 'list))
        (key (- (char-code (char-upcase (read-char (format t "Enter one character as a key: ")))) 64)))
    (get-crypt key text)))

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
    (substitute-plain text key)))

(defun substitute-plain (text key)
  (cond
    ((null text) nil)
    (t (let ((ch (char-code (car text))))
         (cons (if (> (+ ch key) 90) (- (- key ch) 26) (- ch key)) (substitute-plain (cdr text) key))))))
