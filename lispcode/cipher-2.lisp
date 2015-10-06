;; cipher-2.lisp

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
    (coerce (mapcar #'code-char (substitute-plain text key)) 'string)))

(defun substitute-plain (text key)
  (format t "substitute-plain text [~a] key [~a]~%" text key)
  (let ((num-list (mapcar #'char-code text)))
    (format t "num-list [~a]~%" num-list)
    (mapcar (lambda (nl) (if (< 64 (- nl key)) (- nl key) (+ (- nl key) 26))) num-list)))

