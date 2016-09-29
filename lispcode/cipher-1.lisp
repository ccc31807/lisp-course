;; cipher-1.lisp

;;;;;;;;;;;;encryption;;;;;;;;;;;;;;;;
(defun encrypt ()
  (let ((text (coerce (read-line (format t "Enter a line of text to be encrypted: ")) 'list))
        (key (- (char-code (char-upcase (read-char (format t "Enter one character as a key: ")))) 64)))
    (get-crypt key text)))

(defun get-crypt (key text)
  (let* ((purified-text (mapcar #'char-code (mapcar #'char-upcase (remove-if-not #'alpha-char-p text))))
         (cy-text (substitute-cyph purified-text key)))
    (format t "cy-text is [~a]~%" cy-text)
    (coerce (mapcar #'code-char cy-text) 'string)))

(defun substitute-cyph (pt k)
  (format t "in substitute-cyph: list is [~a] and key is [~a]~%" pt k)
  (mapcar (lambda (ch) (if (> (+ k ch) 90) (- (+ k ch) 26) (+ k ch))) pt))

;;;;;;;;;;;;encryption;;;;;;;;;;;;;;;;
(defun decrypt ()
  (let ((text (coerce (read-line (format t "Enter a line of text to be decrypted: ")) 'list))
        (key (- (char-code (char-upcase (read-char (format t "Enter one character as a key: ")))) 64)))
    (format t "decrypt, text is [~a] and key is [~a]~%" text key)
    (get-plain text key)))

(defun get-plain (obscure key)
  (let* ((purified-code (mapcar #'char-code (mapcar #'char-upcase (remove-if-not #'alpha-char-p obscure))))
         (plain-text (substitute-plain purified-code key)))
  (format t "in get-plain, text is [~a] and key is [~a]~%" purified-code key)
  (coerce (mapcar #'code-char plain-text) 'string)))

(defun substitute-plain (pt k)
  (format t "in substitute-plain list is [~a] and key is [~a]~%" pt k)
  (mapcar (lambda (ch) (if (< (- ch k) 65) (+ (- ch k) 26) (- ch k))) pt))
