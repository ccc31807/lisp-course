;; cipher-5.lisp

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




;(defun transform-to-plain (txt key continuous)
;  (let ((full-key (loop repeat (ceiling (length txt) (length key)) appending key))
;        (cont-key (append key (mapcar (lambda (x) (- x 64)) txt))))
;  (format t "Text: [~a], Full-Key: [~a], Continuous: ~a, Cont-Key: ~a~%" txt full-key continuous cont-key)
;  (coerce (mapcar #'code-char (mapcar #'normalize-out txt (if continuous cont-key full-key))) 'string)))

(defun normalize-out (n k)
  (let ((rv (- n k)))
    (if (< rv 65) (+ rv 26) rv)))

;;;;;;;;;;;;;;;;;;;;encrypt;;;;;;;;;;;;;;;;;;;;;;
;;[125]> (encrypt-1)
;;Enter a line of text to be encrypted: we attack at dawn
;;Enter text to be your key: e
;;Continuous key? (Y|N): n
;;Text: [(87 69 65 84 84 65 67 75 65 84 68 65 87 78)], Full-Key: [(5 5 5 5 5 5 5 5 5 5 5 5 5 5)], Continuou
;;s: NIL, Cont-Key: (5 23 5 1 20 20 1 3 11 1 20 4 1 23 14)
;;"BJFYYFHPFYIFBS"
;;[126]> (encrypt-1)
;;Enter a line of text to be encrypted: we attack at dawn
;;Enter text to be your key: maze
;;Continuous key? (Y|N): n
;;Text: [(87 69 65 84 84 65 67 75 65 84 68 65 87 78)], Full-Key: [(13 1 26 5 13 1 26 5 13 1 26 5 13 1 26 5)
;;], Continuous: NIL, Cont-Key: (13 1 26 5 23 5 1 20 20 1 3 11 1 20 4 1 23 14)
;;"JFAYGBCPNUDFJO"
;;[127]> (encrypt-1)
;;Enter a line of text to be encrypted: we attack at dawn
;;Enter text to be your key: maze
;;Continuous key? (Y|N): y
;;Text: [(87 69 65 84 84 65 67 75 65 84 68 65 87 78)], Full-Key: [(13 1 26 5 13 1 26 5 13 1 26 5 13 1 26 5)
;;], Continuous: T, Cont-Key: (13 1 26 5 23 5 1 20 20 1 3 11 1 20 4 1 23 14)
;;"JFAYQFDEUUGLXH"
;;;;;;;;;;;;;;;;;;;;decrypt;;;;;;;;;;;;;;;;;;;;;;
;;[128]> (decrypt-1)
;;Enter a line of text to be decrypted: BJFYYFHPFYIFBS
;;Enter text that is your key: e
;;Continuous key? (Y|N): n
;;Text: [(66 74 70 89 89 70 72 80 70 89 73 70 66 83)], Full-Key: [(5 5 5 5 5 5 5 5 5 5 5 5 5 5)], Continuou
;;s: NIL, Cont-Key: (5 2 10 6 25 25 6 8 16 6 25 9 6 2 19)
;;NIL
;;[129]> (decrypt-1)
;;Enter a line of text to be decrypted: JFAYGBCPNUDFJO
;;Enter text that is your key: maze
;;Continuous key? (Y|N): n
;;Text: [(74 70 65 89 71 66 67 80 78 85 68 70 74 79)], Full-Key: [(13 1 26 5 13 1 26 5 13 1 26 5 13 1 26 5)
;;], Continuous: NIL, Cont-Key: (13 1 26 5 10 6 1 25 7 2 3 16 14 21 4 6 10 15)
;;NIL
;;[130]> (decrypt-1)
;;Enter a line of text to be decrypted: JFAYQFDEUUGLXH
;;Enter text that is your key: maze
;;Continuous key? (Y|N): y
;;Text: [(74 70 65 89 81 70 68 69 85 85 71 76 88 72)], Full-Key: [(13 1 26 5 13 1 26 5 13 1 26 5 13 1 26 5)
;;], Continuous: T, Cont-Key: (13 1 26 5 10 6 1 25 17 6 4 5 21 21 7 12 24 8)
;;NIL
;;[131]>
;;---------------------------------------------------------------------------
;;[135]> (decrypt-1)
;;Enter a line of text to be decrypted: BJFYYFHPFYIFBS
;;Enter text that is your key: e
;;Continuous key? (Y|N): n
;;Text: [(66 74 70 89 89 70 72 80 70 89 73 70 66 83)], Full-Key: [(5 5 5 5 5 5 5 5 5 5 5 5 5 5)], Continuou
;;s: NIL, Cont-Key: (5 2 10 6 25 25 6 8 16 6 25 9 6 2 19)
;;"WEATTACKATDAWN"
;;[136]> (decrypt-1)
;;Enter a line of text to be decrypted: JFAYGBCPNUDFJO
;;Enter text that is your key: maze
;;Continuous key? (Y|N): n
;;Text: [(74 70 65 89 71 66 67 80 78 85 68 70 74 79)], Full-Key: [(13 1 26 5 13 1 26 5 13 1 26 5 13 1 26 5)
;;], Continuous: NIL, Cont-Key: (13 1 26 5 10 6 1 25 7 2 3 16 14 21 4 6 10 15)
;;"WEATTACKATDAWN"
;;[137]> (decrypt-1)
;;Enter a line of text to be decrypted: JFAYQFDEUUGLXH
;;Enter text that is your key: maze
;;Continuous key? (Y|N): y
;;Text: [(74 70 65 89 81 70 68 69 85 85 71 76 88 72)], Full-Key: [(13 1 26 5 13 1 26 5 13 1 26 5 13 1 26 5)
;;], Continuous: T, Cont-Key: (13 1 26 5 10 6 1 25 17 6 4 5 21 21 7 12 24 8)
;;"WEATGZCFDOCGCM"
;;[138]>


;(defun get-crypt (key text)
;  (let* ((purified-text (mapcar #'char-code (mapcar #'char-upcase (remove-if-not #'alpha-char-p text))))
;         (cy-text (substitute-cyph purified-text key)))
;    (format t "cy-text is [~a]~%" cy-text)
;    (coerce (mapcar #'code-char cy-text) 'string)))
;
;(defun substitute-cyph (cs k)
;  (format t "in substitute-cyph: list is [~a] and key is [~a]~%" cs k)
;  (mapcar (lambda (ch) (if (> (+ k ch) 90) (- (+ k ch) 26) (+ k ch))) cs))
;
;(defun decrypt ()
;  (let ((text (coerce (read-line (format t "Enter a line of text to be decrypted: ")) 'list))
;        (key (- (char-code (char-upcase (read-char (format t "Enter one character as a key: ")))) 64)))
;    (format t "decrypt, text is [~a] and key is [~a]~%" text key)
;    (coerce (mapcar #'code-char (substitute-plain text key)) 'string)))
;
;(defun substitute-plain (text key)
;  (format t "substitute-plain text [~a] key [~a]~%" text key)
;  (let ((num-list (mapcar #'char-code text)))
;    (format t "num-list [~a]~%" num-list)
;    (mapcar (lambda (nl) (if (< 64 (- nl key)) (- nl key) (+ (- nl key) 26))) num-list)))
