(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))

(defun player-letter (n)
  (code-char (+ 65 n)))

(defun draw-board (board)
  (loop for y below *board-size*
        do (progn (fresh-line)
                  (loop repeat (- *board-size* y)
                        do (princ "  "))
                  (loop for x below *board-size*
                        for hex = (aref board (+ x (* *board-size* y)))
                        do (format t "~a-~a " (player-letter (first hex))
                                              (second hex))))))

(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player 
                          spare-dice 
                          first-move 
                          (attacking-moves board player spare-dice))))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *num-players*)
                             0 
                             t)) 
            moves)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
                (mapcan (lambda (dst)
                          (when (and (not (eq (player dst) cur-player))
                                (> (dice src) (dice dst)))
                          (list 
     (list (list src dst)
           (game-tree (board-attack board cur-player src dst (dice src))
                      cur-player 
                      (+ spare-dice (dice dst))
                      nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum*
                  collect n))))

(defun neighbors (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
          when (and (>= p 0) (< p *board-hexnum*))
          collect p)))

(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))

(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)
             (cond ((null lst) nil)
                   ((zerop n) lst)
               (t (let ((cur-player (caar lst))
                        (cur-dice (cadar lst)))
                    (if (and (eq cur-player player) (< cur-dice *max-dice*))
                        (cons (list cur-player (1+ cur-dice)) 
                              (f (cdr lst) (1- n)))
                        (cons (car lst) (f (cdr lst) n))))))))
    (board-array (f (coerce board 'list) spare-dice))))

(defun play-vs-human (tree)
  (print-info tree)
  (if (caddr tree)
      (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))))

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (loop for move in moves
          for n from 1
          do (let ((action (car move)))
               (fresh-line)
               (format t "~a. " n)
               (if action
                   (format t "~a -> ~a" (car action) (cadr action))
                   (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))

(defun winners (board)
  (let* ((tally (loop for hex across board
                      collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
      (format t "The game is a tie between ~a" (mapcar #'player-letter w))
      (format t "The winner is ~a" (player-letter (car w))))))

;xx;To play against a human:
;xx;
;xx;(play-vs-human (game-tree (gen-board) 0 0 t))
;xx
;xx;The code below adds the AI player
;xx
;xx(defun rate-position (tree player)
;xx  (let ((moves (caddr tree)))
;xx    (if moves
;xx      (apply (if (eq (car tree) player)
;xx               #'max
;xx             #'min)
;xx             (get-ratings tree player))
;xx      (let ((w (winners (cadr tree))))
;xx      (if (member player w)
;xx          (/ 1 (length w))
;xx        0)))))
;xx
;xx(defun get-ratings (tree player)
;xx  (mapcar (lambda (move)
;xx          (rate-position (cadr move) player))
;xx        (caddr tree)))
;xx
;xx(defun handle-computer (tree)
;xx  (let ((ratings (get-ratings tree (car tree))))
;xx    (cadr (nth (position (apply #'max ratings) ratings) (caddr tree)))))
;xx
;xx(defun play-vs-computer (tree)
;xx  (print-info tree)
;xx  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
;xx      ((zerop (car tree)) (play-vs-computer (handle-human tree)))
;xx      (t (play-vs-computer (handle-computer tree)))))
;xx
;xx;To play against the computer:
;xx;
;xx;(play-vs-computer (game-tree (gen-board) 0 0 t))
;xx
;xx;The code below optimizes the game and allows play on a 3x3 board
;xx
;xx(defparameter *board-size* 3)
;xx(defparameter *board-hexnum* (* *board-size* *board-size*))
;xx
;xx(let ((old-neighbors (symbol-function 'neighbors))
;xx      (previous (make-hash-table)))
;xx  (defun neighbors (pos)
;xx    (or (gethash pos previous)
;xx        (setf (gethash pos previous) (funcall old-neighbors pos)))))
;xx
;xx(let ((old-game-tree (symbol-function 'game-tree))
;xx      (previous (make-hash-table :test #'equalp)))
;xx  (defun game-tree (&rest rest)
;xx    (or (gethash rest previous)
;xx      (setf (gethash rest previous) (apply old-game-tree rest)))))
;xx
;xx(let ((old-rate-position (symbol-function 'rate-position))
;xx      (previous (make-hash-table)))
;xx  (defun rate-position (tree player)
;xx    (let ((tab (gethash player previous)))
;xx      (unless tab
;xx        (setf tab (setf (gethash player previous) (make-hash-table))))
;xx      (or (gethash tree tab)
;xx          (setf (gethash tree tab)
;xx                (funcall old-rate-position tree player))))))
;xx
;xx(defun add-new-dice (board player spare-dice)
;xx  (labels ((f (lst n acc)
;xx             (cond ((zerop n) (append (reverse acc) lst))
;xx                   ((null lst) (reverse acc))
;xx                   (t (let ((cur-player (caar lst))
;xx                            (cur-dice (cadar lst)))
;xx                        (if (and (eq cur-player player)
;xx                                 (< cur-dice *max-dice*))
;xx                            (f (cdr lst) 
;xx                               (1- n)
;xx                               (cons (list cur-player (1+ cur-dice)) acc))
;xx                          (f (cdr lst) n (cons (car lst) acc))))))))
;xx    (board-array (f (coerce board 'list) spare-dice ()))))
;xx
