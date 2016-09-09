;;;     Name:   guesscc.lisp
;;;     Author: Charles Carter
;;;     Date:   September 2, 2016
;;;     Purpose:    implements the guess my number game

(defun start-game ()
  (setf lower-limit 0)
  (setf upper-limit 100)
  (setf answer (read (format t "Enter the answer: ")))
  (game-loop lower-limit upper-limit answer))

(defun game-loop (lower-limit upper-limit answer)
  (setf guess (get-guess lower-limit upper-limit))
  (format t "the guess is ~s~%" guess)
  (cond
    ((= guess answer) 
      (format t "Number guessed, game over"))
    (t 
      (setf hint (read-line (format t "Enter 'up' if guess is too low, 'down' if guess is too high: ")))
      (if (equal hint "up")
          (setf lower-limit (+ guess 1))
          (setf upper-limit (- guess 1)))
      (game-loop lower-limit upper-limit answer))))

(defun get-guess (lower-limit upper-limit)
  (format t "upper ~s, lower ~s~%" upper-limit lower-limit)
    (ash (+ lower-limit upper-limit) -1))
