(defconstant empty 0 "An empty square")
(defconstant cross -1 "A cross")
(defconstant naught 1 "A naught")

(defconstant winning-positions
  '(
    ;; Horizontal positions
    (0 1 2)
    (3 4 5)
    (6 7 8)

    ;; Vertical
    (0 3 6)
    (1 4 7)
    (2 5 8)
    
    ;; Diagonal
    (0 4 8)
    (6 4 2)))

(deftype piece () `(integer ,empty ,naught))
(defun name-of-piece (piece)
  (cond 
   ((eql piece cross) "X")
   ((eql piece naught) "O")
   ((eql piece empty) ".")
   (t nil)))

(defun opponent (player)
  (if (eql player cross)
      naught
      cross))

(deftype board () '(simple-array piece (9)))

(defun bref (board square) 
  (aref board square))

(defsetf bref (board square) (val)
  `(setf (aref ,board ,square) ,val))

(defun empty-cells (board)
  "Given a board, return a list of indices corresponding to empty
  cells"
  (loop for i from 0 upto 8 when (eql (aref board i) empty) collect i))

(defun initial-board ()
  "Initialise a board to 9 empty squares"
  (let ((board (make-array 9 :initial-element empty)))
    board))

(defun copy-board (board)
  (copy-seq board))

(defun valid-move? (move board)
  (numberp (position move (empty-cells board))))

(defun get-move (strategy player board)
  (let ((move (funcall strategy player (copy-board board))))
    (cond
     ((valid-move? move board)
      (format t "~&~c moves to ~d." (name-of-piece player) move)
      (make-move move player board))
     (t (warn "Illegal move: ~d" move)
        (get-move strategy player board)))))

(defun make-move (move player board)
  (setf (bref board move) player)
  board)

(defun minimax (player board)
;; Loop for moves in the empty cells of the board
;; Try playing that move
;; Get the score - minimax of the new board for the opponent
;; If that score is better, use that as the score with the position
   (if (not (null (outcome board)))
      (progn
        (values (* player (outcome board))
                0))
      (let ((best-val -2)
            (best-move nil)
            (moves (empty-cells board)))
        (dolist (move moves)
          (let* ((board2 (make-move move player (copy-board board)))
                 (val (- (minimax (opponent player) board2))))
            (when (or (eql best-val -2)
                      (> val best-val))
              (setf best-val val)
              (setf best-move move))))
            (values best-val best-move))))

(defun minimax-strategy (player board)
  (print "In minimax strategy")
  (multiple-value-bind (value move)
                       (minimax player board)
    (declare (ignore value))
    move))

(defun winning-position-met (board)
  (loop for winning-position in winning-positions
    when
    (and (not (eql (bref board (first winning-position)) 0))
            (eql (bref board (first winning-position))
                 (bref board (second winning-position)))
            (eql (bref board (first winning-position))
                 (bref board (third winning-position))))
    return (bref board (first winning-position))))

(defun outcome (board)
  (let ((winning-position (winning-position-met board)))
    (cond
     ((numberp winning-position) winning-position)
     ((eql (length (empty-cells board)) 0) 0)
     (t nil))))
 
(defun tictactoe (c-strategy n-strategy)
  (let ((board (initial-board)))
    (loop for player = cross
      then (opponent player)
      for strategy = (if (eql player cross)
                         c-strategy
                         n-strategy)
      until (not (null (outcome board)))
      do 
      (get-move strategy player board)
      (print-board board))
    (printable-result (outcome board))))

(defun printable-result (outcome)
  (cond
   ((eql outcome cross) (format t "~&Crosses won"))
   ((eql outcome naught) (format t "~&Naughts won"))
   ((eql outcome 0) (format t "~&The game is a tie"))))

(defun human (player board)
  "A human player for tictactoe"
  (declare (ignore board))
  (format t "~&~c to move: " (name-of-piece player))
  (read))

(defun random-strategy (player board)
  "Make any legal move"
  (declare (ignore player))
  (random-elt (empty-cells board)))

(defun random-elt (choices)
  (elt choices (random (length choices))))

(defun print-board (board)
  (format t "~2&     1    2    3")
  (loop for row from 0 to 2 do
    (format t "~&  ~d " (* 10 (+ row 1)))
    (loop for col from 0 to 2
      for piece = (bref board (+ col (* row 3)))
      do (format t "~c    " (name-of-piece piece))))
  (format t "~2&"))