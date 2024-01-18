```scheme

;; Complex Scheme code to implement a simple Tic-Tac-Toe game with AI and optional GUI

;; Define the game board as a 3x3 matrix. 'x' represents player's move, 'o' represents AI's move, and '.' represents an empty square.
(define board (make-matrix 3 3 '.'))

;; Define possible winning combinations as lists of board indices.
(define winning-combinations '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6)))

;; Function to check if a player has won the game given a board configuration.
(define winning?
  (lambda (player)
    (for-each (combination winning-combinations)
      (if (every? (lambda (index) (eq? player (board-ref board index))) combination)
        #t)))))

;; Function to check if there are any empty squares left on the board.
(define board-full?
  (lambda ()
    (every? (lambda (square) (eq? '.' square)) (board->list board))))

;; Function to get the next move of the AI player. It uses the minimax algorithm with alpha-beta pruning for intelligent decision-making.
(define ai-move
  (lambda ()
    (let loop
      [(v -∞) (beta +∞)]
      (for-each (combination winning-combinations)
        (if (all? (lambda (index) (eq? '.' (board-ref board index))) combination)
          (set! v (max v (minimax board (list->board combination) #f -∞ v)))
          (set! v (max v (minimax board (list->board combination) #t -∞ v)))
          (set! beta (min beta v))))
      (for-each* (move (generate-moves board))
        (if (= v (minimax board (move->board move) #t -∞ beta))
          move)))))

;; Function to recursively evaluate the minimax algorithm with alpha-beta pruning.
(define minimax
  (lambda (board moves maximizing? alpha beta)
    (if (board-full? board)
      (if (winning? 'o' board)
        1
        (if (winning? 'x' board)
          -1
          0)))
    (if maximizing?
      (let loop
        [(v -∞) (beta +∞)]
        (for-each (move moves)
          (set! v (max v (minimax (move->board move) (moves-without move) #f alpha beta)))
          (set! beta (min beta v)))
        v)
      (let loop
        [(v +∞) (alpha -∞)]
        (for-each (move moves)
          (set! v (min v (minimax (move->board move) (moves-without move) #t alpha beta)))
          (set! alpha (max alpha v)))
        v)))))

;; Function to generate all possible moves for the current player based on the given board configuration.
(define generate-moves
  (lambda (board)
    (for*/list (i (in-range (vector-length board))) (j (in-range (vector-length (board-ref board i))))
      (if (eq? '.' (board-ref board i j))
        (cons (make-move i j) '())))))

;; Function to create a new board configuration after a player's move.
(define move->board
  (lambda (move)
    (let ((new-board (copy-matrix board)))
      (set-board-ref! new-board (car move) (cdr move))
      new-board)))

;; Function to extract all possible moves except the given move from the list of moves.
(define moves-without
  (lambda (move moves)
    (filter (lambda (m) (not (equal? m move))) moves)))

;; Function to swap the player symbols 'x' and 'o'.
(define swap-players
  (lambda (player)
    (if (eq? player 'x')
      'o
      'x)))

;; Function to start a new game. It initializes the game board, displays it, and starts the game loop.
(define new-game
  (lambda ()
    (set! board (make-matrix 3 3 '.'))
    (display-board board)
    (game-loop 'x)))

;; Function to run the game loop. It takes the player symbol ('x' or 'o') as an argument.
(define game-loop
  (lambda (player)
    (if (board-full? board)
      (display-message "Tie!")
      (if (winning? player board)
        (display-message (format "%s wins!" player))
        (let ((move (if (eq? player 'x') (ai-move) (get-player-move))))
          (set! board (move->board move))
          (display-board board)
          (game-loop (swap-players player)))))))

;; Function to display the current game board in the console.
(define display-board
  (lambda (board)
    (for-each (row board)
      (displayln (format "~a~a~a" (board-ref row 0) (board-ref row 1) (board-ref row 2))))))

;; Function to display a message in the console.
(define display-message
  (lambda (msg)
    (newline)
    (displayln msg)))

;; Function to get the player's move from the console.
(define get-player-move
  (lambda ()
    (let ((input (read-line)))
      (when (string-match #rx"[0-2],[0-2]" input)
        (list (string->number (substring input 0 1)) (string->number (substring input 2 3)))))))

;; Function to check if a string matches a regular expression.
(define string-match
  (lambda (regex str)
    (let loop
      [(regex-parts (split-string (substring regex 1 (- (string-length regex) 1)) ",")) (str-parts (split-string str ","))]
      (cond
        ((null? regex-parts)) #t
        ((null? str-parts)) #f
        ((equal? (car regex-parts) "*") (or (null? str-parts) (loop (cdr regex-parts) (cdr str-parts))))
        ((equal? (car regex-parts) (car str-parts)) (loop (cdr regex-parts) (cdr str-parts)))))))

;; Start a new game
(new-game)

```