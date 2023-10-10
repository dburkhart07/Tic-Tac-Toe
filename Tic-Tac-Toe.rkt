(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) ; you might want this

(define SIZE 679) ;; can range from [300,900]
(define MTS (empty-scene SIZE SIZE))
(define WIDTH (/ SIZE 60))
(define PEN (make-pen "medium cyan" (round WIDTH) "solid" "round" "bevel"))
(define box-width (/ SIZE 10))
(define board-boarders-end (- SIZE box-width))
(define board-boarders-start (+ 0 box-width))
(define board-size (- SIZE (* 2 box-width)))
(define tile-size (/ board-size 3))
(define SIZE2 (+ tile-size box-width))
(define SIZE3 (+ (* 2 tile-size) box-width))
(define X-color "spring green")
(define O-color "cornflower blue")
(define text-size (round (/ SIZE 5)))

(define X-text (text "X" text-size X-color))
(define O-text (text "O" text-size O-color))
;; Convert 0-based row and column to Pos
(define (r-c->pos r c) (+ (* r 3) c))  ;helpful for writing tests
(define (pos->r-c- pos) (make-posn
                         (modulo pos 3)
                         (/ (- pos (modulo pos 3)) 3)))

;; Unit is (listof Pos) of length 3
;; interp. 
;;  The position of every square in a unit. There are
;;  9 of these for the 3 rows, and 3 columns.

;(define UNITS (append ROWS COLS))
  



;; Constants:

(define ALL-VALS (list "X" "O"))

(define B false) ;B stands for blank

(define BD1
  (list B B B
        B B B
        B B B))

(define BD2
  (list "X" B "O"
        "O" "X" B
        "O" "X" "O"))

(define BD3
  (list "X" "O" "X"
        "O" "X" "O"
        "X" "O" "X"))

(define BD4
  (list "X" "O" "X"
        "O" "X" "O"
        "O" B B))

(define BD5
  (list "X" "X" "O"
        B   "X" "O"
        "O" "O" B  ))

(define BD6
  (list "O" "X" "O"
        B  "O" "X"
        "X" "X" "O"))
(define BD7
  (list "X" "O" "X"
        "X" "O" "O"
        "O" "X" "X"))
(define BD8
  (list "X" "O" "X"
        "O"  B  "X"
        "O" "X" "O"))


(define ROWS
  (list
   (list 0 1 2)
   (list 3 4 5)
   (list 6 7 8)))

(define COLS
  (list
   (list 0 3 6)
   (list 1 4 7)
   (list 2 5 8)))

(define DIAGS
  (list
   (list 0 4 8)
   (list 2 4 6)))

(define ALLWINS
  (append ROWS COLS DIAGS))

(define make-grid
  (add-line
   (add-line
    (add-line
     (add-line MTS SIZE2 board-boarders-end SIZE2 box-width PEN)
     SIZE3 board-boarders-end SIZE3 box-width PEN)
    box-width SIZE3 board-boarders-end SIZE3 PEN)
   box-width SIZE2 board-boarders-end SIZE2 PEN))




;;Data Definitions:

(define-struct ws (board turn won? difficulty))
;;ws is (make-ws (listof String|False) String (Boolean|String) Natural)
;;interp. board is the board you're starting with
;;        turn is whoever's turn it is
;;        won? is whether someone has won, and if they have, who it is
;;        difficulty is how difficult it is (how many depths ahead
;;        it will search)

(define (fn-for-ws ws)
  (...                 
   (fn-for-board (ws-board ws))
   (ws-turn ws)
   (ws-won? ws)
   (ws-difficulty ws)))

        
(define (fn-for-board board)
  (cond[(empty? board) (...)]
       [else
        (... (first board)
             (rest board))]))

(define START (make-ws BD1 "Player" false 0))
(define START2 (make-ws BD5 "Computer" false 2))
(define START3 (make-ws BD1 "Player" false 5))
(define START4 (make-ws BD1 "Player" false 9))
(define START5 (make-ws BD1 "Computer" false 0))
(define START6 (make-ws BD3 "Computer" "X" 0))


;; main is top level because it is the main function

;;to run: (main START)
(define (main ws)
  (big-bang ws
    (on-tick computer-move)
    (to-draw render-board)
    (on-key handle-key)
    (on-mouse player-move)))


;; computer-move is top level because it is in big-bang

;;WS -> WS
;;Checks if a computer move can be made, and if it can, does it

(check-expect (computer-move START)
              START);base case (not computer's move, doesn't make it)

(check-expect (computer-move START6)
              START6);someone has already won, no move made

(check-random (computer-move START5)
              (make-ws
               (computer-turn (ws-difficulty START5) (ws-board START5))
               "Player"
               false
               (ws-difficulty START5))) ;random computer move made

             
(define (computer-move ws)
  (if (or (string=? (ws-turn ws) "Player")
          (not (false? (ws-won? ws))))
      ws
      (local
        [;;Constants:
         (define new-board
           (computer-turn (ws-difficulty ws) (ws-board ws)))]
        
        (make-ws new-board
                 "Player"
                 (o-won? new-board)
                 (ws-difficulty ws)))))

;; computer-turn is top level because it is used to define a local variable in
;; computer-move, not “necessary” but easier to read and use 🙂

;; Natural Board -> Board
;; Takes the optimal computer turn based on the
;; given difficulty and board, returns board with turn made

(check-random (computer-turn 0 BD1)
              (local
                [(define new-computer-value-assigner
                   (computer-value-assigner BD1 0 0))]
       
                (add-o (index-of new-computer-value-assigner
                                 (argmax (λ (n) (+ n 0))
                                         (map (λ (n)
                                                (if (number? n)
                                                    n
                                                    -100))
                                              new-computer-value-assigner)))
                       BD1))) ;Base case

(check-random (computer-turn 3 BD4)
              (local
                [(define new-computer-value-assigner
                   (computer-value-assigner BD4 0 3))]
       
                (add-o (index-of new-computer-value-assigner
                                 (argmax (λ (n) (+ n 0))
                                         (map (λ (n)
                                                (if (number? n)
                                                    n
                                                    -100))
                                              new-computer-value-assigner)))
                       BD4))) ;Nonzero difficulty



(define (computer-turn difficulty board)
  (local
    [;;Constants:
     (define new-computer-value-assigner
       (computer-value-assigner board 0 difficulty))]
       
    (add-o (index-of new-computer-value-assigner
                     (argmax (λ (n) (+ n 0))
                             (map (λ (n)
                                    (if (number? n)
                                        n
                                        -100))
                                  new-computer-value-assigner)))
           board)))

;; computer-value-assigner is top level for the same reason as computer-turn,
;; easier to read and to use

;;Board Natural Natural -> (listof String|Number)
;; Assigns a value to all unfilled squares
;; on the board based on difficulty

(check-random (computer-value-assigner BD1 0 0)
              (map (λ (n)
                     (if (false? n)
                         (- (/ (random 1000) 1000) 0.5)
                         n)) BD1));base case and depth starts equal to difficulty

(check-random (computer-value-assigner BD3 0 0)
              (map (λ (n)
                     (if (false? n)
                         (- (/ (random 1000) 1000) 0.5)
                         n)) BD3));random assignment with some fillings in board

(check-random (computer-value-assigner BD2 2 5)
              (minimax BD2 2 5 0 true));even depth

(check-random (computer-value-assigner BD3 3 7)
              (minimax BD3 3 7 0 false));odd depth

(define (computer-value-assigner board depth difficulty)
  (cond
    [(= depth difficulty) (map (λ (n)
                                 (if (false? n)
                                     (- (/ (random 1000) 1000) 0.5)
                                     n))
                               board)]
    [(even? depth)
     (minimax board depth difficulty 0 true)]
    [(odd? depth)
     (minimax board depth difficulty 0 false)])) 


;; minimax is top level because it is a complex function that needs many test cases

;; Board Natural Natural Natural Boolean -> (listof String|Number)
;; Runs minimax by checking each tile and assigning a value based on
;; the outcome at the current depth or futer down, assigning
;; less value to wins further down

(check-random (minimax BD1 0 0 9 false) empty) ;no position 9

(check-random (minimax BD2 0 4 3 true)
              (cons (list-ref BD2 3)
                    (minimax BD2 0 4 (add1 3) true)));string at position

(check-random (minimax BD5 2 7 8 true)
              (append (list 1)
                      (build-list (- 9 8) (λ (n) 0))));o can win on given move

(check-random (minimax BD4 0 6 8 false)
              (append (list -1)
                      (build-list (- 9 8) (λ (n) 0))));x will win if given move performed

(check-random (minimax BD8 2 5 4 false)
              (cons 0 empty));draw is inevitable

(check-random (minimax BD4 0 7 7 true)
              (cons (* .99 (argmin (λ (n) (+ n 0))
                                   (filter number?
                                           (computer-value-assigner
                                            (add-o 7 BD4) (+ 0 1) 7))))
                    (minimax BD4 0 7 (add1 7) true)));normal move for o
(check-random (minimax BD5 0 7 3 false)
              (cons (* .99 (argmax (λ (n) (+ n 0))
                                   (filter number?
                                           (computer-value-assigner
                                            (add-x 3 BD5) (+ 0 1) 7))))
                    (minimax BD5 0 7 (add1 3) false)));normal move for x


(define (minimax board depth difficulty position is-o)
  
  ;;depth: Natural; represents how many moves ahead have been searched.
  ;                 stops when the depth is equal to the difficulty
  ;                 initialized to 0
  ;;position: Natural; represents the current position being searched.
  ;                    stops when position reaches a number greater
  ;                    than the numbers on the board (8),
  ;                    initialized to 0
  (cond
    [(> position 8) empty]
    [(string? (list-ref board position))
     (cons (list-ref board position)
           (minimax board depth difficulty (add1 position) is-o))]
    ;;checking if O or X have won
    [(and is-o
          (has-won? "O" (add-o position board)))
     (append (list 1)
             (build-list (- 9 position) (λ (n) 0)))]
    
    [(and (not is-o)
          (has-won? "X" (add-x position board)))
     (append (list -1)
             (build-list (- 9 position) (λ (n) 0)))]
    
    ;checking if there is a draw
    
    [(and (not is-o)
          (string? (x-won? (add-x position board))))
     (cons 0 empty)]
    
    [is-o (cons (* .99 (argmin (λ (n) (+ n 0))
                               (filter number?
                                       (computer-value-assigner
                                        (add-o position board) (add1 depth) difficulty))))
                (minimax board depth difficulty (add1 position) is-o))]
    
    [else (cons (* .99 (argmax (λ (n) (+ n 0))
                               (filter number?
                                       (computer-value-assigner
                                        (add-x position board) (add1 depth) difficulty))))
                (minimax board depth difficulty (add1 position) is-o))]))

;; render-board is top level because it is in big-bang

;; WS -> Image
;; Draws the board based on the current world state

(define (render-board ws)
  (local
    [;;Board Natural -> Image
     ;;Runs through the board, and renders all X's and O's 
     ;;and puts that on top of a grid
     (define (get-details board position)
       ;;position: Natural; represents the current position being searched.
       ;                    stops when position reaches a number greater
       ;                    than the numbers on the board (8),
       ;                    initialized to 0
       (cond
         [(empty? board) make-grid]
         [else
          (local
            [;;Constant
             (define position2
               (pos->r-c- position))
             
             ;; Natural -> Natural
             ;; Gets the x or y position to center the X/O on the tile
             ;; based on column/row
             ;; Assume that any number being passed into this is on the board [0,8]
             (define (get-location n)
               (+ box-width (/ tile-size 2) (* tile-size n)))

             ;;String|False -> Image
             ;;checks to see if the element is an "X", "O", or neither
             ;;and outputs the corresponding image

             (define (check-image var)
               (cond[(false? var) (square 0 "solid" "white")]
                    [else
                     (if (string=? var "X")
                         X-text
                         O-text)]))]
            
            (place-image
             (check-image (first board))
             (get-location (posn-x position2))
             (get-location (posn-y position2))
             (get-details (rest board) (add1 position))))]))
     (define transparent (make-color 255 255 255 200))]

    (if (string? (ws-won? ws))
        (local[;;String -> Image
               ;;Displays whoever the winner or produces a draw, based on the string
               (define (display-winner winner)
                 (cond
                   [(string=? winner "Draw")
                    (text "DRAW" text-size "black")]
                   [(string=? winner "X")
                    (text "X WINS" text-size X-color)]
                   [else
                    (text "O WINS" text-size O-color)]))]
          (place-image
           (display-winner (ws-won? ws))
           (/ SIZE 2)
           (/ SIZE 2)    
           (place-image
            (square SIZE "solid" transparent)
            (/ SIZE 2)
            (/ SIZE 2)
            (get-details (ws-board ws) 0))))
        (get-details (ws-board ws) 0)))) 

;; player-move is top level because it is 🕟 in big-bang

;; WS Natural Natural MouseEvent -> WS
;; Places an x where the player clicks if it's their turn
;; as long as the move is legal
(check-expect (player-move START 0 0 "button-down")
              START);clicking outside of boarder

(check-expect (player-move START (- box-width 1) (- box-width 1) "button-down")
              START) ;edge-case just ouside boarder (nothing happens)

(check-expect (player-move START box-width (- box-width 1) "button-down")
              START);only one coordinate in bounds (nothing happens)

(check-expect (player-move START box-width box-width "button-down")
              (make-ws (list "X" B B
                             B  B B
                             B  B B)
                       "Computer"
                       (x-won? (ws-board START))
                       (ws-difficulty START))) ;edge-case on just inside boarder

(check-expect (player-move START (/ SIZE 2) (/ SIZE 2) "button-down")
              (make-ws (list  B  B  B
                              B "X" B
                              B  B  B)
                       "Computer"
                       (x-won? (ws-board START))
                       (ws-difficulty START))) ;normal click

(check-expect (player-move START (+ SIZE 1) (+ SIZE 1) "button-down")
              START);out of bounds case (nothing happens)
(check-expect (player-move START (/ SIZE 2) (/ SIZE 2) "button-up")
              START) ;misclick
              

(define (player-move ws x y me)
  (cond 
    [(or (string? (ws-won? ws))
         (string=? (ws-turn ws) "Computer"))
     ws]
    [(mouse=? me "button-down")
     (place-x x y ws)]
    [else ws]))

;; place-x is top level for readability purposes

;; Natural Natural WS -> WS
;; Puts an X at the correct tile
;; as long as the move is legal
(check-expect (place-x 0 0 START)
              START);clicking outside of boarder

(check-expect (place-x (- box-width 1) (- box-width 1) START)
              START) ;edge-case just ouside boarder (nothing happens)

(check-expect (place-x box-width (- box-width 1) START)
              START);only one coordinate in bounds (nothing happens)

(check-expect (place-x box-width box-width START)
              (make-ws (list "X" B B
                             B  B B
                             B  B B)
                       "Computer"
                       (x-won? (ws-board START))
                       (ws-difficulty START))) ;edge-case on just inside boarder

(check-expect (place-x (/ SIZE 2) (/ SIZE 2) START)
              (make-ws (list  B  B  B
                              B "X" B
                              B  B  B)
                       "Computer"
                       (x-won? (ws-board START))
                       (ws-difficulty START))) ;normal click

(check-expect (place-x (+ SIZE 1) (+ SIZE 1) START)
              START);out of bounds case (nothing happens)
                        

(define (place-x x y ws)
  (if (and (>= x board-boarders-start)
           (<= x board-boarders-end)
           (>= y board-boarders-start)
           (<= y board-boarders-end))
      
      (local
        [;Constants
         (define col 
           (round (- (/ (- x box-width) tile-size) 0.5)))
         (define row
           (round (- (/ (- y box-width) tile-size) 0.5)))
         (define position
           (r-c->pos row col))
         (define new-board
           (add-x position (ws-board ws)))]

        (if (equal? new-board (ws-board ws))
            ws
            (make-ws new-board
                     "Computer"
                     (x-won? new-board)
                     (ws-difficulty ws)))) 
      ws))

;; add-x is top level because it is used in multiple functions

;; Natural Board -> Board
;; adds an X at the position on the board if there is no X/O there already
;; Assume: position passed in is legal position [0,8]
(check-expect (add-x 0 BD1)
              (cons "X" (rest BD1))) ;First spot add
(check-expect (add-x 4 BD1)
              (list B  B  B
                    B "X" B
                    B  B  B)) ;Middle of board add
(check-expect (add-x 7 BD4)
              (list "X" "O" "X"
                    "O" "X" "O"
                    "O" "X" B)) ;Already an O there
(check-expect (add-x 8 BD4)
              (list "X" "O" "X"
                    "O" "X" "O"
                    "O"  B  "X")) ;Added to make a win

(define (add-x position board)
  ((add-string "X") position board))

;; add-o is top level because it is used in multiple functions

;; Natural Board -> Board
;; adds an O at the position on the board if there is no X/O there already
;; Assume: position passed in is legal position [0,8]
(check-expect (add-o 0 BD1)
              (cons "O" (rest BD1))) ;First spot add
(check-expect (add-o 4 BD1)
              (list B  B  B
                    B "O" B
                    B  B  B)) ;Middle of boad add
(check-expect (add-o 3 BD4)
              (list "X" "O" "X"
                    "O" "X" "O"
                    "O"  B   B)) ;Already an O there
(check-expect (add-o 8 BD5)
              (list "X" "X" "O"
                    B  "X" "O"
                    "O" "O" "O")) ;Added to make a win

(define (add-o position board)
  ((add-string "O") position board))


;; add-string is top level because it is used in two functions
;; and is higher order because it generalizes code from both add-x and add-o

;;String -> (Natural Board -> Board)
;;Creates a function that adds a string to the board
;;at the given location
;;Cases are tested with add-o and add-x
(define (add-string string)
  (λ (position board)
    (if (false? (list-ref board position))
        (list-set board position string)
        board)))

;; x-won? is top level because it is used in multiple functions

;;Board -> String|False
;;Checks to see if X has won or there is a draw
;;if not, returns false

; never a case where o wins on the last move available
; because x will always be the last one to play if there
; is to be a full board, and therefore got to play
; because o hadn't won yet
(check-expect (x-won? BD3) "X"); x wins
(check-expect (x-won? BD4) false) ;nobody has won
(check-expect (x-won? BD6) false) ;o wins
(check-expect (x-won? BD7) "Draw");draw case
(check-expect (x-won? BD7) "Draw");nobody wins

(define (x-won? board)
  ((who-won? "X") board))


;; o-won? is top level because it is used in multiple functions

;;Board -> String|False
;;Checks to see if O has won or there is a draw
;;if not, returns false

;potential edge case: checking if o-won on
;a full board, when x has won instead. That should
;never happen though, as when the last x is placed
;the computer will not get a chance to move, and
;therefore not get to call o-won

(check-expect (o-won? BD4) false) ;nobody has won
(check-expect (o-won? BD6) "O") ;o wins
(check-expect (o-won? BD7) "Draw");draw case

(define (o-won? board)
  ((who-won? "O") board))


;; who-won? is top level because it is used in multiple functions
;; and is higher order because it generalizes code from both x-won? and o-won?


;;String -> (Board -> String|False)
;;Produces a function to check to see
;;if the specified turn has won or there
;;is a draw. Returns false otherwise.

;;Cases are tested with x-won? and o-won?

(define (who-won? turn)
  (λ (board)
    (cond
      [(has-won? turn board) turn]
      [(empty? (filter false? board)) "Draw"]
      [else false])))


;; has-won? is top level because it is used in multiple functions

;; String Board -> Boolean
;; Checks if the player passed in has won

(check-expect (has-won? "X" BD3) true) ;X won
(check-expect (has-won? "X" BD1) false);Nobody has won
(check-expect (has-won? "O" BD3) false);Computer won instead
(check-expect (has-won? "O" (build-list 8 (λ (n) "O"))) true);O won 

(define (has-won? turn board)
  (ormap(λ (set)
          (andmap (λ (tile)
                    (equal? (list-ref board tile) turn))
                  set))
        ALLWINS))

;; handle-key is top level because it is in big-bang 🌌

;;WS KeyEvent -> WS
;;Changes the difficulty in the worldstate if
;;a legal number [0, 9] is pressed
(check-expect (handle-key START "0")
              (make-ws (ws-board START)
                       (ws-turn START)
                       (ws-won? START)
                       (string->number "0")));Same number, but should still "change" ws
(check-expect (handle-key START "1")
              (make-ws (ws-board START)
                       (ws-turn START)
                       (ws-won? START)
                       (string->number "1")));Upping the difficulty
(check-expect (handle-key START3 "2")
              (make-ws (ws-board START3)
                       (ws-turn START3)
                       (ws-won? START3)
                       (string->number "2")));Lowering the difficulty
(check-expect (handle-key START "a") START);nothing happens (misprint)
      
(define (handle-key ws ke)
  (cond
    [(or (string? (ws-won? ws))
         (string=? (ws-turn ws) "Computer")) ws]
    [(number? (string->number ke))
     (make-ws (ws-board ws)
              (ws-turn ws)
              (ws-won? ws)
              (string->number ke))]
    [else ws]))

