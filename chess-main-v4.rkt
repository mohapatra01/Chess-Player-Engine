#lang racket
(require "Rules-v6.rkt")
(require "minimax-v4.rkt")
(require (prefix-in htdp: 2htdp/universe) 2htdp/image)
(require racket/mpair)

(define whose-move 1) ;;to determine whose chance is now and that initially it's white's chance
(define previous-move -1)
(define chessboard
  (vector
   (vector -9  -11 -13 -15 -16 -14 -12 -10)
   (vector -1  -2  -3  -4  -5  -6  -7  -8)                              
   (vector  0   0   0   0   0   0   0   0)
   (vector  0   0   0   0   0   0   0   0)
   (vector  0   0   0   0   0   0   0   0)
   (vector  0   0   0   0   0   0   0   0)
   (vector  1   2   3   4   5   6   7   8)
   (vector  9   11  13  15  16  14  12  10)))
;(define chessboard
;         (vector #(-9 0 -13 -16 -10 0 0 0)
;  #(-1 0 -3 0 0 15 -7 -8)
;  #(0 0 -4 0 0 0 0 -14)
;  #(0 0 0 0 0 6 0 0)
;  #(0 -2 0 0 0 0 0 0)
;  #(0 0 3 -12 0 0 7 12)
;  #(1 2 0 0 0 0 0 8)
;  #(9 11 0 16 0 10 0 0)))
                     
;(valid-move? 8 2 8 4 chessboard)
;(get-piece 8 2 chessboard)
;(get-piece 8 4 chessboard)

(define game-moves (list (my-vector-copy chessboard)))  ;;this mlist contains chessboards of all moves from initial chessboard

(define 1p #f)

;;represents last comp move
(define l '())
(define xi-comp 9)
(define yi-comp 9)
(define xf-comp 9)
(define yf-comp 9) 

;; mouse events

(define (mouse-handler STATE x y action)
  (define chessboard (my-vector-copy (State-board STATE)))
  (let ([xf (+ 1 (quotient x 100))]   ;;Xc starts from 0 to 7.
        [yf (+ 1 (quotient y 100))])  ;;Yc starts from 0 to 7.
    
  (cond [(equal? action "button-down") 
          (cond [(equal? (State-game-started? STATE) "2p")
                 (cond [(State-piece-selected? STATE) ;;making move
                        (let* ([xi (car (State-piece-selected? STATE))]
                               [yi (cdr (State-piece-selected? STATE))]
                               [move-which (begin (displayln (list xi yi xf yf))
                                                  (get-piece xi yi chessboard))]
                               [attack-which (get-piece xf yf chessboard)]
                               [king (if (> move-which 0) king_white
                                         king_black)])
                          (begin ;(displayln (list xf yf xi yi))
                                 (cond [(and (= xi xf) (= yi yf))
                                        (set! whose-move whose-move)] ;;when player clicks on the same piece
                                       ;;to cancel previous selection.
                                       [(valid-move? xi yi xf yf chessboard)
                                        (begin (displayln "move completed")
                                               (cond [(king? move-which) ;;king moves
                                                      (begin (cond [(not (= 0 attack-which))
                                                                    (begin (let* ([piece (get-piece xf yf chessboard)])
                                                                             (if (> piece 0)
                                                                                 (delete_w piece )
                                                                                 (delete_b piece )))
                                                                           (displayln "OPPONENT LOST HIS PIECE"))])
                                                             (set-KING-Xc! king xf)
                                                             (set-KING-Yc! king yf)
                                                             (set-KING-move-status! king #t)
                                                             (make-move1 xi yi xf yf chessboard)
                                                             (set! game-moves (cons (my-vector-copy chessboard) game-moves))
                                                             (set-State-board! STATE chessboard))]
                                                     [else (begin (cond [(not (= 0 attack-which)) ;;others
                                                                         (begin (let* ([piece (get-piece xf yf chessboard)])
                                                                                  (if (> piece 0)
                                                                                      (delete_w piece )
                                                                                      (delete_b piece )))
                                                                                (displayln "OPPONENT LOST A PIECE"))])
                                                                  (make-move1 xi yi xf yf chessboard)
                                                                  (set! game-moves (cons (my-vector-copy chessboard) game-moves))
                                                                  (set-State-board! STATE chessboard))])
                                       (set! previous-move whose-move)
                                       (set! whose-move (- 0 whose-move)))])
                         (set-State-piece-selected?! STATE #f)
                         STATE))]
               [else (let ([piece (get-piece xf yf chessboard)])  ;;piece selection n check who's move
                       (cond [(> (* piece whose-move) 0) (begin
                                                           (set-State-piece-selected?! STATE (cons xf yf))
                                                           STATE)] 
                             [else (begin (displayln "NOT YOUR MOVE")
                                          STATE)]))])]
                ;;AI;;
                [(equal? (State-game-started? STATE) "1p") (cond [(State-piece-selected? STATE)
                 (let* ([xi (car (State-piece-selected? STATE))]
                        [yi (cdr (State-piece-selected? STATE))]
                        [move-which (begin (displayln (list xi yi xf yf))
                                           (get-piece xi yi chessboard))]
                        [attack-which (get-piece xf yf chessboard)]
                        [king (if (> move-which 0) king_white
                                  king_black)])
                   (begin ;(displayln (list xf yf xi yi))
                         (cond [(and (= xi xf) (= yi yf))
                                 (set! whose-move whose-move)] ;;when player clicks on the same piece
                                                               ;;to cancel previous selection.
                               [(valid-move? xi yi xf yf chessboard)
                                (begin (displayln "press enter")
                                       (cond [(king? move-which)
                                              (begin (cond [(not (= 0 attack-which))
                                                             (begin (let* ([piece (get-piece xf yf chessboard)])
                                                                          (if (> piece 0)
                                                                              (delete_w piece )
                                                                              (delete_b piece )))
                                                                    (displayln "YOU LOST A PIECE"))])
                                                     (set-KING-Xc! king xf)
                                                     (set-KING-Yc! king yf)
                                                     (set-KING-move-status! king #t)
                                                     (make-move1 xi yi xf yf chessboard)
                                                     (set! game-moves (cons (my-vector-copy chessboard) game-moves))
                                                     (set-State-board! STATE chessboard))]
                                             [else (begin (cond [(not (= 0 attack-which))
                                                                 (begin (let* ([piece (get-piece xf yf chessboard)])
                                                                          (if (> piece 0)
                                                                              (delete_w piece )
                                                                              (delete_b piece )))
                                                                        (displayln "COMPUTER LOST A PIECE"))])
                                                          (make-move1 xi yi xf yf chessboard)
                                                          (set! game-moves (cons (my-vector-copy chessboard) game-moves))
                                                          (set-State-board! STATE chessboard))])
                                       (set! previous-move whose-move)
                                       (set! whose-move (- 0 whose-move)))])
                         (set-State-piece-selected?! STATE #f)
                         STATE))]
               [else (let ([piece (get-piece xf yf chessboard)])
                       (cond [(> whose-move 0) (begin
                                                 (set-State-piece-selected?! STATE (cons xf yf))
                                                 STATE)] 
                             [else (begin (displayln "PRESS ENTER")
                                          STATE)]))])]
                ;;;gameplay ends
                [1p (cond [(and (< x 550) (> x 250) (< y 500) (> y 400)) 
                           (begin (set-depth 2)
                             (set-State-game-started?! STATE "1p") STATE)]
                          [(and (< x 550) (> x 250) (< y 400) (> y 300))  
                           (begin (set-depth 1)
                             (set-State-game-started?! STATE "1p") STATE)]
                          [else STATE])]
                [else (cond [(and (< x 550) (> x 250) (< y 500) (> y 400)) ;start 2p
                             (begin (set-State-game-started?! STATE "2p") STATE)]
                            [(and (< x 550) (> x 250) (< y 400) (> y 300))  ;start 1p
                             (begin (displayln "Esc to go back") (set! 1p #t) STATE)]
                            [(and (> 625 (expt (- x 750) 2)) (> 625 (expt (- y 50) 2)))
                             (begin (set-State-game-started?! STATE "instructions")
                                    (displayln "left-right arrow keys to navigate")
                                    (displayln "Esc to go back")
                                    STATE)]
                            [else STATE])])]
        
        [else STATE])))

;;stopping condition
(define (check-mate STATE)
  (let ([chessboard (State-board STATE)])
    (or (checkmate? (KING-val king_black) chessboard)
        (checkmate? (KING-val king_white) chessboard))))

;;king under check
(define (check? STATE)
  (let ([chessboard (State-board STATE)])
    (cond [(under-check? (KING-val king_black) chessboard) (get-piece-pos -16 chessboard)]
          [(under-check? (KING-val king_white) chessboard) (get-piece-pos 16 chessboard)]
          [else #f])))

;; initialisations

(define w-pawn (bitmap "pieces/w-pawn.png"))
(define w-rook (bitmap "pieces/w-rook.png"))
(define w-knight (bitmap "pieces/w-knight.png"))
(define w-bishop (bitmap "pieces/w-bishop.png"))
(define w-king (bitmap "pieces/w-king.png"))
(define w-queen (bitmap "pieces/w-queen.png"))

(define b-pawn (bitmap "pieces/b-pawn.png"))
(define b-rook (bitmap "pieces/b-rook.png"))
(define b-knight (bitmap "pieces/b-knight.png"))
(define b-bishop (bitmap "pieces/b-bishop.png"))
(define b-king (bitmap "pieces/b-king.png"))
(define b-queen (bitmap "pieces/b-queen.png"))

(define w-sq (square 100 'solid (color 0 0 0 10)))
(define b-sq (square 100 'solid (color 0 0 0 190)))

(define row1 (beside b-sq w-sq b-sq w-sq b-sq w-sq b-sq w-sq))
(define row2 (beside w-sq b-sq w-sq b-sq w-sq b-sq w-sq b-sq))

(define PLAIN-BOARD (above row1 row2 row1 row2 row1 row2 row1 row2))

(define piece-list
  (list
   (cons (text "      GENERAL" 80 'white)
         (text "\n\n\nThe game can be played by mouse clicks\n
Click once to select any piece\n
If its your move then the piece will be highlighted\n
Along with possible moves\n
Otherwise it will display NOT YOUR MOVE\n
In 1 Player mode press Enter after your move\n
Wait for some time for comp to make its move\n
Its move will also be highlighted" 20 'white))
   (cons (above w-pawn (text "PAWN" 60 'white))
         (text "Moves 1 block in forward direction
or 2 blocks in first move
Kills at 1 block diagonally in foraward direction" 30 'white))
   (cons (above w-rook (text "ROOK" 60 'white))
         (text "The Rook can move any number of
squares in horizontal or vertical direction" 30 'white))
   (cons (above w-knight (text "KNIGHT" 60 'white))
         (text "The Knight's motion is unusual: it makes
a double move with single straight and single diagonal
move.This L-shaped move can be in any direction
giving 8 possible moves when knight is near centre
Knights can jump over pieces of same or opposite colour" 25 'white))
   (cons (above w-bishop (text "BISHOP" 60 'white))
         (text "The Bishop can move any number of 
squares in a direction and captures diagonally.
unlike Knight, it cannot jump over pieces." 30 'white))
   (cons (above w-queen (text "QUEEN" 60 'white))
         (text "The Queen has the abillity to move in a horizontal
,vertical or diagonal path across any number of spaces as 
long as she is not blocked.The Queen therefore moves like 
Rook and Bishop combined.Hence,the Queen is a
formidable piece." 30 'white))
   (cons (above w-king (text "KING" 60 'white))
         (text "The King is the main object of your opponent's
attacks.Once checkmated,the game is over!.It can move in all
8 possible straight and diagonal directions, but only one
square at a time." 30 'white))
   ))


;; creating chessboard from State information

(define (make-block n)
  (cond [(or (= n -9) (= n -10)) b-rook]
        [(or (= n -11) (= n -12)) b-knight]
        [(or (= n -13) (= n -14)) b-bishop]
        [(= n -16) b-king]
        [(= n -15) b-queen]
        [(and (< n 0) (> n -9)) b-pawn]
        [(or (= n 9) (= n 10)) w-rook]
        [(or (= n 11) (= n 12)) w-knight]
        [(or (= n 13) (= n 14)) w-bishop]
        [(= n 16) w-king]
        [(= n 15) w-queen]
        [(and (> n 0) (< n 9)) w-pawn]
        [(= n 0) (text "" 80 'grey)]))


(define (board-maker STATE)
  (define (helper1 ly board)
    (define (helper2 lx)
      (cond [(not (null? lx)) (begin (set! board
                              (place-image (make-block (get-piece (+ 1 (car lx)) (+ 1 (car ly)) (State-board STATE)))
                                           (+ 50 (* (car lx) 100)) (+ 50 (* (car ly) 100))
                                           
                                           (place-image (cond [(and (check? STATE) ;highlighting check king
                                                               (equal? (cons (+ 1 (car lx)) (+ 1 (car ly)))
                                                                       (check? STATE)))
                                                               (square 100 'solid (color 255 0 0 120))]
                                                              [(and (State-piece-selected? STATE) ;highlighting selected
                                                                    (equal? (State-piece-selected? STATE)
                                                                            (cons (+ 1 (car lx)) (+ 1 (car ly)))))
                                                               (square 100 'solid (color 255 255 0 100))]
                                                              [(and (State-piece-selected? STATE) ;possible moves
                                                                    (valid-move? (car (State-piece-selected? STATE))
                                                                                 (cdr (State-piece-selected? STATE))
                                                                                 (+ 1 (car lx)) (+ 1 (car ly))
                                                                                 (State-board STATE)))
                                                               (square 100 'solid (color 50 255 50 100))] ;last comp move
                                                              [(or (and (= xi-comp (+ 1 (car lx))) (= yi-comp (+ 1 (car ly))))
                                                                   (and (= xf-comp (+ 1 (car lx))) (= yf-comp (+ 1 (car ly)))))
                                                               (square 100 'solid (color 255 255 0 100))]
                                                              [else (text "" 80 'grey)])
                                                        
                                                        (+ 50 (* (car lx) 100)) (+ 50 (* (car ly) 100))
                                           board))) (helper2 (cdr lx)))])) ;helper2 ends
    
    (cond [(null? ly) board]
          [else (begin (helper2 (build-list 8 values)) (helper1 (cdr ly) board))])) ;helper1 ends
  
  (cond [(check-mate STATE) (cond [(= 1 whose-move) (place-image (place-image (text "BLACK WON" '80 'black) ;end screen
                                                                 400 400 (square 800 'solid (color 255 255 255 170)))
                                                    400 400 (helper1 (build-list 8 values) PLAIN-BOARD))]
                                  [else (place-image (place-image (text "WHITE WON" '80 'black)
                                                                  400 400 (square 800 'solid (color 255 255 255 170)))
                                                 400 400 (helper1 (build-list 8 values) PLAIN-BOARD))])]
        
        [(equal? "instructions" (State-game-started? STATE)) (place-image (place-image (cdar piece-list) 400 400
                                                                           (place-image (caar piece-list) 150 150
                                                                           (square 800 'solid (color 0 0 0 220)))) 400 400
                                                                          (helper1 (build-list 8 values) PLAIN-BOARD))]
        
        [(State-game-started? STATE) (helper1 (build-list 8 values) PLAIN-BOARD)] ;;gameplay
        
        [1p (place-image (place-image (above (place-image (text "BEGINNER" 50 'black) 150 50
                                                          (rectangle 300 100 'solid (color 255 0 0 210)))
                                             (place-image (text "ADVANCED" 50 'black) 150 50
                                                          (rectangle 300 100 'solid (color 255 0 0 210))))
                                      400 400 (square 800 'solid (color 255 255 255 220)))
                           400 400 (helper1 (build-list 8 values) PLAIN-BOARD))]
        
        [else (place-image (place-image (place-image (text "i" 40 'black) 25 25 ;start screen
                                                     (circle 25 'solid (color 255 0 0 210))) 750 50
                                         (place-image (text "CHESS PRO" 100 'black) 400 200
                                          (place-image (above (place-image (text "1 Player" '80 'black) 150 50
                                                            (rectangle 300 100 'solid (color 255 0 0 210)))
                                               (place-image (text "2 Player" '80 'black) 150 50
                                                            (rectangle 300 100 'solid (color 255 0 0 210))))
                                        400 400 (square 800 'solid (color 255 255 255 220)))))
                           400 400 (helper1 (build-list 8 values) PLAIN-BOARD))]))


(define new 0) ;val of piece killed
;; undo is by pressing u key

(define (Undo STATE key)
  (cond [(and (not (State-game-started? STATE)) 1p (equal? key "escape"))
         (begin (set! 1p #f) STATE)]
        [(equal? (State-game-started? STATE) "instructions") (cond [(equal? key "escape") (begin
                                                                    (set-State-game-started?! STATE #f)
                                                                                           STATE)]
                                                                   [(equal? key "right") (begin
                                                                    (set! piece-list (append (cdr piece-list)
                                                                                     (list (car piece-list))))
                                                                                           STATE)]
                                                                   [(equal? key "left") (begin
                                                                    (set! piece-list (cons (car (reverse piece-list))
                                                                                     (reverse (cdr (reverse piece-list)))))
                                                                                         STATE)]
                                                                   [else STATE])] ;;instructions navigation
        
        [(equal? key "u") (begin (displayln "undo")
                            (set! previous-move whose-move)
                            (set! whose-move (- 0 whose-move))
                            (set-State-piece-selected?! STATE #f)
                            (set! game-moves (cdr game-moves))
                            (set-State-board! STATE (car game-moves))
                            (set! xi-comp 9)
                            (set! yi-comp 9)
                            (set! xf-comp 9)
                            (set! yf-comp 9)
                            STATE)]
        
        [(and (equal? "1p" (State-game-started? STATE)) (equal? key "\r"))
         (cond [(< whose-move 0) (begin (displayln "done")
                                        (set! l (bestmove (State-board STATE)))
                                        (set! xi-comp (car l))
                                        (set! yi-comp (cadr l))
                                        (set! xf-comp (caddr l))
                                        (set! yf-comp (cadddr l))
                                        (set! new (get-piece xf-comp yf-comp (State-board STATE)))
                                        (if (> new 0) (displayln "YOU LOST A PIECE") (set! new 0))
                                        (set-State-board! STATE (make-move2 l (my-vector-copy (State-board STATE))))
                                        (set! game-moves (cons (State-board STATE) game-moves))
                                        (set! previous-move whose-move)
                                        (set! whose-move (- 0 whose-move))
                                        (displayln "your move")
                                        STATE)]
               [else (begin (displayln "make a move") STATE)])]
        
        [else STATE]))


;; initial state

(struct State (game-started? piece-selected? board) #:mutable #:transparent) ;;piece-selected is either false 
                                                                             ;;or a cons pair of coordinates
(define INITIAL-STATE (State #f #f chessboard))

(htdp:big-bang INITIAL-STATE
  (htdp:name "CHESS")
  (htdp:to-draw board-maker)
  (htdp:on-mouse mouse-handler)
  (htdp:on-key Undo))