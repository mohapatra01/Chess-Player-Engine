#lang racket
(provide (all-defined-out))
(require "macros.rkt")
(require racket/vector)
;;numbers from 1 to 8 are designated for pawns starting from left for bottom side
;;numbers 9 and 10 for rook
;;numbers 11 and 12 for knight
;;numbers 13 and 14 for bishop
;;15 for Queen
;;16 for King
;;similar arrangement in negative numbers for opponent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;THIS PART CONTAINS SOME FUNC FOR GETTING THE PIECE ON X,Y COORDINATES OF BOARD


;;This function gives vector along movement direction.
;;which will be used further in checking in while loop.
(define (get-directions xi yi xf yf)
  (let* ([a (- xf xi)]
         [b (- yf yi)])
    (cons a b)))

;;This function checks whether whether path btw
;;init-pos and final-pos is empty or not.
;;;State represents whether path from init till now is empty or not
(define (check-empty? right up xi yi xf yf board) 
  (define state #t)            
  (let ([unit-r (if (equal? right 0) 0
                    (/ right (abs right)))]
        [unit-u (if (equal? up 0) 0
                    (/ up (abs up)))]
        [xc xi]
        [yc yi])
    (begin (while (and (equal? state #t)
                       (not (and (equal? xc (- xf unit-r))
                                 (equal? yc (- yf unit-u))))) : (begin (set! xc (+ xc unit-r))
                                                                       (set! yc (+ yc unit-u))
                                                                       (cond [(not (empty? xc yc board)) (set! state #f)])))
           state))) 

;;;Checks whether own piece is not attckd by self
(define (same-parity? xi yi xf yf chessboard)       
  (let ([init (get-piece xi yi chessboard)]
        [final (get-piece xf yf chessboard)])
    (or (and (> init 0) (> final 0))
        (and (< init 0) (< final 0)))))

;;;Gives the value designated to the piece at the clicked pos
 ;;;if value is zero => empty cell
(define (get-piece i j chessboard)        
 (vector-ref  (vector-ref chessboard (- j 1)) (- i 1))) 

;As the name suggsets ,checks cell is empty or not.
(define (empty? xi yi chessboard)
  (= 0 (get-piece xi yi chessboard))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Function to make a vector-copy of 2d vector
;;as direct vector-copy function applied to chessboard doesn't works.
(define (my-vector-copy board)
  (vector (vector-copy (vector-ref board 0))
          (vector-copy (vector-ref board 1))
          (vector-copy (vector-ref board 2))
          (vector-copy (vector-ref board 3))
          (vector-copy (vector-ref board 4))
          (vector-copy (vector-ref board 5))
          (vector-copy (vector-ref board 6))
          (vector-copy (vector-ref board 7))))

;;Make the corresponding changes in the chessboard for a move and returns the chessboard.
(define (make-move1 xi yi xf yf chessboard)
  (define (change x y val)
    (vector-set! (vector-ref chessboard (- y 1)) (- x 1) val))
  (let ([init (get-piece xi yi chessboard)])
    (begin (change xi yi 0)
           (change xf yf init)
           chessboard)))

;Another version of make-move1 required when a list is given instead of xi's.
(define (make-move2 pos chessboard)
  (match pos
    [(list xi yi xf yf) (make-move1 xi yi xf yf chessboard)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;STRUCT DEFINITIONS
(struct KING (move-status val Xc Yc) #:transparent #:mutable) ;;Xc-->x coordinate Yc--> y coordinate
(define king_black (KING #t -16 5 1))
(define king_white (KING #t 16 5 8))

(struct ROOK (move-status val Xc Yc) #:transparent #:mutable)
(define rook1_w (ROOK #f 9 8 8))
(define rook2_w (ROOK #f 10 1 8))
(define rook1_b (ROOK #f -9 8 1))
(define rook2_b (ROOK #f -10 1 1))



;Whites is the list of remaining pieces of player white.
(define whites (build-list 15 (lambda (x) (+ x 1))))

(define blacks (build-list 15 (lambda (x) (* -1 (+ x 1)))))

;Removes a piece from the list when the piece is lost.
(define (delete_w piece)
  (set! whites (remove piece whites)))
(define (delete_b piece)
  (set! blacks (remove piece blacks)))

(define (piece-list board)
  (define black-lst '())
  (define white-lst '())
  (define i 1)
  (define j 1)
  (while (< j 9) :
         (begin (while (< i 9) :
                       (begin (let ([piece (get-piece i j board)])
                                (cond [(> 0 piece) (set! black-lst (cons piece black-lst))]
                                      [(< 0 piece) (set! white-lst (cons piece white-lst))]))
                              (set! i (+ i 1))))
                (set! i 1)
                (set! j (+ 1 j))))
  (list black-lst white-lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Following are some functions/variables giving directions of movecment of pieces.
(define queen-direc
  (lc (cons u v) : u <- '(1 0 -1) v <- '(1 0 -1) @ (not (and (= 0 u) (= 0 v)))))
(define king-direc queen-direc)
(define rook-direc
  (lc (cons u v) : u <- '(1 0 -1) v <- '(1 0 -1) @ (and (= 0 (* u v))
                                                        (not (and (= 0 u) (= 0 v))))))
(define bishop-direc
  (lc (cons u v) : u <- '(1 -1) v <- '(1 -1)))
(define knight-direc
  (lc (cons u v) : u <- '(1 -1 2 -2) v <- '(1 -1 2 -2) @ (= 2 (abs (* u v)))))
(define wpawn-direc
  (list (cons 0 -1) (cons 0 -2) (cons 1 -1) (cons -1 -1)))
(define bpawn-direc
  (list (cons 0 1) (cons 0 2) (cons 1 1 ) (cons -1 1)))

;Gives the chessboards-of-valid moves of a pawn on chessboard.

(define (knight? val)
  (or (= (abs val) 12) (= (abs val) 11)))
(define (pawn? val)
  (and (not (= 0 (abs val))) (< (abs val) 9)))
(define (king? val)
  (or (= val 16) (= val -16))) 

;;returns direc of a particular piece.
(define (direc val)
  (let ([val1 (abs val)])
    (cond [(or (= val1 10) (= val1 9)) rook-direc]
          [(= val1 16) king-direc]
          [(= val1 15) queen-direc]
          [(or (= val1 13) (= val1 14)) bishop-direc]
          [(or (= val1 12) (= val1 11)) knight-direc]
          [(and (< val 0) (> val -9)) bpawn-direc]
          [(< val1 9) wpawn-direc])))

;As the King, Knight and pawn have fixed length move,
;this func gives the list of possible final position
;without checking out-of-board.
(define (build-move-list direc x y)
  (map (lambda (dir) (let ([u (car dir)]
                           [v (cdr dir)])
                       (cons (+ x u) (+ y v)))) direc))

;Gives list of (list xi yi xf yf) of all valid moves of a piece.
(define (move-through val x y direc chessboard)
  (cond [(= val 0) '()]
        [(or (pawn? val) (king? val) (knight? val)) (f x y direc chessboard)]
        [else (let ([move-list (append* (map (lambda (u) (move-through-helper x y u chessboard)) direc))])
                move-list)]))

;A helper function for move through which gives list of (xi yi xf yf)
;for queen,rook,bishop.
(define (move-through-helper x y dir chessboard)
  (let* ([u (car dir)]
         [v (cdr dir)]
         [i 1]
         [move-list '()]
         [xc (+ x (* u i))]   ;;current x coordinate 
         [yc (+ y (* v i))])  ;;current y coprdinate
    (begin (while (not-out-of-board xc yc) : (begin (set! move-list (cons (list x y xc yc) move-list))
                                                    (set! xc (+ xc (* u i)))
                                                    (set! yc (+ yc (* v i)))))
           (filter (lambda (move) (valid-move2? move chessboard))
                    move-list))))

; a higher-order-function which performs some common move actions for king ,pawn,knight
;and returns list of list(xi yi xf yf) of moves
(define (f x y direc chessboard)
  (let* ([init-lst (build-move-list direc x y)]
         [filtered-lst (filter-inboards init-lst)]
         [final-lst (filter (lambda (pos) (valid-move? x y (car pos) (cdr pos) chessboard))
                            filtered-lst)]
         [res (map (lambda (pos) (list x y (car pos) (cdr pos))) final-lst)])
    res))

;;this func gives chessboards with all possible valid moves of a given piece 
(define (list-of-moves piece chessboard) 
  (let* ([pos (get-piece-pos piece chessboard)]
         [x (car pos)]
         [y (cdr pos)]
         [move-list (move-through piece x y (direc piece) chessboard)])
    (map (lambda (move) (make-move2 move (my-vector-copy chessboard))) move-list)))

;;gives the list of (list xi yi xf yf) for all pieces of a player
(define (list-of-moves-global player chessboard)
  (define i 1)
  (define j 1)
  (define res '())
  (while (< j 9) :
         (begin (while (< i 9) : (begin (let ([val (get-piece i j chessboard)])
                                          (cond [(> (* val player) 0)
                                                 (set! res (append res (move-through val i j (direc val) chessboard)))])
                                        (set! i (+ i 1)))))
                (set! i 1)
                (set! j (+ j 1))))
  res)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;THIS CONTAINS FUNCTIONS REQUIRED FOR CHECK? AT ANY TIME ON KINGS OF PLAYERS

(define (not-out-of-board xc yc)
  (and (> xc 0) (< xc 9) (> yc 0) (< yc 9)))

;;filter-out out-of-board positions
(define (filter-inboards lst)
  (filter (lambda (pos) (and (not-out-of-board (car pos) (cdr pos)))) lst))

;;First piece encountered in a particular direction of a piece.
(define (first-piece-encounter direc x y chessboard)
  (define found-till-now 0)
  (let ([u (car direc)]
        [v (cdr direc)]
        [xc x]               ;;;current x-coordinate
        [yc y])              ;;;current y-coordinate
    (cond [(not (or (= 2 (* u v)) (= -2 (* u v))))   ;;;;case other than knight because here multiple moves in a direc 
           (begin (set! xc (+ u xc))                 ;;;;have to be accompolished to check
                  (set! yc (+ v yc))
                  (while (and (= found-till-now 0) (not-out-of-board xc yc)) :
                         (begin (set! found-till-now (get-piece xc yc chessboard))
                                (set! xc (+ u xc))
                                (set! yc (+ v yc))))
                  found-till-now)] 
          [else (begin (set! xc (+ u xc)) ;;;since knight will only be at a L shape distance
                       (set! yc (+ v yc))
                       (if (not-out-of-board xc yc) (get-piece xc yc chessboard)
                           0))])))

;is queen or rook?
(define (is-q/r? a val)
  (cond [(> val 0) (or (= a -15) (= a -9) (= a -10) )]
        [else (or (= a 15) (= a 9) (= a 10))]))

;is queen or bishop?
(define (is-b/q? a val)
  (cond [(> val 0) (and (< a 0) (or (= a -15) (= a -13) (= a -14)))]
        [else (and (> a 0) (or (= a 15) (= a 13) (= a 14)))]))

(is-b/q? 7 -16)
(define (is-pawn? a val)
  (cond [(> val 0) (and (< a 0) (> a -9))]
        [else (and (> a 0) (< a 9))]))

(define (is-knight? a val)
  (cond [(> val 0) (or (= a -11) (= a -12))]
        [else (or (= a 11) (= a 12))]))

;;to check whether first pieces encountered in x or y direc
 ;;are queen/rook/king or not
(define (no-enemy-x-or-y? val x y chessboard)   
  (define list-of-dir                         
    (list (cons 1 0) (cons -1 0) (cons 0 1) (cons 0 -1)))
  (foldr (lambda (a b) (and (not(is-q/r? a val)) b)) #t
         (map (lambda (dir) (first-piece-encounter dir x y chessboard)) list-of-dir)))

;;;to check whether first pieces encountered in inclined angles path
 ;;;are bishop/queen/king/pawn of opponent or not
(define (no-enemy-xy? val x y chessboard)  
  (define list-of-dir                     
    (list (cons 1 1) (cons 1 -1) (cons -1 -1) (cons -1 1)))
  (foldr (lambda (a b) (and (not (is-b/q? a val)) b)) #t
         (map (lambda (dir) (first-piece-encounter dir x y chessboard)) list-of-dir)))

;;to check whether pos is under the attck of knight or not
(define (no-enemy-l? val x y chessboard)  
  (define list-of-dir
    (list (cons 1 2) (cons 1 -2) (cons -1 2) (cons -1 -2)
          (cons 2 1) (cons 2 -1) (cons -2 -1) (cons -2 1)))
  (foldr (lambda (a b) (and (not (is-knight? a val)) b)) #t
         (map (lambda (dir) (first-piece-encounter dir x y chessboard)) list-of-dir)))

;to check whether pos is under attck of king or not.
(define (no-enemy-king? val x y chessboard)
  (let* ([init-checks (map (lambda (dir) (cons (+ x (car dir)) (+ y (cdr dir)))) queen-direc)]
         [filter-checks (filter (lambda (pos) (not-out-of-board (car pos) (cdr pos))) init-checks)]
         [mod-lst (map (lambda (pos) (get-piece (car pos) (cdr pos) chessboard)) filter-checks)])
    (if (member (* -1 val) mod-lst) #f #t)))

;same thing for pawn.
(define (no-enemy-pawn? val x y chessboard)
  (let* ([dir (if (> val 0)
                  (list (cons 1 -1) (cons -1 -1))
                  (list (cons 1 1) (cons -1 1)))]
         [pieces (map (lambda (dir) (cons (+ x (car dir)) (+ y (cdr dir)))) dir)]
         [filtered-pieces (filter (lambda (pos) (not-out-of-board (car pos) (cdr pos))) pieces)]
         [mod-lst (map (lambda (pos) (get-piece (car pos) (cdr pos) chessboard)) filtered-pieces)])
    (foldr (lambda (a b) (and (not (is-pawn? a val)) b)) #t mod-lst)))
                 
;returns coordinates of a piece given its value and chessboard.
(define (get-piece-pos val board)
  (define i 0)
  (define found #f)
  (begin (while (equal? found #f) : (begin (let ([res (vector-member val (vector-ref board i))])
                                             (if (number? res) (set! found (cons (+ 1 res) (+ 1 i) ))
                                                 (set! i (+ 1 i))))))
         found))

;king is under check or not?

(define (under-check? king chessboard)     ;;;;Case when after moving from under-check pos
 (let* ([val king]                          ;;;; to pos where there is another king not covered yet
        [pos (get-piece-pos val chessboard)]
        [x  (car pos)]
        [y (cdr pos)])                   
   (not (begin (and (no-enemy-x-or-y? val  x y chessboard)
                    (no-enemy-xy? val x y chessboard)
                    (no-enemy-l? val x y chessboard)
                    (no-enemy-pawn? val x y chessboard))))))

;;;;;;;BEWARE , THERE IS A LOOP HOLE IN BELOW TO BE CORRECTED.
;(define (checkmate? king chessboard)
;    (cond [(not (under-check? king chessboard)) #f]
;          [else (let* ([val  king]
;                      [lst (piece-list chessboard)]
;                      [piece-lst (if (> val 0) (cadr lst) (car lst))]
;                      [king-moves (list-of-moves val chessboard)]
;                      [piece-moves (append* (map (lambda (x) (list-of-moves x chessboard)) piece-lst))]
;                      [possible_saves (append king-moves piece-moves)]
;                      [checks-after-move (begin (map (lambda (x) (under-check? king x)) possible_saves))])
;                  (foldr (lambda (x y) (and x y)) #t checks-after-move))]))

(define (checkmate? king chessboard)
  (cond [(not (under-check? king chessboard)) #f]
        [else (null? (list-of-moves-global king chessboard))]))

;;Stalemate when a player has no valid move for any of it's piece

;(define (stalemate? king chessboard)
;  (let* ([val  king]
;         [lst (piece-list chessboard)]
;         [piece-lst (if (> val 0) (cadr lst) (car lst))]
;         [king-moves (list-of-moves val chessboard)]
;         [piece-moves (append* (map (lambda (x) (list-of-moves x chessboard)) piece-lst))]
;         [all-moves (append king-moves piece-moves)])
;    (and (not (under-check? king chessboard)) (null? all-moves))))

(define (stalemate? king chessboard)
  (cond [(under-check? king chessboard) #f]
        [else (null? (list-of-moves-global king chessboard))]))
    


(define vec (vector (vector -10 -12 -14 -15 -16 -13 -11 -9)
                    (vector -1  -2  -3  -4  -5  -6  -7  -8)                              
                    (vector  0   0   0   0   0   0   0   0)
                    (vector  0   0   0   0   0   0   0   0)
                    (vector  0   0   0   0   0   0   0   0)
                    (vector  1   0   0   0   0   0   0   0)
                    (vector  0   2   3   4   5   6   7   8)
                    (vector  10  12  14  15  16  13  11  9)))

(no-enemy-xy? -16 6 2  vec)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;THIS IS THE HEART OF RULES.RKT,TELLS WHETHER A MOVE MADE BY PLAYER IS VALID OR NOT

(define (valid-move2? pos chessboard)
  (match pos
    [(list xi yi xf yf) (valid-move? xi yi xf yf chessboard)]))
  
(define (valid-move? xi yi xf yf chessboard)
  (define (valid-move-helper) 
    (cond [(and (= yi yf) (= xi xf)) #t]    ;; Technically no move at all
          [(same-parity? xi yi xf yf chessboard) #f]   ;;so that it doesn't attcks its own kind                        
          [else (let* ([direc (get-directions xi yi xf yf)]
                       [right (car direc)]
                       [up (cdr direc)]
                       [val1 (get-piece xi yi chessboard)]
                       [val (abs val1)]
                       [final_vac (= 0 (get-piece xf yf chessboard))])
                  (define (checker)
                    (check-empty? right up xi yi xf yf chessboard))
                  (cond [(< val 9)
                         (cond[(> val1 0) (cond [(and (= 0 right) (= yi 7))   ;;if pawn hasn't moved earlier
                                                 (let ([one_forward (empty? xi 6 chessboard)])
                                                   (cond [(= yf 5) (and one_forward final_vac)] 
                                                         [(= yf 6) one_forward]
                                                         [else #f]))]
                                                [(and (= 0 right) (= -1 up)) final_vac]
                                                [(and (= 1 (abs right)) (= -1 up)) (not final_vac)] ;;Capturing
                                                [else #f])]
                              [else (cond [(and (= 0 right) (= yi 2))
                                           (let ([one_forward (empty? xi 3 chessboard)])
                                             (cond [(= yf 4) (and one_forward final_vac)]
                                                   [(= yf 3) one_forward]
                                                   [else #f]))]
                                          [(and (= 0 right) (= 1 up)) final_vac]
                                          [(and (= 1 (abs right)) (= 1 up)) (not final_vac)]
                                          [else #f])])]  
                        [(or (= val 9) (= val 10)) (begin 
                                                          (cond [(not (or (= right 0) (=  0 up))) #f] ;;Directions for Rook
                                                                [else (checker)]))]
                        [(or (= val 11) (= val 12)) (or (and (= 1 (abs right))  ;;Directions for knight 
                                                             (= 2 (abs up)))
                                                        (and (= 2 (abs right))
                                                             (= 1 (abs up))))]
                        [(or (= val 13) (= val 14)) (cond [(not (equal? (abs up) (abs right))) #f]  ;;Directions for Bishop
                                                          [else (checker)])]
                        [(= val 15) (cond [(not (or (or (= up 0) (= right 0))  ;;Directions for Queen
                                                    (= (abs up) (abs right)))) #f]
                                          [else (checker)])]
                        [(= val 16) (cond [(not(or (and (= 1 (abs up)) (= 1 (abs right)))      ;;Directions for King  
                                                   (and (or (= 1 (abs up)) (= 1 (abs right)))  ;;Castling hasn't been enabled yet
                                                        (or (= 0 (abs up)) (= 0 (abs right)))))) #f]
                                          [else (checker)])]))]))
  (let* ([piece (get-piece xi yi chessboard)]
         [king (if (> piece 0) 16 -16)])
    (if (valid-move-helper) (begin 
                                   (not (under-check? king (make-move1 xi yi xf yf (my-vector-copy chessboard)))))
        #f)))
(define vec3 (vector #(-10 -12 0 -15 -16 -13 -11 -9)
                      #(-1 -2 0 0 0 0 0 0)
                      #(0 0 0 0 0 0 0 0)
                      #(0 0 0 0 0 0 -7 0)
                      #(1 0 -3 0 -14 -6 0 0)
                      #(12 2 3 0 0 0 -8 0)
                      #(0 0 0 4 0 0 0 8)
                      #(10 0 14 15 16 0 11 9)))

;(list-of-moves-global -2 vec4)
