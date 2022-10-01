#lang racket
(require "macros.rkt")
(require "Rules-v6.rkt")
(provide (all-defined-out))
(define computer-move #t)
(define i 0)
(define j 0)
;;QRnbPQRnbP
;;0123456789

(define hash-t (make-hash (list (list 15 9) (list 9 5) (list 10 5) (list 11 3) (list 12 3)  (list 13 3) (list 14 3)
                                (list 1 1) (list 2 1) (list 3 1) (list 4 1) (list 5 1) (list 6 1) (list 7 1) (list 8 1)
                                (list -15 -9) (list -10 -5) (list -9 -5) (list -14 -3) (list -13 -3) (list -11 -3) 
                                (list -12 -3) (list -1 -1) (list -2 -1) (list -3 -1) (list -4 -1) (list -5 -1)
                                (list -6 -1) (list -7 -1) (list -8 -1))))

(define queenboard
  (vector (vector -20 -10 -10  -5  -5 -10 -10 -20)
          (vector -10  0  0  0  0  0  0 -10)
          (vector -10  0  5  5  5  5  0 -10)
          (vector -5  0  5  5  5  5  0 -5)
          ( vector 0  0  5  5  5  5  0 -5)
          (vector -10  5  5  5  5  5  0 -10)
          (vector -10  0  5  0  0  0  0 -10)
          (vector -20 -10 -10 -5 -5 -10 -10 -20)))

(define bishopboard
  (vector (vector -20 -10 -10 -10 -10 -10 -10 -20)
          (vector -10  0  0  0  0  0  0 -10)
          (vector -10  0  5 10 10  5  0 -10)
          (vector -10  5  5 10 10  5  5 -10)
          (vector -10  0 10 10 10 10  0 -10)
          (vector -10 10 10 10 10 10 10 -10)
          (vector -10 5  0  0  0  0  5 -10)
          (vector -20 -10 -10 -10 -10 -10 -10 -20)))

(define knightboard
  (vector (vector -50 -40 -30 -30 -30 -30 -40 -50)
          (vector -40 -20  0  0  0  0 -20 -40)
          (vector -30  0 10 15 15 10  0 -30)
          (vector -30  5 15 20 20 15  5 -30)
          (vector -30  0 15 20 20 15  0 -30)
          (vector -30  5 10 15 15 10  5 -30)
          (vector -40 -20  0   5  5  0 -20 -40) 
          (vector -50 -40 -30 -30 -30 -30 -40 -50)))

(define rookboard
  (vector (vector 0  0  0  0  0  0  0  0)
          (vector 5 10 10 10 10 10 10  5)
          (vector -5  0  0  0  0  0  0 -5)
          (vector -5  0  0  0  0  0  0 -5)
          (vector -5  0  0  0  0  0  0 -5)
          (vector -5  0  0  0  0  0  0 -5)
          (vector -5  0  0  0  0  0  0 -5)
          (vector 0  0  0  5  5  0  0  0)))

(define pawnboard
  (vector (vector 0  0  0  0  0  0  0  0)
          (vector 50 50 50 50 50 50 50 50)
          (vector 10 10 20 30 30 20 10 10) 
          (vector 5  5 10 25 25 10  5 5)
          (vector 0  0  0 20 20  0  0 0)
          (vector 5 -5 -10  0  0 -10 -5  5)
          (vector 5 10 10 -20 -20 10  10  5)
          (vector 0  0  0  0  0  0  0  0)))



(define (getpiece i j val)
  (cond [(= val 15) (get-piece i j queenboard)]
        [(or (= val 13) (= val 14)) (get-piece i j bishopboard)]
        [(or (= val 11) (= val 12)) (get-piece i j knightboard)]
        [(or (= val 10) (= val 9)) (get-piece i j rookboard)]
        [(or (= val 1) (= val 2) (= val 3) (= val 4) (= val 5) (= val 6) (= val 7) (= val 8)) (get-piece i j pawnboard)]
        [(or (= val 16) (= val -16)) 0]
        [(= val -15)  (-  (get-piece i (- 9 j) queenboard))]
        [(or (= val -13) (= val -14)) (- (get-piece i (- 9 j) bishopboard))]
        [(or (= val -11) (= val -12)) (- (get-piece i (- 9 j) knightboard))]
        [(or (= val -10) (= val -9)) (- (get-piece i (- 9 j) rookboard))]
        [(or (= val -1) (= val -2) (= val -3) (= val -4) (= val -5) (= val -6) (= val -7) (= val -8)) (- (get-piece i (- 9 j) pawnboard))]))
        
       
(define (evaluation chessboard)
  (define score 0)
  (define j 1)
  (define i 1)
  (cond [(equal? computer-move #t)
         (cond [(checkmate? -16 chessboard) -1000]
               [(stalemate? -16 chessboard) 0]
               [else (begin (while (< j 9) :
                                   (begin (while (< i 9) : (begin (let ([piece (get-piece i j chessboard)])
                                                                    (cond [(not (or (= 0 piece) (= (abs piece ) 16)))
                                                                           (set! score (- score (/ (getpiece i j piece) 100) (car (hash-ref hash-t piece))  ))]))
                                                                  (set! i (+ i 1))))
                                          (set! i 1)
                                          (set! j (+ j 1))))
                            score)])]
        [else
         (cond[(checkmate? 16 chessboard) 1000]
              [(stalemate? 16 chessboard ) 0 ]
              [else (begin (while (< j 9) :
                                   (begin (while (< i 9) : (begin (let ([piece (get-piece i j chessboard)])
                                                                    (cond [(not (or (= 0 piece) (= (abs piece ) 16)))
                                                                           (set! score (- score (/ (getpiece i j piece) 100) (car (hash-ref hash-t piece))))]))
                                                                  (set! i (+ i 1))))
                                          (set! i 1)
                                          (set! j (+ j 1))))
                            score)])]))

(define bestw -10000);;Arbitary large number
(define bestmovew 0)
(define final-depth 1)
(define val 0)
(define count 0)
(define (set-depth n)
  (set! final-depth n))

(define (update score move chessboard)
  (match move
    [(list xi yi xf yf)
     (let ([new-board (make-move2 move (my-vector-copy chessboard))])
       (cond [computer-move
              (let ([move-lst (list-of-moves-global -1 new-board)]  ;;There is a huge error.<<<===
                    [kingb-check? (under-check? -16 chessboard)])
                (cond [(and (null? move-lst) kingb-check?) -1000]
                      [(null? move-lst) 0]
                      [else (let* ([piece (get-piece xf yf chessboard)]
                                  [moved-piece (get-piece xi yi chessboard)]
                                  [vi (/ (getpiece xi yi moved-piece)100)]
                                  [vf (/ (getpiece xf yf moved-piece) 100)])
                                        (if (= piece 0) (- (+ score vi) vf)
                                            (+ score
                                     (car (hash-ref hash-t piece))
                                     (/ (getpiece xf yf piece) 100)
                                     (- vi vf))))]))]
                                            
             [else
              (let ([move-lst (list-of-moves-global 1 new-board)]
                    [kingw-check? (under-check? 16 chessboard)])
                (cond [(and (null? move-lst) kingw-check?) 1000]
                      [(null? move-lst) 0]
                      [else (let* ([piece (get-piece xf yf chessboard)]
                                  [moved-piece (get-piece xi yi chessboard)]
                                  [vi (/ (getpiece xi yi moved-piece) 100)]
                                  [vf (/ (getpiece xf yf moved-piece) 100)])
                              (if (= piece 0) (- (+ score vi) vf)
                                  (+ score
                                     (car (hash-ref hash-t piece))
                                     (/ (getpiece xf yf piece) 100)
                                     (- vi vf))))]))]))]))
                                  


(define (minimax chessboard depth maximising-player alpha beta curr_score) ;; Still confusion in state of move.
  (cond[(= curr_score 1000) 1000]
       [(= curr_score -1000) -1000]
       [else
        (cond [(equal? maximising-player #t) (let ([best -1000]
                                      [listb (list-of-moves-global -1 chessboard)]) ;;One nasty func
                                  (cond [(= depth final-depth) (begin (set! computer-move #t)
                                                                      curr_score)];;applying while loop
                                        [else (begin (while (and (not (null? listb)) (>= beta alpha)) :
                                                            (begin  (set! val (minimax (make-move2
                                                                                        (car listb)
                                                                                        (my-vector-copy chessboard))
                                                                                       (+ depth 1) #f alpha beta
                                                                                       (update curr_score (car listb) chessboard)))
                                                                    (set! listb (cdr listb))
                                                                    (set! best (max best val))
                                                                    (set! alpha (max alpha best))))
                                                     best)]))]
              
              
              [else (let ([best 1000]
                          [listw (list-of-moves-global 1 chessboard)])
                      (cond[(= depth final-depth) (begin (set! computer-move #f)
                                                         curr_score)]
                           [else (begin (while (and (not (null? listw)) (>= beta alpha)) :
                                               (begin  (set! val (minimax (make-move2
                                                                           (car listw)
                                                                           (my-vector-copy chessboard))
                                                                          (+ depth 1) #t alpha beta
                                                                          (update curr_score (car listw) chessboard)))
                                                       (set! listw (cdr listw))
                                                       (set! best (min best val))
                                                       (set! alpha (min alpha best))))
                                        best)]))])]));;applying while loop




(define bestval -10000)

(define (random-move lst)
  (let ([num (random (length lst))])
    (list-ref lst num)))

(define (bestmove chessboard) ;;Uses minimax for all valid moves to find bestmove.
  (let* ([listb (list-of-moves-global -1 chessboard)]
         [thebest (car listb)]
         [listb1 listb]
         [best-moves-list (list (car listb))])
    (define (helper-bestmove board lst)
      (cond
           [(null? lst) (begin (displayln best-moves-list)
                               (random-move best-moves-list))] ;;Playing the move;;thebest is the best move.
           [else (let* ([new-board (make-move2 (car lst) (my-vector-copy board))]
                        [res (minimax new-board 0 #f -1000 1000 (evaluation new-board))])
                   (cond [(> res bestval)
                          (begin (set! thebest (car lst))
                                 (set! best-moves-list (list (car lst)))
                                 (set! bestval res)
                                 (helper-bestmove board (cdr lst)))];;will apply let later
                         [(= res bestval)
                          (begin (set! best-moves-list (append (list (car lst)) best-moves-list))
                                 (helper-bestmove board (cdr lst)))]
                         [else (helper-bestmove board (cdr lst))]))]))

        (let ([res (helper-bestmove chessboard listb)])
      (begin (set! bestval -10000)
             res))))

;(define (bestmove chessboard) ;;Uses minimax for all valid moves to find bestmove.
;  (let* ([listb (list-of-moves-global -1 chessboard)]
;         [thebest (car listb)])
;    (define (helper-bestmove board lst)
;      (cond [(null? lst) (begin (displayln thebest) thebest)]
;            [else (let* ([new-board (make-move2 (car lst) (my-vector-copy board))]
;                         [res (minimax new-board 0 #f -1000 1000 (evaluation new-board))])
;                    (cond [(>= res bestval)
;                            (begin (set! thebest (car lst))
;                                   (set! bestval res)
;                                   (helper-bestmove board (cdr lst)))]
;                          [else (helper-bestmove board (cdr lst))]))]))
;    (let ([res (helper-bestmove chessboard listb)])
;      (begin (set! bestval 0)
;             res))))
         
     
