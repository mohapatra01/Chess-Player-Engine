#lang racket
(provide (all-defined-out))
(struct node(t1 t2) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax list-of-three
  (syntax-rules (@ <-)
    [(list-of-three b @ c ... <- d) `(b d c ...)]))

;(define x (list-of-three  7 @ 1 2 3 4 5 <- 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax mycond
  (syntax-rules (> <)
    [(mycond < bexp exp > more ...) (if bexp exp (mycond more ...))]
    [(mycond < exp >) exp]))                                 

(define (fact n)
  (mycond < (= n 0) 1 >
          < (* n (fact (- n 1))) >))


(define (fib n)
  (mycond < (= n 0) 0 >
          < (= n 1) 1 >
          < (+ (fib (- n 1)) (fib (- n 2))) >))
   
;(fib 10)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Expermenting with the expansion of lc. Play with it till you
;understand it fully.

;(lc (+ x y) : x <- '(1 2 3 4 5 6) @(even? x)  y <- '( 7 8 9))


; (append* (lc
;           (lc (+ x y) : @(even? x) y <- '(7 8 9))
;           : x <- '(1 2 3 4 5 6)))

; (append* (lc
;   (append* (lc (lc (begin (displayln (+ x y)) (+ x y)) : y <- '(7 8 9)) : @(even? x)))
;   : x <- '(1 2 3 4 5 6)))


; (append* (lc
;   (if (even? x) (lc (+ x y) : y <- '(7 8 9)) '())
;   : x <- '(1 2 3 4 5 6)))  
          
; (append* (lc
;   (if (even? x) (map (lambda (y) (+ x y)) '(7 8 9)) '())
;   : x <- '(1 2 3 4 5 6))) 

; (append* (map (lambda (x)
;                 (if (even? x) (map (lambda (y) (+ x y)) '(7 8 9)) '()))
;               '(1 2 3 4 5 6)))

;(lc (node x y) : x <- '(t1 t2 t3)
;                 y <- '(s1 s2))

(define (cprod l)
  (cond [(null? l) '(())]
        [else
         (lc (cons x y) :
             x <- (car l)
             y <- (cprod (cdr l)))]))

(define (qsort l)
  (cond [(null? l) '()]
        [else (let ((lows (lc x : x <- (cdr l) @(<= x (car l))))
                    (highs (lc x : x <- (cdr l) @(> x (car l)))))
                (append (qsort lows) (list (car l))
                        (qsort highs)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax for
  (syntax-rules (:)
    [(for init : condition : step : statements)
       (begin
         init
         (define (iter)
           (cond [condition (begin statements
                                   step
                                   (iter))]))
         (iter))]))

(define i 0)
(define sum 0)
(for (begin
       (set! i 0)
       (set! sum 0)) :
  (< i 10) :
  (set! i (+ i 1)) :
  (set! sum (+ sum i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin
  (begin
    (define x 0)
    (define sum1 0))
  (define (iter)
    (cond [(< x 10) (begin
                      (set! sum1 (+ sum1 x))
                      (set! x (+ x 1))
                      (iter))]))
  (iter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Write a  macro  to implement  C-style  while loops  in drracket.  In
;general, the syntax of while is:
;
;   (while {boolean-expression} {one-or-more-statements})
(define-syntax while
  (syntax-rules (:)
    [(while bexp : statements )
     (begin
       (define (iter)
         (cond [bexp (begin statements
                            (iter))])) 
       (iter))]))
(define k 1)
(define product 1)
(while (<= k 10) : (begin (set! product (* product k))
                          (set! k (+ k 1)))) 





;The example below illustrates the use of the while macro:
;
    (define j 10)
    (define z 0)
    (while (> j 0) :
       (begin (set! z (+ z j))
              (set! j (- j 1))))



  