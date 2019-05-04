
#lang racket
(provide (all-defined-out))
(struct node1(t1 t2) #:transparent)
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
  



(define-syntax list-of-three
  (syntax-rules (@ <-)
    [(list-of-three b @ c ... <- d) `(b d c ...)]))

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

;(begin
;  (begin
;    (define x 0)
;    (define sum 0))
;  (define (iteefine r)
;    (cond [(< x 10) (begin
;                      (set! sum (+ sum x))
;                      (set! x (+ x 1))
;                      (iter))]))
;  (iter))

;Write a  macro  to implement  C-style  while loops  in drracket.  In
;general, the syntax of while is:
;
;   (while {boolean-expression} {one-or-more-statements})
;
;The example below illustrates the use of the while macro:
;
;    (define i 10)
;    (define y 0)
;    (while (> i 0) 
;       (set! y (+ y i))
;       (set! i (- i 1)))
