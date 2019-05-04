

#lang racket

;; You can require more modules of your choice.
(require racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt"))
(require "list-comprehension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; Strategies                                                                          ;;
;; ==========                                                                          ;;
;; For the purpose of this assignment, just the `etai` strategy is expected, since     ;;
;; we have dictionary-closure and secret-word-enumeration to leap-frog to the right    ;;
;; key. This technique would fail for harder keys which are arbitrary permutations of  ;;
;; the alphabet. We will be forced to explore many more strategies (along with         ;;
;; dictionary-closure of course).                                                      ;;
;;                                                                                     ;;
;; Strategies to guess substitutions for the key using statistical information about   ;;
;; - the English language from utils.rkt                                               ;;
;; - the cipher text      from statistics.rkt                                          ;;
;;                                                                                     ;;
;; Follow the function signature as indicated below. Deviations will make it           ;;
;; impossible for automatic grading of your submission.                                ;;
;; Moreover, we do not expect your strategies to require any more/different            ;;
;; arguments. Note that you recieve the key as argument, so you can at the very        ;;
;; least ensure that all the substitutions are monoalphabetic wrt this key.            ;;
;;                                                                                     ;;
;; Signature:                                                                          ;;
;; ```                                                                                 ;;
;; (define (my-fundoo-strategy key)                                                    ;;
;;   ;; Make use of `utils:ciphertext`, `utils:cipher-word-list`                       ;;
;;   ...)                                                                              ;;
;; ```                                                                                 ;;
;;                                                                                     ;;
;; Substitutions                                                                       ;;
;; -------------                                                                       ;;
;; In order to extend the key incrementally, we use `utils:add-substitution` to        ;;
;; extend a given key with a substitution.                                             ;;
;;                                                                                     ;;
;; A substitution is a list of pairs, each pair mapping a plaintext char to a          ;;
;; ciphertext char. For example, to extend the key with T -> a and O -> r              ;;
;; (simultaneously), we use the substitution:                                          ;;
;; ```                                                                                 ;;
;; (list (cons #\T #\a) (cons #\O #\r))                                                ;;
;; ```                                                                                 ;;
;; For a single substitution use a singleton list (containing just one pair).          ;;
;;                                                                                     ;;
;; **CAUTION**                                                                         ;;
;; -----------                                                                         ;;
;; 1. Note that add-substitution does not do sanity checks on the substitution and use ;;
;;    of `utils:is-monoalphabetic` is recommended to ensure that you don't             ;;
;;    inadvertently create invalid keys.                                               ;;
;; 2. You must provide a list called `compositions` in this module.                    ;;
;;                                                                                     ;;
;; See docs in "utils.rkt" for more information.                                       ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You must add "public" functions of this module to this list.
(provide etai
         ;; Some more suggested strategies:
         
          ;;common-words-double
         ;; bigrams
         ;; common-initial-letters
         ;; common-final-letters
         ;; common-words-triple
         ;; trigrams
         ;; common-double-letters
         ;; common-words-quadruple
         ;; quadgrams
         
         ;; lists of strategies
         composition)

;; A strategy that uses some statistical information to generate potential
;; substitutions for E, T, A and I.
;; Refer the assignment manual for tips on developing this strategy. You can
;; interact with our etai with the executable we provide.
(define (monograms-sequence)
  (take (stats:cipher-monograms utils:ciphertext) 5))

(define (single-words)
  (string-to-char (stats:cipher-common-words-single utils:cipher-word-list)))
(define (string-to-char list-string)
  (cond [(null? list-string) '()]
        [else (cons (car (string->list (car list-string))) (string-to-char (cdr list-string)))]))

(define (count)
  (search-list-list (single-words) (monograms-sequence)))
(define (search-list-list l1 l2)
  (cond [(null? l1) '()]
        [else (append (search-element (car l1) l2) (search-list-list (cdr l1) l2))]))
(define (search-element element list)
  (cond [(null? list) '()]
        [(equal? element (car list)) (cons (car list) '())]
        [else (search-element element (cdr list))]))

(define (neighbourhood)
  (rearrangement (remove* (count) (reverse-ordering (stats:cipher-unique-neighbourhood (stats:cipher-bigrams utils:cipher-word-list) 'both) (monograms-sequence)))))

(define (rearrangement lst)
  (cons (car lst) (cons (last lst) (remove* (list (last lst)) (cdr lst)))))
(define (reverse-ordering l1 l2)
  (reverse (ordering l1 l2)))

(define (ordering l1 l2)
  (cond [(null? l1) '()]
        [(= (length (indexes-of l2 (car (car l1)))) 1) (cons (car (car l1)) (ordering (cdr l1) l2))]
        [else (ordering (cdr l1) l2)]))

(define (zip l1 l2)
  (cond [(or (null? l1) (null? l2)) '()]
        [else (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))]))

(define (etai key)
  (cond [(= (length (single-words)) 2) (append* (map (lambda (x y) (list x y)) (lc (append x (zip '(#\A #\I) (single-words))) : x <- (combsoptions2 '(#\T #\E) (neighbourhood)))
                                               (lc (append x (zip '(#\A #\I) (reverse (single-words)))) : x <- (combsoptions2 '(#\T #\E) (neighbourhood)))))]
        [(= (length (single-words)) 1) (append (lc (append x (zip '(#\A) (single-words))) : x <- (combsoptions3 '(#\T #\E #\I) (neighbourhood)))
                                               (lc (append x (zip '(#\I) (single-words))) : x <- (combsoptions3 '(#\T #\E #\A) (neighbourhood))))]
        [else (lc x : x <- (combsoptions4 '(#\T #\E #\A #\I) (neighbourhood)))]))

(define (combsoptions2 list-subs list-to)
  (lc (list (cons #\T x) (cons #\E y)) : x <- list-to  y <- (remove* (list x) list-to)))

(define (combsoptions3 list-subs list-to)
  (lc (list (cons #\T x) (cons #\E y) (cons #\I z)) : x <- list-to  y <- (remove* (list x) list-to) z <- (remove* (list x y) list-to)))

(define (combsoptions4 list-subs list-to)
  (lc (list (cons #\T x) (cons #\E y) (cons #\A z) (cons #\I a)) :
      x <- list-to  y <- (remove* (list x) list-to) z <- (remove* (list x y) list-to) a <- (remove* (list x y z) list-to)))


(define (zip-words word1 word2)
  (zip (string->list word1) (string->list word2)))

(define (trigrams nullifykey)
  (append* (map (lambda (x) (map (lambda (y) (zip-words x y)) (take (stats:cipher-trigrams utils:cipher-word-list) 5))) (take utils:plain-trigrams 3))))

(define (quadgrams nullifykey)
  (append* (map (lambda (x) (map (lambda (y) (zip-words x y)) (take (stats:cipher-quadgrams utils:cipher-word-list) 5))) (take utils:plain-quadgrams 3))))

(define (common-words-triple nullifykey)
  (append* (map (lambda (x) (map (lambda (y) (zip-words x y)) (take (stats:cipher-common-words-triple utils:cipher-word-list) 5))) (take utils:plain-common-words-triple 3))))

(define (common-words-double nullifykey)
  (append* (map (lambda (x) (map (lambda (y) (zip-words x y)) (take (stats:cipher-common-words-double utils:cipher-word-list) 5))) (take utils:plain-common-words-double 3))))

;(define (common-words-quadruple nullifykey)
;  (append* (map (lambda (x) (map (lambda (y) (zip-words x y)) (take (stats:cipher-common-words-quadruple utils:cipher-word-list) 5))) (take utils:plain-common-words-quadruple 3))))
;; A suggested composition of strategies that might work well. Has not been
;; exhaustively tested by us. Be original ;)
(define composition (list
                   etai
                  ;; common-words-double
                  ;; bigrams
                  ;; common-initial-letters
                  ;; common-final-letters
                  ;; common-words-triple
                  ;; trigrams
                  ;; common-double-letters
                  ;; common-words-quadruple
                  ;; quadgrams
                  ))
