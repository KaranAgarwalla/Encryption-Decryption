

#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         zip
         ;; any other functions of your design come below:

         ;; my-fundoo-analysis
         )

;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.
(define (cipher-monograms ciphertext)
  (define l2 (string->list "abcdefghijklmnopqrstuvwxyz"))
  (getfirst (sort-by-frequency (zip l2 (cipher-helper ciphertext)))))

(define (sort-by-frequency al)
  (sort al (lambda (x y) (> (cdr x) (cdr y)))))
  
(define (zip l1 l2)
  (cond [(or (null? l1) (null? l2)) '()]
        [else (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))]))

(define (cipher-helper l)
  (cond [(= (string-length l) 0) (make-list 26 0)]
        [else (cond 
                     [ (and (<= (char->integer (string-ref l 0)) 122) (>= (char->integer (string-ref l 0)) 97))
                      (list-update (cipher-helper (substring l 1)) (- (char->integer (string-ref l 0)) 97) (lambda (x) (+ x 1)))]
                      [ else (cipher-helper (substring l 1))])]))

;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (getfirst l)
  (cond [(null? l) '()]
        [else (cons (car (car l)) (getfirst (cdr l)))]))

(define (makebigrams cipher-word-list)
  (define (makebigramsword word)
  (cond [(or (= (string-length word) 0) (= (string-length word) 1)) '()]
        [else (append (list (substring word 0 2)) (makebigramsword (substring word 1)))]))
  (cond [(null? cipher-word-list) '()]
        [else (append (makebigramsword (car cipher-word-list)) (makebigrams (cdr cipher-word-list)))]))

(define (cipher-bigrams cipher-word-list)
  (getfirst (sort-by-frequency (map (lambda (x) (cons x (length (indexes-of (makebigrams cipher-word-list) x))))
                                    (remove-duplicates (makebigrams cipher-word-list))))))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.
(define (nextchar char)
  (integer->char (+ (char->integer char) 1)))

(define (letter-unique cipher-bigrams-list letter mode)
  (cond [(equal? mode 'predecessor) (cond [(null? cipher-bigrams-list) (cons letter 0)]
                                          [(equal? letter (car (string->list (car cipher-bigrams-list))))
                                                (cons letter (+ 1 (cdr (letter-unique (cdr cipher-bigrams-list) letter mode))))]
                                          [else (letter-unique (cdr cipher-bigrams-list) letter mode)])]
                                    
        [(equal? mode 'successor)  (cond [(null? cipher-bigrams-list) (cons letter 0)]
                                         [(equal? letter (cadr (string->list (car cipher-bigrams-list))))
                                                (cons letter (+ 1 (cdr (letter-unique (cdr cipher-bigrams-list) letter mode))))]
                                         [else (letter-unique  (cdr cipher-bigrams-list) letter mode)])]
        
        [(equal? mode 'both)       (cond [(null? cipher-bigrams-list) (cons letter 0)]
                                         [(or (equal? letter (cadr (string->list (car cipher-bigrams-list)))) (equal? letter (car (string->list (car cipher-bigrams-list)))))
                                                (cons letter (+ 1 (cdr (letter-unique (cdr cipher-bigrams-list) letter mode))))]
                                         [else (letter-unique  (cdr cipher-bigrams-list) letter mode)])]))
        
(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  (define (cipher-unique-neighbourhood-helper cipher-bigrams-helper letter mode)
    (cond [(char-ci<=? letter #\z )
           (append (list (letter-unique cipher-bigrams-helper letter mode)) (cipher-unique-neighbourhood-helper cipher-bigrams-helper (nextchar letter) mode))]
          [else '()]))
 (sort-by-frequency (cipher-unique-neighbourhood-helper cipher-bigrams-list #\a mode)))

(define (list-alphabetic l)
  (cond [(null? l) '()]
        [(char-alphabetic? (car l)) (cons (car l) (list-alphabetic (cdr l)))]
        [else (list-alphabetic (cdr l))]))
  
  
;(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
;  '())

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
;(define (cipher-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
;  '())

(define (cipher-bigrams-frequency cipher-word-list)
  (sort-by-frequency (map (lambda (x) (cons x (length (indexes-of (makebigrams cipher-word-list) x))))
                                    (remove-duplicates (makebigrams cipher-word-list)))))

(define (cipher-neighbourhood cipher-bigrams-list mode);cipher word list
  (define (cipher-neighbourhood-helper cipher-bigrams-helper letter mode)
    (cond [(char-ci<=? letter #\z )
           (append (list (letter-neighbour cipher-bigrams-helper letter mode)) (cipher-neighbourhood-helper cipher-bigrams-helper (nextchar letter) mode))]
          [else '()]))
 (sort-by-frequency (cipher-neighbourhood-helper (cipher-bigrams-frequency cipher-bigrams-list) #\a mode)))

(define (letter-neighbour cipher-bigrams-1 letter mode)
  (cond [(equal? mode 'predecessor) (if (null? cipher-bigrams-1) (cons letter 0)

                                        (let* ([first (car cipher-bigrams-1)]
                                              [rem (cdr cipher-bigrams-1)]
                                              [bigram-word (car first)]
                                              [bigram-frequency (cdr first)])
                     (cond
                                          [(and (equal? letter (car (string->list bigram-word))) (char-alphabetic? (cadr (string->list bigram-word))))
                                                (cons letter (+ bigram-frequency (cdr (letter-neighbour rem letter mode))))]
                                          [else (letter-neighbour rem letter mode)])))]
                                    
        [(equal? mode 'successor)  (if (null? cipher-bigrams-1) (cons letter 0)

                                        (let* ([first (car cipher-bigrams-1)]
                                              [rem (cdr cipher-bigrams-1)]
                                              [bigram-word (car first)]
                                              [bigram-frequency (cdr first)])
                                 (cond
                     
                                          [(and (equal? letter (cadr (string->list bigram-word))) (char-alphabetic? (car (string->list bigram-word))))
                                                (cons letter (+ bigram-frequency (cdr (letter-neighbour rem letter mode))))]
                                          [else (letter-neighbour rem letter mode)])))]
        
        [(equal? mode 'both) (cons letter (- (+ (cdr (letter-neighbour cipher-bigrams-1 letter 'predecessor)) (cdr (letter-neighbour cipher-bigrams-1 letter 'successor)))
                                             (successive cipher-bigrams-1 letter)
                                             ))]))

(define (successive cipher-bigrams-1 letter)
  (cond [(null? cipher-bigrams-1) 0]
        [(equal? (car (car cipher-bigrams-1)) (list->string (list letter letter))) (cdr (car cipher-bigrams-1))]
        [else (successive (cdr cipher-bigrams-1) letter)]))
  
;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (maketrigrams cipher-word-list)
  (define (maketrigramsword word)
  (cond [(< (string-length word) 3) '()]
        [else (append (list (substring word 0 3)) (maketrigramsword (substring word 1)))]))
  (cond [(null? cipher-word-list) '()]
        [else (append (maketrigramsword (car cipher-word-list)) (maketrigrams (cdr cipher-word-list)))]))

(define (cipher-trigrams cipher-word-list)
  (getfirst (sort-by-frequency (map (lambda (x) (cons x (length (indexes-of (maketrigrams cipher-word-list) x))))
                                    (remove-duplicates (maketrigrams cipher-word-list))))))

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (makequadgrams cipher-word-list)
  (define (makequadgramsword word)
  (cond [(< (string-length word) 4) '()]
        [else (append (list (substring word 0 4)) (makequadgramsword (substring word 1)))]))
  (cond [(null? cipher-word-list) '()]
        [else (append (makequadgramsword (car cipher-word-list)) (makequadgrams (cdr cipher-word-list)))]))

(define (cipher-quadgrams cipher-word-list)
  (getfirst (sort-by-frequency (map (lambda (x) (cons x (length (indexes-of (makequadgrams cipher-word-list) x))))
                                    (remove-duplicates (makequadgrams cipher-word-list))))))

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!
;(define (cipher-common-words-single cipher-word-list)
  
;  '())

(define (cipher-common-words-single cipher-word-list)
  (cipher-common-words-nple cipher-word-list 1))

;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-double cipher-word-list)
  (cipher-common-words-nple cipher-word-list 2))

(define (cipher-common-words-nple cipher-word-list n)
  (define (helper cipher-word-lst-nple)
    (cond [(null? cipher-word-lst-nple) '()]
        [(= (string-length (car cipher-word-lst-nple)) n) (cons (car cipher-word-lst-nple) (helper (cdr cipher-word-lst-nple)))]
        [else (helper (cdr cipher-word-lst-nple))]))
 (getfirst (sort-by-frequency (map (lambda (x) (cons x (length (indexes-of (helper cipher-word-list) x))))
                                    (remove-duplicates (helper cipher-word-list)))))) 
  
;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
  (cipher-common-words-nple cipher-word-list 3))
;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
  (cipher-common-words-nple cipher-word-list 4))

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
  '())
