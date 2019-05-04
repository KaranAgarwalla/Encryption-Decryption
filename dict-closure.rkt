#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))
(provide dictionary-closure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dictionary-closure key)
  (dictionary-closure-helper key (utils:cipher-word-list-f (utils:decrypt key utils:ciphertext))))

(define (dictionary-closure-helper key word-list)
  (cond [(null? word-list) key]
        [(string=? (car word-list) (string-upcase (car word-list))) (dictionary-closure-helper key (cdr word-list))]
        [(let ([matches (match-word-dictionary (car word-list) key utils:dictionary '())])
                (if (null? matches) #\f
                    (cond
        [(= (length matches) 1) (dictionary-closure (utils:add-substitution (remove-duplicates (subs-needed (car matches) (car word-list) )) key))]
        [else (dictionary-closure-helper key (cdr word-list))])))]))

(define (zip l1 l2)
  (cond [(= (length l1) 0) '()]
        [else (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))]))

(define (parallel word1 word2)
  (zip (string->list word1) (string->list word2) ))

(define (capitals-decoded word1 word2)
  (filter (lambda (x) (char-upper-case? (cdr x))) (parallel word1 word2)))


(define (subs-needed word1 word2)
  (remove* (capitals-decoded word1 word2) (parallel word1 word2)))

(define key-trial (utils:add-substitution (list (cons #\T  #\y) (cons #\E  #\e) (cons #\A  #\w) (cons #\I  #\q)) (make-list 26 #\_)))

(define (match-word-word word1 word2 key)
  (if (= (string-length word1) (string-length word2))
      (if (andmap (lambda (x y) (if (char-upper-case? y) (equal? x y) #t)) (string->list word1) (string->list word2))
          (utils:is-monoalphabetic? (remove-duplicates (subs-needed word1 word2)) key) #f) #f))

(define (match-word-dictionary word1 key diction substitution)
  (cond [(null? diction) substitution]
        [(> (length substitution) 1) substitution]
        [(match-word-word (car diction) word1 key) (match-word-dictionary word1 key (cdr diction) (cons (car diction) substitution))]
        [else (match-word-dictionary word1 key (cdr diction) substitution)]))