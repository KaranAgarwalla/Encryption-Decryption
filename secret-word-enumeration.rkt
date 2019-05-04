  #lang racket

;; You can require more modules of your choice.
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt"))

(provide secret-word-enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (secret-word-enumeration key-after-dictionary-closure) ;; Returns a key or false (#f)
   (let ([possible-matches (swe (string-upcase (list->string (take key-after-dictionary-closure 6))))])
     (if (null? possible-matches) #f
         (map (lambda (x) (if (dictionary-check (utils:cipher-word-list-f (utils:decrypt (utils:encryption-key x) utils:ciphertext)))
                              (utils:encryption-key x) '())) possible-matches))))

  (define (swe string)
  (remove* (list "") (map (lambda (x) (if (match (string->list string) (string->list x)) x "")) utils:dictionary)))

(define (match string x)
  (cond [(not (= (length string) (length x))) #f]
        [else (cond
                [(null? string) #t]
                [(char-alphabetic? (car string)) (if (equal? (car x) (car string)) (match (cdr string) (cdr x)) #f)]
                [else (match (cdr string) (cdr x))])]))


(define (dictionary-check cipher-word-list)
  (cond [(null? cipher-word-list) #t]
        [(member (car cipher-word-list) utils:dictionary) (dictionary-check (cdr cipher-word-list))]
        [else #f]))
     