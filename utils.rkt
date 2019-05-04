#lang racket/base

;; CAUTION
;; =======
;; YOU ARE NOT ALOWED TO EDIT THIS FILE. IF YOU WISH TO EDIT OR AUGMENT THESE
;; FUNCTIONS, DO SO IN SOME OTHER (NEW) MODULE.

;; You CANNOT require more modules of your choice :)
(require racket/sequence
         racket/random
         racket/string
         racket/match
         racket/list
         racket/port)

(provide encrypt
         decrypt
         encryption-key
         encryption-key-hard
         dictionary
         flat-dictionary
         locally-consistent
         ciphertext
         cipher-word-list
         cipher-word-list-f
         read-hist-word-list
         read-plain-word-list
         plain-char-offset
         cipher-char-offset
         is-monoalphabetic?
         add-substitution
         show-key
         read-ciphertext
         read-plaintext
         plain-monograms
         plain-bigrams
         plain-trigrams
         plain-quadgrams
         plain-common-words-single
         plain-common-words-double
         plain-common-words-triple
         plain-common-words-quadruple
         plain-common-initial-letters
         plain-common-final-letters
         plain-common-double-letters
         PLAIN-BEGIN
         CIPHER-BEGIN)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Encryption and Decryption utils                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generates an encryption key with the given word as the secret-word.
(define (encryption-key secret-word)
  (let* ([secret (string->list (string-downcase secret-word))]
         [offset (cipher-char-offset (last secret))])
    (append
     secret
     (remove* secret
              (build-list 26
                          (lambda (x) (integer->char
                                       (+ (remainder (+ x offset)
                                                     26)
                                          CIPHER-BEGIN))))))))

;; Generate a random permutation of the alphabet.
(define (encryption-key-hard)
  (map integer->char
       (shuffle (build-list 26 (lambda (x) (+ x CIPHER-BEGIN))))))

;; Decrypts text, even with a partial key.
(define (decrypt key ciphertext)
  (list->string
   (for/list ([cipher (in-string ciphertext)])
     ; Ciphertext is lowercase. Don't touch plaintext!
     (if (and (char-alphabetic? cipher) (char-lower-case? cipher))
         (if (member cipher key)
             (integer->char (+ (index-of key cipher) PLAIN-BEGIN))
             cipher)
         cipher))))

;; Encrypts using a complete key. Not suitable with partial keys.
(define (encrypt key plaintext)
  (list->string
   (for/list ([plain (in-string plaintext)])
     (if (char-alphabetic? plain)
         (list-ref key (plain-char-offset plain))
         plain))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Miscellaneous                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define PLAIN-BEGIN  65)  ; upcase
(define CIPHER-BEGIN 97) ; downcase

(define (plain-char-offset char)
  (- (char->integer char) PLAIN-BEGIN))

(define (cipher-char-offset char)
  (- (char->integer char) CIPHER-BEGIN))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                File Utilities                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ciphertext is all lower case
(define (read-ciphertext file-path)
  (call-with-input-file file-path
    (lambda (fin) (string-downcase (port->string fin)))))

;; plaintext is all upper case
(define (read-plaintext file-path)
  (call-with-input-file file-path
    (lambda (fin) (string-upcase (port->string fin)))))

(define (read-plain-word-list file-path)
  (call-with-input-file file-path
    (lambda (fin) (sequence->list (in-lines fin)))))

(define (read-hist-word-list file-path #:pick? [choice 'word])
  (call-with-input-file file-path
    (lambda (fin)
      (for/list ([word-count (in-lines fin)])
        (let ([wc-split (string-split word-count #:trim? #t)])
          (match choice
            ['word   (car wc-split)]
            ['counts (string->number (cadr wc-split))]
            ['both   (cons (car wc-split)
                           (string->number (cadr wc-split)))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Reading all the data                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The plain and cipher text

(define plaintext (read-plaintext "samples/text-04"))
;; (define ciphertext (read-ciphertext "encrypted/02.txt"))
(define ciphertext (encrypt (encryption-key "PLACID") plaintext))

;; List of words
(define dictionary
  (read-hist-word-list "data/google-books-common-words.txt"))

(define flat-dictionary (string-join dictionary))

(define-values
  (plain-monograms
   plain-bigrams
   plain-trigrams
   plain-quadgrams)
  (values
   (map (compose1 car string->list)
        (read-hist-word-list "data/ngrams1.txt"))
   (read-hist-word-list "data/ngrams2.txt")
   (read-hist-word-list "data/ngrams3.txt")
   (read-hist-word-list "data/ngrams4.txt")))

(define-values
  (plain-common-words-single
   plain-common-words-double
   plain-common-words-triple
   plain-common-words-quadruple)
  (values
   (list "A" "I")
   (filter (lambda (x) (= (string-length x) 2)) dictionary)
   (filter (lambda (x) (= (string-length x) 3)) dictionary)
   (filter (lambda (x) (= (string-length x) 4)) dictionary)))

(define-values
  (plain-common-initial-letters
   plain-common-final-letters
   plain-common-double-letters)
  (values
   (read-plain-word-list "data/initial-letters.txt")
   (read-plain-word-list "data/final-letters.txt")
   (read-plain-word-list "data/double-letters.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Key Predicates and Functions                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Extends the key using the given substitution.
;; Substitution MUST be a list of pairs, pair of plaintext char and ciphertext char.
;; The function does not perform any checks, please use `is-monoalphabetic`.
(define (add-substitution substitution key)
  (foldl (lambda (subst-pair new-key)
           (list-set new-key
                     (plain-char-offset (car subst-pair))
                     (cdr subst-pair)))
         key
         substitution))  

(define (locally-consistent substitution)
  (not (or
        (check-duplicates (for/list ([subst-pair substitution])
                            (car subst-pair)))
        (check-duplicates (for/list ([subst-pair substitution])
                            (cdr subst-pair))))))

;; Determines if the substitution is monoalphabetic (correct) wrt. this
;; key. Assumes that the key is monoalphabetic!
;;
;; A substitution fails this test if it maps same plain char to multiple cipher
;; chars, or vice-versa, or inconsistently with the given key. Does not complain
;; if the extended-key is exactly same as the key.
(define (is-monoalphabetic? substitution key)
  (let ([result 
         (and
          (locally-consistent substitution)
          (for/and ([subst-pair substitution])
            (let ([plain-code  (car subst-pair)]
                  [cipher-code (cdr subst-pair)])
              (and (char-lower-case? cipher-code)
                   (char-upper-case? plain-code)
                   (or
                    (equal? #\_
                            (list-ref key (plain-char-offset plain-code)))
                    (equal? cipher-code
                            (list-ref key (plain-char-offset plain-code))))))))])
    result))

(define (show-key key)
  (begin
    (displayln (build-list 26 (lambda (x) (integer->char (+ x PLAIN-BEGIN)))))
    (displayln key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Ignore the following, don't modify.                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sanitize-text ciphertext)
  (regexp-replace* (pregexp "'\\w+") ciphertext ""))

(define cipher-word-list
  ;; unfortunately the succint regexp is not a good one: "(?<!'\\w+)\\w+"
  ;; it cannot be represented in a fixed amount of memory
  ;; thus a sanitation step is necessary.
  (regexp-match* (pregexp "\\w+") (sanitize-text ciphertext)))

(define (cipher-word-list-f cipher-text)
  ;; unfortunately the succint regexp is not a good one: "(?<!'\\w+)\\w+"
  ;; it cannot be represented in a fixed amount of memory
  ;; thus a sanitation step is necessary.
  (regexp-match* (pregexp "\\w+") (sanitize-text cipher-text)))
