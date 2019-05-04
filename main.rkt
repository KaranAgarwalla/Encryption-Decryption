#lang racket

;; You can require more modules of your choice.
;; For example, list-comprehensions!
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt")
         (prefix-in strat: "strategies.rkt")
         (prefix-in stats: "statistics.rkt")
         (prefix-in  algo: "dict-closure.rkt")
         (prefix-in  algo: "secret-word-enumeration.rkt"))

(provide crack-cipher
         crack-hard-cipher ;; Optional, no extra credits for this :)
         )

;; TIPS
;; ----
;; 0. Please read the README. It will help you navigate these modules.
;; 1. Take a look at utilities in "utils.rkt" to ensure you don't re-invent the wheel!
;; 2. You can create a key quickly using `utils:encryption-key`
;; 3. You can create a random permutation key quickly using `utils:encryption-key-hard`
;; 4. Please make use of `utils:plaintext`, `utils:ciphertext` and `utils:cipher-word-list`.
;; 5. DO NOT ATTEMPT TO SPLIT THE TEXT INTO WORDS ON YOUR OWN.

;; CAUTION
;; =======
;; Your submission must operate on `utils:ciphertext` and
;; `utils:cipher-word-list`. We will be setting these variables forcibly while
;; automatically grading your submission. PLEASE DO NOT HARDCODE SOME OTHER
;; VARIABLE INSIDE ANY OF YOUR FUNCTIONS.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The "Key"                                                                        ;;
;; ========                                                                         ;;
;;                                                                                  ;;
;; Is represented by a list of 26 lower-case chars (including underscore). Example: ;;
;; ```                                                                              ;;
;; '(#\w #\i #\s #\d #\o #\m #\n #\p #\q #\r #\t #\u #\v #\x #\y #\z #\a #\b #\c #\e #\f #\g #\h #\j #\k #\l) ;;
;; ```                                                                              ;;
;;                                                                                  ;;
;; Make sure you know how to interpret this. The first character replaces "A" from  ;;
;; the plaintext, the next replaces "B", and so on. An underscore implies that we   ;;
;; don't know what replaces the character at that position in the alphabet. It is   ;;
;; important to place only underscores and lower-case characters in this list.      ;;
;;                                                                                  ;;
;; Here, "A" -> "w", "B" -> "i", "C" -> "s", ..., "Z" -> "l"                        ;;
;;                                                                                  ;;
;; Use `utils:show-key` to conveniently display the key.                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define plaintext (utils:read-plaintext "samples/text-2-sherlockholmes.txt"))
;; (define ciphertext (utils:read-ciphertext "encrypted/02.txt"))
;; (define ciphertext (encrypt (encryotion-key "wisdom") plaintext))
;; (define cipher-word-list utils:cipher-word-list)

;; The initial BLANK key

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; crack-cipher                                                                     ;;
;; ============                                                                     ;;
;; The main driver function!                                                        ;;
;;                                                                                  ;;
;; Formal arguments:                                                                ;;
;; `strategies` is a list of functions from the `strat` namespace. The              ;;
;;              strategies should be explored by `crack-cipher` in this order.      ;;
;; `key`        is the (initial) encryption key.                                    ;;
;;                                                                                  ;;
;; Implicit arguments:                                                              ;;
;; `ciphertext` is the encrypted text read from a file. It must be in lower         ;;
;;              case. See `utils:read-ciphertext`.                                  ;;
;; `cipher-word-list` is the list of words in the cipher text. You just have to     ;;
;;              use the pre-computed list in `utils:cipher-word-list`               ;;
;;                                                                                  ;;
;; Behaviour of `crack-cipher`                                                      ;;
;; ---------------------------                                                      ;;
;;                                                                                  ;;
;; `crack-cipher` needs to emulate a (dumb) head first dive into the search space   ;;
;; of the encryption key.                                                           ;;
;;                                                                                  ;;
;; It sufficies (for this assignment) to explore (each) strategy's substitution     ;;
;; choices and pick the first one that seems promising and jump to the next         ;;
;; strategy (since we have, in some sense, used up whatever this strategy could     ;;
;; tell us).                                                                        ;;
;;                                                                                  ;;
;; The most general approach is to revisit all remaining substitutions and explore  ;;
;; them as well, such an exploration is called a Depth First Search.                ;;
;;                                                                                  ;;
;; If the substitution of a particular strategy is found to be bad, we pick it's    ;;
;; next substitution. If we are unlucky, all substitutions could get exhausted, and ;;
;; then we must ditch this strategy and proceed to the next one. But the moment we  ;;
;; get lucky, we short-circuit all the remaining substitutions and proceed to the   ;;
;; next strategy (if necessary).                                                    ;;
;;                                                                                  ;;
;; We judge a substitution as "bad" using dictionary-closure and                    ;;
;; secret-word-enumeration. You are free to create your own criterions to judge bad ;;
;; vs good substitutions. Of course these two steps help greatly in leap-frogging   ;;
;; to the complete key! The smarter they are, the better.                           ;;
;;                                                                                  ;;
;; Take a moment to pin down the termination criterion for your algorithm. How do   ;;
;; you detect errors (early) or eventual completion?                                ;;
;;                                                                                  ;;
;; A word to the wise: When building the substitution list in the strategy          ;;
;; function, if unsure about correctness of a substitution it is better to keep it  ;;
;; than throw it away. You can filter out bad keys with more confidence than        ;;
;; filtering out substitutions.                                                     ;;
;;                                                                                  ;;
;; EDIT in v1.1                                                                     ;;
;; ------------                                                                     ;;
;; The function must return a list of keys, each key being a list of 26             ;;
;; lower-case chars representing the key. If you are unable to completely           ;;
;; determine the key, return a partial key. A trivial partial key is a list         ;;
;; of 26 underscores.                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define key (build-list 26 (lambda (_) #\_)))

(define (crack-cipher strategies key)
   (remove-duplicates (append* (map (lambda (x) (checking-subs key (x key) '())) strategies))))

(define (checking-subs ikey subs ans)
  (if (null? subs) ans
      (let ([match (algo:dictionary-closure (utils:add-substitution (car subs) ikey))])
  (cond [(equal? match #\f) (checking-subs key (cdr subs) ans)]
        [(equal? (algo:secret-word-enumeration match) #f) (checking-subs key (cdr subs) ans)]
        [else (checking-subs key (cdr subs) (append ans (algo:secret-word-enumeration (algo:dictionary-closure (utils:add-substitution (car subs) ikey)))))])))) 


   ;; Returns list of encryption keys.
  ;; make use of `utils:ciphertext` and `utils:cipher-word-list`
  ;; DISPLAY A KEY AS SOON AS YOU FIND IT USING (show-key key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Optional task                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Behaviour is exactly as `crack-cipher` except that you cannot use
;; `secret-word-enumeration` because there is no secret word in the key.
(define (crack-hard-cipher strategies key) ;; Returns a list of encryption keys.
  ;; make use of `utils:ciphertext` and `utils:cipher-word-list`
  ;; DISPLAY A KEY AS SOON AS YOU FIND IT USING (show-key key)
  (list key))
