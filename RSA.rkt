;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname RSA) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;******************************************
;;             RSA Scheme Project
;;               Abhinav Gupta
;;  (works for whole numbers greater than 1)
;;           (works till 1 million)
;;******************************************
;;

(require math/number-theory)

;; ******************************************
;;
;;                 USER INPUT
;;
;; ******************************************

;; message is a part of the main RSA scheme
(define message 21)

;; ******************************************
;;
;;                   SET UP
;;
;; ******************************************

;; A Prime is a Nat
;; Requires: A Prime is only divisible
;;           by 1 and itself

;; consists of a list of primes till 1000
(define list-primes
  (filter number?
          (build-list 3000 (lambda (x)
                             (cond
                               [(prime? x) x]
                               [else false])))))

;; (pick-item l) randomly chooses an element of l
;; pick-item: (listof X) -> X
(define (pick-item l)
  (list-ref l (random (length l))))

;; (prime-unique num check) produces a prime number that
;;                          is not equal to check
;; prime-unique: Prime Nat -> Prime
(define (prime-unique num check)
  (cond
    [(= num check)
     (prime-unique (pick-item list-primes) check)]
    [else num]))

;; (valid-prime prime-num) checks if the second chosen
;;                         prime is valid for message
;; valid-prime: Prime -> Prime
(define (valid-prime prime-num)
  (cond
    [(<= (* prime-num prime1) message)
     (valid-prime (pick-item list-primes))]
    [else (prime-unique prime-num prime1)]))

;; ******************************************
;;
;;                 ENCRYPTION
;;
;; ******************************************

;; (encryption M-expt-e) produces the encrypted number
;; encryption: Nat -> Nat
(define (encryption M-expt-e)
  (local
    [(define as (list M-expt-e M-expt-e))
     (define ns (list prime1 prime2))]
    (solve-chinese as ns)))

;; (lesser-n/q M-expt-e n) produces a number lesser than n/q
;; lesser-n/q: Nat Nat -> Nat
(define (lesser-n/q M-expt-e n/q)
  (cond
    [(> M-expt-e n/q)
     (lesser-n/q (- M-expt-e (* n/q (round (/ M-expt-e n/q)))) n/q)]
    [(< M-expt-e 0) (lesser-n/q (+ M-expt-e n/q) n/q)]
    [else M-expt-e]))

;; ******************************************
;;
;;                 DECRYPTION
;;
;; ******************************************

;; (decryption C-expt-e p1 p2) produces the original message
;; decryption: Nat Nat Nat -> Nat
(define (decryption C-expt-e p1 p2)
  (local
    [(define as (list C-expt-e C-expt-e))
     (define ns (list p1 p2))]
    (solve-chinese as ns)))

;; ******************************************
;;
;;                DECRYPT-ANY
;;
;; ******************************************

;; (decrypt cipher d-private p1 p2)
;; produces the original message for any cipher
;; decrypt: Nat Nat Prime Prime -> Nat
(define (decrypt cipher d-private p1 p2)
  (decryption (expt cipher d-private) p1 p2))

;; ******************************************
;;
;;             IMPORTANT TO USER
;;
;; ******************************************

;; randomly chooses the 1st prime number
;; prime1 is a part of the main RSA scheme
(define prime1 (pick-item list-primes))

;; randomly chooses the 2nd prime number
;; prime2 is a part of the main RSA scheme
(define prime2 (valid-prime (pick-item list-primes)))

;; q is a part of the main RSA scheme
(define q (* (sub1 prime1) (sub1 prime2)))

;; n is a part of the main RSA scheme
(define n (* prime1 prime2))

;; e-public is a part of the main RSA scheme
(define e-public (prime-unique (pick-item list-primes) q))

;; d is a part of the main RSA scheme
(define d-private (lesser-n/q (first (bezout e-public q)) q))

;; public-key is a part of the main RSA scheme
(define public-key (list e-public n))

;; private-key is a part of the main RSA scheme
(define private-key (list d-private n))

;; cipher is the encrypted number
;; cipher is a part of the main RSA scheme
(define cipher (encryption (expt message e-public)))

;; decrypted-message is a part of the main RSA scheme
(define decrypted-message (decryption (expt cipher d-private) prime1 prime2))