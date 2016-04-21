;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname random) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Tools:
(require "provide.rkt")
(provide random-range non-zero random-num random-odd)

;; (random-range min max) generates a random natural number in [min, max)
;; random-range: Nat Nat -> Nat
;; requires: max > min
(define (random-range min max)
  (+ min (random (- max min))))

;; (non-zero n) generates a random integer up to and including n that is not zero
;; non-zero: Nat -> Nat
(define (non-zero n)
  (local [(define n0 (random n))]
    (cond
      [(zero? n0) (non-zero n)]
      [else n0])))

;; (random-num digits) generates a random number of digits digits
;; random-num: Nat -> Nat

(define (random-num digits)
  (local
    [(define num-list (build-list digits (lambda (i) (random 10))))
     
     ;; (fix-list lon) checks if the first of lon is zero, if so it replaces with a non-zero digit
     ;; fix-list: (listof Nat) -> (listof Nat)
     (define (fix-list lon)
       (cond
         [(zero? (first lon)) (cons (non-zero 9) (rest lon))]
         [else lon]))

     (define valid-num-list (fix-list num-list))

     (define power-list (build-list digits (lambda (i) (* (list-ref valid-num-list i)
                                                          (expt 10 (- (sub1 digits) i))))))]

    (foldr + 0 power-list)))



;; (random-odd digits) produces a random odd number of digits digits
;; random-odd: Nat -> Nat

(define (random-odd digits)
  (local [(define n (random-num digits))]
    (cond [(even? n) (add1 n)]
          [else n])))

