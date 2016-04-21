;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mathtools) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Tools:

(require "provide.rkt")
(require "random.rkt")

(provide euclid-gcd solve-lc mod-expt prime? random-prime)

;; This module provides the functions involved in modular arithmetic, such as modular exponentation, solving linear congruence, Euclidean Algorithm
;; Extended Euclidean Algorithm, and the Solovay-Strassen Primality Test


;; EUCLIDEAN ALGORITHM:

;; (euclid-gcd a b) finds gcd(a, b) using the Euclidean Algorithm
;; euclid-gcd: Nat Nat -> Nat

(define (euclid-gcd a b)
  (cond
    [(zero? b) a]
    [else (euclid-gcd b (remainder a b))]))




;; EXTENDED EUCLIDEAN ALGORITHM:


(define-struct eea-row (x y r q))
;; An EEA-Row is a (make-eea-row Int Int Nat Nat)


;; (roweea row1 row2) executes the Extended Euclidean Algorithm given two EEA-Rows
;; roweea: EEA-Row EEA-Row -> EEA-Row
(define (roweea row1 row2)
  (cond
    [(zero? (eea-row-r row2)) row1]
    [else
     (local
       [(define newq (quotient (eea-row-r row1) (eea-row-r row2)))
        (define newr (- (eea-row-r row1) (* newq (eea-row-r row2))))
        (define newx (- (eea-row-x row1) (* newq (eea-row-x row2))))
        (define newy (- (eea-row-y row1) (* newq (eea-row-y row2))))
        (define newrow (make-eea-row newx newy newr newq))]
       (roweea row2 newrow))]))

;; (eea a b) finds gcd(a, b) = d and some x, y such that ax + by = d, using the Extended Euclidean Algorithm
;; eea: Nat Nat -> EEA-Row
;; requires: a, b > 0

(define (eea a b)
  (local    
    [(define prev-row (make-eea-row 1 0 a 0))
     (define cur-row (make-eea-row 0 1 b 0))]
    (roweea prev-row cur-row)))



;; SOLVING LINEAR CONGRUENCES:

;; (make-positive a b) adds b to a until a is positive
;; make-positive: Int Nat -> Nat
;; requires: b > 0
(define (make-positive a b)
  (cond
    [(> a 0) a]
    [else (make-positive (+ a b) b)]))

;; (solve-lc a c m) solves the linear congruence in the form of ax = c mod m (gives the first solution)
;; solve-lc: Nat Nat Nat -> anyof(Nat, False)
;; requires: m > 0

(define (solve-lc a c m)
  (local
    [(define eea-result (eea a m))
     (define gcd (eea-row-r eea-result))
     (define x (eea-row-x eea-result))
     (define newx (* x (/ c gcd)))]
    
    (cond
      [(not (zero? (remainder c gcd))) false]
      [else (make-positive newx m)])))
     


;; MODULAR EXPONENTATION (POLYNOMIAL TIME):

;; (dec-to-bin n) converts n to a binary representation with a list of digits (reversed)
;; dec-to-bin: Nat -> (listof Nat)

(define (dec-to-bin n)
  (cond
    [(zero? n) empty]
    [else (cons (remainder n 2)
                (dec-to-bin (quotient n 2)))]))


;; (mod-2-power base exp m) computes base^2^exp mod m
;; mod-power: Nat Nat Nat -> Nat
;; requires: m > 0

(define (mod-2-power base exp m)
  (cond
    [(zero? exp) (remainder base m)]
    [else (mod-2-power (remainder (sqr base) m) (sub1 exp) m)]))


;; (mod-power-lst base exp m) produces (list base^2^0 mod m, base^2^1 mod m, base^2^2 mod m, ... , base^2^exp-1 mod m)
;; mod-power-lst: Nat Nat Nat -> (listof Nat)
;; requires: m > 0

(define (mod-power-lst base exp m)
  (build-list exp (lambda (i) (mod-2-power base i m))))


;; (mod-expt base exp m) computes base^exp mod m in polynomial time
;; mod-expt: Nat Nat Nat -> Nat

(define (mod-expt-lst binary power-ind m acc)
  (cond
    [(empty? binary) acc]
    [(zero? (first binary))
     (mod-expt-lst (rest binary) (rest power-ind) m acc)]
    [else (mod-expt-lst (rest binary) (rest power-ind) m (remainder (* acc (first power-ind)) m))]))
    

(define (mod-expt base exp m)
  (local
    [(define exp-bin (dec-to-bin exp))
     (define powers (mod-power-lst base (length exp-bin) m))]

    (mod-expt-lst exp-bin powers m 1)))


;(check-expect (mod-expt 5 6 3) (remainder (expt 5 6) 3))
;(check-expect (mod-expt 16 23 7) (remainder (expt 16 23) 7))


;; SOLOVAY-STRASSEN PRIMALITY TEST:

(define num-trials 10) ;; number of times we generate a new random number n

;; (jacobi a p) calculates the Jacobi symbol (a/p), where p is potentially prime
;; legendre: Nat Nat -> Int
;; requires: p is odd

(define (jacobi a p)
  (cond
    [(> a p) (jacobi (remainder a p) p)]
    
    [(and (zero? (remainder a 2))
          (or (= (remainder p 8) 1) (= (remainder p 8) 7)))
     (jacobi (/ a 2) p)]
    
    [(and (zero? (remainder a 2))
          (or (= (remainder p 8) 3) (= (remainder p 8) 5)))
     (* -1 (jacobi (/ a 2) p))]
    
    [(= a 1) 1]

    [(not (= (euclid-gcd a p) 1)) 0]

    [(= 3 (remainder a 4) (remainder p 4))
     (* -1 (jacobi p a))]

    [else (jacobi p a)]))
     
  

;; (log10 n) calculates log(n) base 10
;; log10: Num -> Num
(define (log10 n)
  (/ (log n) (log 10)))


;; (prime? p) checks whether p is prime using the Solovay-Strassen Test, in trials iterations
;; prime?: Nat-> Bool
;; requires: p is odd

(define (prime-trials? p trials)
  (local
    [(define power (ceiling (log10 p)))
     (define digits (cond [(exact? power) power]
                          [else (inexact->exact power)]))
     (define random-dig (non-zero digits))
     (define a (random-num random-dig))
     (define ex (/ (- p 1) 2))
     (define a-power (mod-expt a ex p))
     (define simp-power (cond [(= a-power (sub1 p)) -1]
                              [else a-power]))]

    (cond
      [(zero? trials) true]
      [(not (= simp-power (jacobi a p))) false]
      [else (prime-trials? p (sub1 trials))])))

(define (prime? p)
  (prime-trials? p num-trials))



;; (random-prime min max) generates a random prime number that is between min and max digits long
;; random-prime: Nat Nat -> Nat

(define (random-prime min max)
  (local
    [(define digits (random-range min max))
     (define candidate (random-odd digits))]
    (cond
      [(prime? candidate) candidate]
      [else (random-prime min max)])))


;; (random-coprime n) finds a random number less than and coprime with n
;; random-coprime: Nat -> Nat

(define (random-coprime n)
  (local
    [(define n-power (ceiling (log10 n)))
     (define n-digits (cond [(exact? n-power) n-power]
                            [else (inexact->exact n-power)]))
     (define newdig (non-zero n-digits))
     (define newnum (random-num newdig))]

    (cond
      [(and (> newnum 1) (= 1 (euclid-gcd newnum n))) newnum]
      [else (random-coprime n)])))

    