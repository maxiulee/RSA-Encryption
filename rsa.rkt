;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rsa) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; This module provides the functions needed for RSA Encryption

(require "provide.rkt")
(require "random.rkt")
(require "mathtools.rkt")

;; Useful Constants:
(define min-dig 10)
(define max-dig 20)


;; Public Key:
(define-struct pub (e n))
;; a Pub (public key) is a (make-pub Nat Nat)

;; Private Key:
(define-struct priv (d n))
;; a Priv (private key) is a (make-priv Nat Nat)

;; Master Key:
(define-struct key (public private))
;; a Key (master key) is a (make-key Pub Priv)


;; KEY GENERATION & MANAGEMENT:

;; (create-master-key min max) creates a Master Key, where the generating primes are between min and max digits
;; create-master-key: Nat Nat -> Key

(define (create-master-key min max)
  (local
    [(define p (random-prime min max))
     (define q (random-prime min max))
     (define n (* p q))
     (define gen-mod (* (- p 1) (- q 1)))
     (define e (random-coprime gen-mod))
     (define d (solve-lc e 1 gen-mod))
     (define public-key (make-pub e n))
     (define private-key (make-priv d n))]

    (make-key public-key private-key)))


;; (public-key master) extracts the public key from master for publication
;; public-key: Key -> Pub

(define (public-key master)
  (key-public master))

;; (private-key master) extracts the private key from master for decryption
;; private-key: Key -> Priv

(define (private-key master)
  (key-private master))


;; SENDING (ENCRYPTING) A MESSAGE:

;; (rsa-encrypt message pub-key) encrypts message with the given public key
;; rsa-encrypt: Nat Pub -> Nat

(define (rsa-encrypt message pub-key)
  (local
    [(define key (pub-e pub-key))
     (define prod (pub-n pub-key))]

    (mod-expt message key prod)))


;; RECEIVING (DECRYPTING) A MESSAGE:

;; (rsa-decrypt ciphertext priv-key) decrypts ciphertext using private key
;; rsa-decrypt: Nat Priv -> Nat

(define (rsa-decrypt ciphertext priv-key)
  (local
    [(define key (priv-d priv-key))
     (define prod (priv-n priv-key))]

    (mod-expt ciphertext key prod)))
