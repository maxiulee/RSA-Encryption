# RSA-Encryption

- This is an implementation of the RSA encryption algorithm on Racket
- A brief overview of each module:
- 1. random.rkt provides functions needed to generate large random numbers (since Racket's built-in random function can only generate         numbers up to 4294967087)
- 2. mathtools.rkt provides the mathematical machinery needed for the encryption algorithm to work. This includes the Extended Euclidean      Algorithm, solving linear congruences, efficient modular exponentation, and the Solovay-Strassen Primality test. Also includes a         function that generates random primes and finding random coprimes
- 3. rsa.rkt provides the RSA interface functions for encryption and decryption, implemented according to the RSA Theorem
- 4. provide.rkt makes the provide function available to the other modules, which are written in the teaching languages

