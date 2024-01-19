```scheme

(define (my-factorial n)
  (if (= n 0)
      1
      (* n (my-factorial (- n 1)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (my-gcd a b)
  (cond
    [(zero? b) a]
    [else (my-gcd b (remainder a b))]))

(define (my-random x)
  (random x))

(define (my-pi x)
  (+ (* 4 (atan 1.0)) x))

(define (my-sqrt x)
  (sqrt x))

(define (my-exp x)
  (exp x))

(define (my-log x)
  (log x))

(define (my-sin x)
  (sin x))

(define (my-cos x)
  (cos x))

(define (my-tan x)
  (tan x))

(define (my-asin x)
  (asin x))

(define (my-acos x)
  (acos x))

(define (my-atan x)
  (atan x))

```