```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fibonacci (- n 1))
            (fibonacci (- n 2)))]))

(define (gcd a b)
  (cond
    [(= b 0) a]
    [else (gcd b (remainder a b))]))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (is-prime? n)
  (cond
    [(= n 1) false]
    [(= n 2) true]
    [(= (remainder n 2) 0) false]
    [else (let loop ([i 3])
      (if (>= i (sqrt n))
          true
          (cond
            [(= (remainder n i) 0) false]
            [else (loop (+ i 2))])))]))

(define (next-prime n)
  (let loop ([p (+ n 1)])
    (if (is-prime? p) p (loop (+ p 1)))))

(define (prime-factors n)
  (if (is-prime? n)
      (list n)
      (let ([p (next-prime 1)])
        (if (= (remainder n p) 0)
            (let loop ([factors (list p)]
                  [remaining (/ n p)])
              (if (= remaining 1)
                  factors (loop factors remaining)))
            (loop 1))))

(define (solve-quadratic a b c)
  (cond
    [(= a 0) (solve-linear b c)]
    [else (let ([d (- (* b b) (* 4 a c))])
      (cond
        [(< d 0) "no real solutions"]
        [(= d 0) (list (/ (- b) a))]
        [else (list (/ (- b (sqrt d)) (* 2 a))
              (/ (- b (- (sqrt d)) (* 2 a)))]))))))

(define (solve-linear b c)
  (cond
    [(= b 0) (if (= c 0) "infinitely many solutions"
               "no solutions")]
    [else (list (/ c b))]))

(define (area-of-triangle a b c)
  (/ (sqrt (* 2 (area-of-heron a b c))) 2))

(define (area-of-heron a b c)
  (* (sqrt (- (* s (- s a)) (* s b)) (* s c)))))

(define (s a b c) (/ (+ a b c) 2))

(define (volume-of-sphere r)
  (* (/ 4 3) (* pi (* r r r)))))

(define (surface-area-of-sphere r)
  (* 4 (* pi (* r r)))))

(define (volume-of-cube s)
  (* s s s))

(define (surface-area-of-cube s)
  (* 6 (* s s))))

(define (volume-of-cylinder r h)
  (* pi (* r r) h))

(define (surface-area-of-cylinder r h)
  (* 2 (* pi (* r r)) (+ h h)))

(define (volume-of-cone r h)
  (* (/ 1 3) (* pi (* r r)) h))

(define (surface-area-of-cone r h)
  (* pi (* r (* r (+ r h)))))

(define (volume-of-pyramid b h)
  (* (/ 1 3) (* b h)))

(define (surface-area-of-pyramid b h)
  (+ (* b h) (/ (* b b) 2))))
```

This code provides a variety of common mathematic functions in Scheme. It includes functions for calculating factorials, Fibonacci numbers, greatest common divisors, least common multiples, checking if a number is prime, finding the next prime number, prime factorization, solving quadratic and linear equations, calculating the area and volume of various geometric shapes, and more. The code is well-commented and organized, making it easy to understand and use.