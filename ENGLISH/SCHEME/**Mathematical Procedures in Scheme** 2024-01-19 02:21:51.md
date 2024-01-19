```scheme
; Define a procedure to calculate the factorial of a number
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

; Define a procedure to calculate the nth Fibonacci number
(define (fib n)
  (if (or (zero? n) (eq? n 1))
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

; Define a procedure to determine if a number is prime
(define (prime? n)
  (cond [(= n 1) false]
        [(= n 2) true]
        [(even? n) false]
        [else (and (prime? (- n 2)) (prime? (/ n 3)))]))

; Define a procedure to find the greatest common divisor of two numbers
(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))

; Define a procedure to find the least common multiple of two numbers
(define (lcm a b)
  (/ (* a b) (gcd a b)))

; Define a procedure to calculate the area of a triangle
(define (triangle-area a b c)
  (sqrt (/ 2 (* (- (* a b) (* b c) (* c a))) 2)))

; Define a procedure to calculate the volume of a sphere
(define (sphere-volume r)
  (* (/ 4 3) (* pi (* r r r))))

; Define a procedure to calculate the surface area of a sphere
(define (sphere-surface-area r)
  (* 4 (* pi (* r r))))

; Define a procedure to calculate the derivative of a function
(define (derivative f x)
  (/ (- (f (+ x h)) (f x)) h))

; Define a procedure to integrate a function
(define (integral f a b n)
  (define h (/ (- b a) n))
  (define sum 0)
  (for ((i 0 (+ 1 n)))
    (set! sum (+ sum (* f (+ a (* i h)))))
  (* h sum)))

; Define a procedure to plot a function
(define (plot f a b n)
  (for ((x a (+ a (/ (- b a) n))))
    (display (format "%.2f %.2f\n" x (f x)))))
```

This code contains a collection of useful mathematical procedures, including factorial, Fibonacci numbers, primality testing, greatest common divisor, least common multiple, triangle area, sphere volume and surface area, derivative, integral, and plotting. It demonstrates a variety of programming techniques and should be interesting and challenging to read and understand.