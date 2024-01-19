```scheme
(define (ackermann m n)
  (cond
    ((zero? m) n)
    ((zero? n) (ackermann (sub1 m) 1))
    (else (ackermann (sub1 m) (ackermann m (sub1 n))))))

(define (factorial n)
  (if (zero? n) 1 (* n (factorial (sub1 n)))))

(define (fibonacci n)
  (cond
    ((< n 2) n)
    (else (+ (fibonacci (sub1 n)) (fibonacci (sub2 n))))))

(define (gcd a b)
  (cond
    ((zero? b) a)
    (else (gcd b (remainder a b)))))

(define (lcm a b)
  (* a b (quotient (gcd a b) 1)))

(define (prime? n)
  (if (or (<= n 1) (zero? (remainder n 2)))
    #f
    (loop
      (let ((divisor (add1 2)))
	(cond
	  ((> divisor (quotient n 2)) #t)
	  ((zero? (remainder n divisor)) #f)
	  (else (set! divisor (add1 divisor)))))))

(define (sqrt x)
  (define (iter guess)
    (define (avg guess x)
      (/ (+ guess (/ x guess)) 2))
    (if (close-enough? guess (avg guess x))
	guess
	(iter (avg guess x))))
  (iter 1))

(define (close-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (abs x)
  (if (negative? x) (- x) x))

(define (negative? x)
  (< x 0))

(define (min a b)
  (if (< a b) a b))

(define (max a b)
  (if (> a b) a b))

(define (brightness->wavelength wavelength)
  (/ (* c speed-of-light) wavelength))

(define (wavelength->brightness wavelength)
  (* c speed-of-light wavelength))

(define (celsius->fahrenheit temperature)
  (+ (* 9 temperature) (/ 5 1)))

(define (fahrenheit->celsius temperature)
  (/ (- temperature 32) (* 9 5)))

(define (speed-of-light) 299792458)
```

This code implements a variety of mathematical functions and algorithms in Scheme. Here's a brief explanation of each function:

- `ackermann`: Computes the Ackermann function, a recursive function that grows very quickly.
- `factorial`: Computes the factorial of a number.
- `fibonacci`: Computes the nth Fibonacci number.
- `gcd`: Computes the greatest common divisor of two numbers.
- `lcm`: Computes the least common multiple of two numbers.
- `prime?`: Determines if a number is prime.
- `sqrt`: Computes the square root of a number using the Babylonian method.
- `close-enough?`: Compares two numbers for approximate equality.
- `square`: Computes the square of a number.
- `abs`: Computes the absolute value of a number.
- `negative?`: Checks if a number is negative.
- `max`: Returns the greater of two numbers.
- `min`: Returns the lesser of two numbers.
- `brightness->wavelength`: Converts a brightness value to a wavelength in nanometers.
- `wavelength->brightness`: Converts a wavelength in nanometers to a brightness value.
- `celsius->fahrenheit`: Converts a temperature in degrees Celsius to degrees Fahrenheit.
- `fahrenheit->celsius`: Converts a temperature in degrees Fahrenheit to degrees Celsius.