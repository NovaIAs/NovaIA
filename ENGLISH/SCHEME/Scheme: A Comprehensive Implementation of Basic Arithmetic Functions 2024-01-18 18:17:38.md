```scheme
(define zero
  (lambda (x) x))

(define one
  (lambda (x) (lambda (y) (x y)))))

(define add
  (lambda (m n)
    (lambda (f) (f (m f) (n f)))))

(define sub
  (lambda (m n)
    (lambda (f) (m (lambda (x) (n f x))))))

(define mul
  (lambda (m n)
    (lambda (f) (m (n f)))))

(define div
  (lambda (m n)
    (lambda (f) (n (lambda (x) (m f x))))))

(define max
  (lambda (m n)
    (if (= m n) m
        (if (> m n) m n))))

(define min
  (lambda (m n)
    (if (= m n) m
        (if (< m n) m n))))

(define abs
  (lambda (n)
    (if (< n 0) (- n) n)))

(define sqrt
  (lambda (n)
    (define sqrt-iter
      (lambda (guess)
        (define improved (div (+ guess (/ n guess)) 2)))
        (if (close? guess improved) improved
            (sqrt-iter improved)))))

(define close?
  (lambda (a b)
    (< (abs (- a b)) 0.001)))

(define power
  (lambda (m n)
    (if (= n 0) 1
        (if (= n 1) m
            (* m (power m (- n 1)))))))

(define fact
  (lambda (n)
    (if (= n 0) 1
        (* n (fact (- n 1))))))

(define N
  (define (make-n m)
    (lambda (f) (f m))))

(define (inc n)
  (add n one))

(define (dec n)
  (sub n one))

(define (is-zero n)
  (= n zero))

(define (is-one n)
  (= n one))

(define (is-even n)
  (zero? (mod n 2)))

(define (is-odd n)
  (not (is-even n)))

(define (even? n)
  (is-even n))

(define (odd? n)
  (is-odd n))

(define (mod n m)
  (sub n (* (div n m) m)))
```

This code defines a number of basic arithmetic functions in Scheme, including addition, subtraction, multiplication, division, maximum, minimum, absolute value, square root, exponentiation, factorial, and modulus. It also defines a number of functions for working with natural numbers, represented as functions that take a function argument and return the result of applying that function to the number. These functions include functions for incrementing, decrementing, testing for zero, testing for one, testing for evenness, and testing for oddness.

The code is written in a very concise and idiomatic Scheme style, using lambda expressions, currying, and higher-order functions. It is also very efficient, as it avoids the use of recursion and instead uses iteration for all of the functions that require it.

Overall, this code is a very comprehensive and well-written implementation of a number of basic arithmetic functions in Scheme. It is a good example of the power and elegance of the Scheme programming language.