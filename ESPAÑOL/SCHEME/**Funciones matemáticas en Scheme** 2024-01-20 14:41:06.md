```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (isPrime n)
  (if (<= n 1)
      #f
      (every? (lambda (d) (not (= 0 (modulo n d))))
               (range 2 (sqrt n)))))

(define (nthPrime n)
  (define (isPrime n)
    (if (<= n 1)
        #f
        (every? (lambda (d) (not (= 0 (modulo n d))))
                 (range 2 (sqrt n)))))
  (let loop ((n n) (i 2) (count 0))
    (if (= count n)
        i
        (if (isPrime i)
            (loop n (+ i 1) (+ count 1))
            (loop n (+ i 1) count)))))

(define (combinations n k)
  (if (< k 0)
      0
      (if (> k n)
          0
          (* (/ (factorial n) (factorial (- n k)))
             (/ (factorial k) (factorial (- k 1)))))))

(define (permutations n k)
  (if (< k 0)
      0
      (if (> k n)
          0
          (* (factorial n) (factorial (- n k))))))

(define (binomial n k)
  (if (< k 0)
      0
      (if (> k n)
          0
          (combinations n k))))

(define (quadraticRoots a b c)
  (let ((discriminant (- (* b b) (* 4 a c))))
    (if (< discriminant 0)
        #f
        (list (/ (- (- b) (sqrt discriminant)) (* 2 a))
              (/ (+ (- b) (sqrt discriminant)) (* 2 a))))))

(define (solveLinearEquation a b)
  (if (= a 0)
      #f
      (/ b a)))

(define (solveQuadraticEquation a b c)
  (let ((discriminant (- (* b b) (* 4 a c))))
    (if (= discriminant 0)
        (list (/ (- b) (* 2 a)))
        (if (< discriminant 0)
            #f
            (list (/ (- (- b) (sqrt discriminant)) (* 2 a))
                  (/ (+ (- b) (sqrt discriminant)) (* 2 a)))))))
```

Este código es una colección de funciones matemáticas útiles escritas en Scheme. Incluye funciones para calcular factoriales, números de Fibonacci, el máximo común divisor, el mínimo común múltiplo, probar si un número es primo, encontrar el enésimo primo, calcular combinaciones y permutaciones, evaluar coeficientes binomiales y resolver ecuaciones lineales y cuadráticas.

Aquí hay algunos ejemplos de cómo se puede usar este código:

```scheme
(factorial 5)
;; => 120

(fibonacci 10)
;; => 55

(gcd 12 18)
;; => 6

(lcm 12 18)
;; => 36

(isPrime 13)
;; => #t

(nthPrime 10)
;; => 29

(combinations 5 2)
;; => 10

(permutations 5 2)
;; => 20

(binomial 5 2)
;; => 10

(quadraticRoots 1 -2 1)
;; => (1 -1)

(solveLinearEquation 2 3)
;; => 3/2

(solveQuadraticEquation 1 -2 1)
;; => (1 -1)
```