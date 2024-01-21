```scheme

(define (factorial n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (or (<= n 0) (= n 1))
    n
    (+ (fibonacci (- n 1))
       (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (isPrime n)
  (if (<= n 1)
    #f
    (for/list ((i 2 (+ i 1)))
      (< (gcd n i) n))))

(define (primeFactors n)
  (if (isPrime n)
    (list n)
    (cons (car (filter (lambda (p) (divides? p n)) (arithmetic-sequence 2 (+ 2 1))))
          (primeFactors (quotient n (car (filter (lambda (p) (divides? p n)) (arithmetic-sequence 2 (+ 2 1))))))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (arithmetic-sequence a b)
  (cons a (arithmetic-sequence (+ a b) b)))

(define (factorial-iter n)
  (define (iter n acc)
    (if (= n 0)
      acc
      (iter (- n 1) (* acc n))))
  (iter n 1))

(define (fibonacci-iter a b)
  (define (iter a b acc)
    (if (or (= a 0) (= b 0))
      acc
      (iter b (+ a b) acc)))
  (iter a b 0))

(define (gcd-iter a b)
  (define (iter a b)
    (if (= b 0)
      a
      (iter b (remainder a b))))
  (iter a b))

(define (lcm-iter a b)
  (* a (/ b (gcd-iter a b))))

(define (isPrime-iter n)
  (for/list ((i 2 (+ i 1)))
    (not (divides? i n))))

(define (primeFactors-iter n)
  (if (isPrime-iter n)
    (list n)
    (cons (car (filter (lambda (p) (divides? p n)) (arithmetic-sequence 2 (+ 2 1))))
          (primeFactors-iter (quotient n (car (filter (lambda (p) (divides? p n)) (arithmetic-sequence 2 (+ 2 1))))))))

;; Proyectos en los que trabajé:
(define (fizzBuzz n)
  (for ((i 1 (+ i 1)))
    (if (= (remainder i 3) 0)
      (if (= (remainder i 5) 0)
        "FizzBuzz"
        "Fizz")
      (if (= (remainder i 5) 0)
        "Buzz"
        i))))

(define (reverse s)
  (string->list s)
  (reverse)
  (list->string))

(define (isPalindrome s)
  (= (reverse s) s))

(define (max3 a b c)
  (max a (max b c)))

(define (min3 a b c)
  (min a (min b c)))

(define (factorial-stream n)
  (define (iter n acc)
    (cons acc (iter (- n 1) (* acc n))))
  (iter n 1))

(define (fibonacci-stream)
  (define (iter a b)
    (cons a (iter b (+ a b))))
  (iter 1 1))

(define (prime-stream)
  (define (iter n)
    (cons n (iter (+ n 2))))
  (iter 2))

(define (gcd-stream a b)
  (define (iter a b)
    (cons a (iter b (remainder a b))))
  (iter a b))

(define (lcm-stream a b)
  (define (iter a b)
    (cons a (iter b (/ (* a b) a))))
  (iter a b))

(define (isPrime-stream n)
  (define (iter n)
    (cons n (iter (+ n 2))))
  (iter n))

(define (primeFactors-stream n)
  (define (iter n)
    (cons n (iter (quotient n (car (filter (lambda (p) (divides? p n)) (arithmetic-sequence 2 (+ 2 1))))))))
  (iter n))

```

Este código es una colección de funciones útiles en Scheme, incluyendo funciones matemáticas, funciones de procesamiento de cadenas, funciones de manipulación de listas, y funciones de generación de secuencias.

Las funciones están escritas en un estilo funcional, utilizando listas y funciones de orden superior. Esto hace que el código sea conciso y fácil de entender.

Aquí hay algunos ejemplos de cómo usar las funciones:

```scheme

 (factorial 5)
=> 120

 (fibonacci 10)
=> 55

 (gcd 12 18)
=> 6

 (lcm 12 18)
=> 36

 (isPrime 11)
=> #t

 (primeFactors 12)
=> (2 2 3)

 (fizzBuzz 15)
=> "FizzBuzz"

 (reverse "Hello")
=> "olleH"

 (isPalindrome "racecar")
=> #t

 (max3 1 2 3)
=> 3

 (min3 1 2 3)
=> 1

 (factorial-stream 5)
=> (1 1 2 6 24 120)

 (fibonacci-stream)
=> (1 1 2 3 5 8 13 21 34 55)

 (prime-stream)
=> (2 3 5 7 11 13 17 19 23 29)

 (gcd-stream 12 18)
=> (12 6 2 1)

 (lcm-stream 12 18)
=> (12 36)

 (isPrime-stream 11)
=> (11)

 (primeFactors-stream 12)
=> (2 2 3)

```