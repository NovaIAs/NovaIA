```scheme
(define (fac n)
  (if (= n 0)
      1
      (* n (fac (- n 1)))))

(define (sum n)
  (if (= n 0)
      0
      (+ n (sum (- n 1)))))

(define (resta n)
  (if (= n 0)
      0
      (- n (resta (- n 1)))))

(define (multi n)
  (if (= n 0)
      0
      (* n (multi (- n 1)))))

(define (divi n)
  (if (= n 0)
      0
      (/ n (divi (- n 1)))))

(define (pot n)
  (if (= n 0)
      0
      (** n (pot (- n 1)))))

(define (primo n)
  (if (<= n 1)
      false
      (cond
        [(= n 2) true]
        [(= n 3) true]
        [(= n 5) true]
        [(= n 7) true]
        [(= n 11) true]
        [(= n 13) true]
        [(= n 17) true]
        [(= n 19) true]
        [(= n 23) true]
        [(= n 29) true]
        [else false]))))

(define (fib n)
  (if (<= n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (mcd n m)
  (if (= n 0)
      m
      (mcd (- m n) n)))

(define (mcm n m)
  (* n m) (/ n (mcd n m))))

(define (combinaciones n k)
  (/ (fac n) (* (fac (- n k)) (fac k)))))

(define (permutaciones n k)
  (* (fac n) (/ n (- n k)))))

(define (binario n)
  (if (= n 0)
      ""
      (string-append (binario (quotient n 2)) (string (remainder n 2)))))

(define (hexadecimal n)
  (if (= n 0)
      ""
      (string-append (hexadecimal (quotient n 16)) (string (remainder n 16)))))

(define (octal n)
  (if (= n 0)
      ""
      (string-append (octal (quotient n 8)) (string (remainder n 8)))))
```

Este código contiene una colección de funciones matemáticas comunes en Scheme, incluyendo funciones para calcular el factorial, la suma, la resta, la multiplicación, la división, la potencia, la primalidad, la serie de Fibonacci, el máximo común divisor, el mínimo común múltiplo, las combinaciones, las permutaciones, la conversión de números a binario, hexadecimal y octal, y la comprobación de si un número es primo.