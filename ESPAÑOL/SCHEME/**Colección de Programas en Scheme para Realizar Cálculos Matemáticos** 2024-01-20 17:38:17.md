```scheme
;;; ``` cont-frac.scm ```: Calcula la fracción continua de un número.

(define (cont-frac n)
  (define (recur n acc)
    (cond [(= n 0) acc]
          [(= n 1) (cons 1 acc)]
          [else (cons (quotient n) (recur (remainder n) acc))]))
  (reverse (recur n '())))

(display (cont-frac 23))
(newline)

;;; ``` euclid.scm ```: Calcula el máximo común divisor de dos números.

(define (euclid a b)
  (cond [(= b 0) a]
        [else (euclid b (remainder a b))]))

(display (euclid 24 18))
(newline)

;;; ``` factorial.scm ```: Calcula el factorial de un número.

(define (factorial n)
  (cond [(= n 1) 1]
        [else (* n (factorial (sub1 n)))]))

(display (factorial 5))
(newline)

;;; ``` fibonacci.scm ```: Calcula la serie de Fibonacci hasta un número determinado.

(define (fibonacci n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fibonacci (sub1 n)) (fibonacci (sub2 n)))]))

(display (fibonacci 10))
(newline)

;;; ``` gcd.scm ```: Calcula el máximo común divisor de dos números usando el algoritmo de Euclides.

(define (gcd a b)
  (define (recur a b)
    (cond [(= b 0) a]
          [else (recur b (remainder a b))]))
  (recur a b))

(display (gcd 24 18))
(newline)

;;; ``` merge-sort.scm ```: Implementa el algoritmo de ordenación MergeSort.

(define (merge-sort lst)
  (cond [(null? lst) '()]
        [(null? (cdr lst)) lst]
        [else (merge (merge-sort (first-n lst (/ (length lst) 2)))
                     (merge-sort (rest-n lst (/ (length lst) 2))))]))

(define (merge lst1 lst2)
  (cond [(null? lst1) lst2]
        [(null? lst2) lst1]
        [(<= (car lst1) (car lst2)) (cons (car lst1) (merge (cdr lst1) lst2))]
        [else (cons (car lst2) (merge lst1 (cdr lst2)))]))

(display (merge-sort '(1 3 5 2 4 6)))
(newline)

;;; ``` newton.scm ```: Implementa el método de Newton para encontrar raíces de una función.

(define (newton f df x0)
  (define (recur x)
    (cond [(abs (f x)) < 1e-6) x]
          [else (recur (- x (/ (f x) (df x))))]))

  (recur x0))

(display (newton (lambda (x) (- x (* x x)))
               (lambda (x) (- 1 (* 2 x)))
               1.5))
(newline)
```

Este código es más complejo y consta de varias funciones distintas. Aquí hay una breve explicación de cada función:

* `cont-frac`: Calcula la fracción continua de un número.
* `euclid`: Calcula el máximo común divisor de dos números.
* `factorial`: Calcula el factorial de un número.
* `fibonacci`: Calcula la serie de Fibonacci hasta un número determinado.
* `gcd`: Calcula el máximo común divisor de dos números usando el algoritmo de Euclides.
* `merge-sort`: Implementa el algoritmo de ordenación MergeSort.
* `newton`: Implementa el método de Newton para encontrar raíces de una función.

Este código es más complejo y difícil de entender, pero también es más poderoso y versátil. Puede usarse para resolver una variedad de problemas diferentes.