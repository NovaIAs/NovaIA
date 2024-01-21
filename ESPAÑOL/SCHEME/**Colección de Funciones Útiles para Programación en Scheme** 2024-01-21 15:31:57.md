```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (fibonacci (- n 1))
             (fibonacci (- n 2))))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (prime? n)
  (if (<= n 1)
      #f
      (for*/and ([i (in-range 2 (floor (sqrt n))))
                 ([divisor (in-range 2 i)])
         (not (= (remainder n divisor) 0))))))

(define (sieve n)
  (for/list ([i (in-range 2 n)])
     (if (prime? i)
         i)))

(define (is-palindrome? s)
  (= (reverse s) s))

(define (caesar-cipher s n)
  (let loop ((s s) (n n) (result ""))
    (if (null? s)
        result
        (let ((char (car s)))
          (if (and (char-alphabetic? char)
                   (< (+ char n)
                      (if (char-uppercase? char)
                          'Z
                          'z)))
              (char (+ char n))
              char)
          (loop (cdr s) (modulo n 26)
                (string-append result (list->string
                                        (list (car s))))))))))

(define (merge-sort xs)
  (if (null? xs)
      '()
      (let loop ((xs xs) (result '()))
        (if (null? xs)
            result
            (let ((mid (quotient (length xs) 2)))
              (loop (append (merge-sort (take xs mid))
                            (merge-sort (drop xs mid)))
                    (cons (smallest (car xs) (car (cdr xs))) result)))))))

(define (smallest x y)
  (if (< x y)
      x
      y))

(define (quick-sort xs)
  (if (null? xs)
      '()
      (let loop ((xs xs) (result '()))
        (if (null? xs)
            result
            (let ((pivot (car xs))
                  (left  (for*/filter ([x (cdr xs)])
                           (< x pivot)))
                  (right (for*/filter ([x (cdr xs)])
                            (>= x pivot))))
              (append (quick-sort left)
                     (list pivot)
                     (quick-sort right)))))))
```

Este código es una colección de funciones útiles en Scheme, algunas de las cuales son:

* `factorial`: calcula el factorial de un número.
* `fibonacci`: calcula el número de Fibonacci de un número.
* `gcd`: calcula el máximo común divisor de dos números.
* `lcm`: calcula el mínimo común múltiplo de dos números.
* `prime?`: comprueba si un número es primo.
* `sieve`: genera una lista de números primos hasta un límite especificado.
* `is-palindrome?`: comprueba si una cadena es un palíndromo.
* `caesar-cipher`: cifra una cadena usando el cifrado César.
* `merge-sort`: ordena una lista usando el algoritmo de ordenación por fusión.
* `quick-sort`: ordena una lista usando el algoritmo de ordenación rápida.