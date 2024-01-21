```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (suma-cuadrados n)
  (if (= n 0)
      0
      (+ (expt n 2) (suma-cuadrados (- n 1)))))

(define (suma-cubos n)
  (if (= n 0)
      0
      (+ (expt n 3) (suma-cubos (- n 1)))))

(define (suma-pares n)
  (if (= n 0)
      0
      (+ n (suma-pares (- n 2)))))

(define (suma-impares n)
  (if (= n 0)
      0
      (+ n (suma-impares (- n 2)))))

(define (suma-primos n)
  (define (es-primo? n)
    (if (or (= n 1) (= n 2))
        True
        (and (not (= n 2))
             (null? (filter (lambda (x) (= 0 (modulo n x)))
                            (range 2 (sqrt n)))))))

  (if (es-primo? n)
      (+ n (suma-primos (- n 1)))
      (suma-primos (- n 1))))

(define (suma-perfectos n)
  (define (es-perfecto? n)
    (let loop ((i 1) (suma 0))
      (if (= i (sqrt n))
          (= suma n)
          (loop (+ i 1) (+ suma i)))))

  (if (es-perfecto? n)
      (+ n (suma-perfectos (- n 1)))
      (suma-perfectos (- n 1))))

(define (suma-abundantes n)
  (define (es-abundante? n)
    (let loop ((i 1) (suma 0))
      (if (= i (sqrt n))
          (> suma n)
          (loop (+ i 1) (+ suma (cond ((= (modulo n i) 0) i)))))))

  (if (es-abundante? n)
      (+ n (suma-abundantes (- n 1)))
      (suma-abundantes (- n 1))))

(define (suma-deficientes n)
  (define (es-deficiente? n)
    (let loop ((i 1) (suma 0))
      (if (= i (sqrt n))
          (< suma n)
          (loop (+ i 1) (+ suma (cond ((= (modulo n i) 0) i)))))))

  (if (es-deficiente? n)
      (+ n (suma-deficientes (- n 1)))
      (suma-deficientes (- n 1))))

(define (suma-armstrong n)
  (define (es-armstrong? n)
    (let ((longitud (string-length (number->string n)))
         (suma 0))
      (let loop ((digito (string-ref (number->string n) 0)))
        (if (= digito '())
            (= suma n)
            (loop (string-tail (string-tail (string-ref (number->string n) 0)))
                 (+ suma (expt digito longitud)))))))

  (if (es-armstrong? n)
      (+ n (suma-armstrong (- n 1)))
      (suma-armstrong (- n 1))))

(define (suma-narcisistas n)
  (define (es-narcisista? n)
    (let ((longitud (string-length (number->string n)))
         (suma 0))
      (let loop ((digito (string-ref (number->string n) 0)))
        (if (= digito '())
            (= suma n)
            (loop (string-tail (string-tail (string-ref (number->string n) 0)))
                 (+ suma (expt digito longitud)))))))

  (if (es-narcisista? n)
      (+ n (suma-narcisistas (- n 1)))
      (suma-narcisistas (- n 1))))

(define (suma-felices n)
  (define (es-feliz? n)
    (let loop ((siguiente n))
      (let ((suma 0))
        (let loop2 ((digito (string-ref (number->string siguiente) 0)))
          (if (= digito '())
              (or (= suma 1) (es-feliz? suma))
              (loop2 (string-tail (string-ref (number->string siguiente) 0))
                   (+ suma (expt digito 2))))))
      (if (or (= siguiente 1) (= siguiente 7))
          True
          (loop (suma))))))

  (if (es-feliz? n)
      (+ n (suma-felices (- n 1)))
      (suma-felices (- n 1))))
```

Explicación:

* El código define una serie de funciones que calculan sumas de diferentes tipos de números.
* La función `factorial` calcula el factorial de un número.
* La función `suma-cuadrados` calcula la suma de los cuadrados de los números hasta un número dado.
* La función `suma-cubos` calcula la suma de los cubos de los números hasta un número dado.
* La función `suma-pares` calcula la suma de los números pares hasta un número dado.
* La función `suma-impares` calcula la suma de los números impares hasta un número dado.
* La función `suma-primos` calcula la suma de los números primos hasta un número dado.
* La función `suma-perfectos` calcula la suma de los números perfectos hasta un número dado.
* La función `suma-abundantes` calcula la suma de los números abundantes hasta un número dado.
* La función `suma-deficientes` calcula la suma de los números deficientes hasta un número dado.
* La función `suma-armstrong` calcula la suma de los números de Armstrong hasta un número dado.
* La función `suma-narcisistas` calcula la suma de los números narcisistas hasta un número dado.
* La función `suma-felices` calcula la suma de los números felices hasta un número dado.