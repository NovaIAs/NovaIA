```scheme
;; Definición de la función factorial.
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

;; Definición de la función fibonacci.
(define (fibonacci n)
  (if (or (zero? n) (eq? n 1))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; Definición de la función gcd (greatest common divisor).
(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))

;; Definición de la función lcm (least common multiple).
(define (lcm a b)
  (* a (/ b (gcd a b))))

;; Definición de la función que convierte un número entero a binario.
(define (to-binary n)
  (if (zero? n)
      "0"
      (let ((rem (remainder n 2)))
        (string-append (to-binary (/ n 2)) (string rem)))))

;; Definición de la función que convierte un número binario a entero.
(define (from-binary n)
  (let loop ((n n) (result 0) (pow 1))
    (if (zero? n)
        result
        (loop (/ n 2) (+ result (* pow (remainder n 2))) (* pow 2)))))

;; Definición de la función que ordena una lista de números enteros.
(define (sort lst)
  (if (null? lst)
      '()
      (let ((pivot (car lst))
            (smaller (filter (lambda (x) (< x pivot)) (cdr lst)))
            (larger (filter (lambda (x) (>= x pivot)) (cdr lst))))
        (append (sort smaller) (list pivot) (sort larger)))))

;; Definición de la función que busca un elemento en una lista.
(define (find lst elem)
  (if (null? lst)
      #f
      (if (eqv? (car lst) elem)
          lst
          (find (cdr lst) elem))))

;; Definición de la función que invierte una lista.
(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst)) (list (car lst)))))

;; Definición de la función que aplana una lista anidada.
(define (flatten lst)
  (let loop ((lst lst) (result '()))
    (if (null? lst)
        result
        (if (list? (car lst))
            (loop (append (car lst) (cdr lst)) result)
            (loop (cdr lst) (append (list (car lst)) result))))))

;; Definición de la función que calcula la suma de los elementos de una lista.
(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

;; Definición de la función que calcula el producto de los elementos de una lista.
(define (product lst)
  (if (null? lst)
      1
      (* (car lst) (product (cdr lst)))))

;; Definición de la función que calcula la media de los elementos de una lista.
(define (average lst)
  (/ (sum lst) (length lst)))

;; Definición de la función que calcula la varianza de los elementos de una lista.
(define (variance lst)
  (let ((avg (average lst)))
    (sum (map (lambda (x) (** (- x avg) 2)) lst))))

;; Definición de la función que calcula la desviación estándar de los elementos de una lista.
(define (standard-deviation lst)
  (sqrt (variance lst)))