```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))]))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (is-prime n)
  (if (< n 2)
      #f
      (cond
        [(modulo n 2) #f]
        [(modulo n 3) #f]
        [else
         (let loop ((i 3))
           (if (> i (sqrt n))
               #t
               (if (modulo n i)
                   (loop (+ i 2))
                   #f))))]))

(define (nth-prime n)
  (define (inner i count)
    (if (= count n)
        i
        (if (is-prime i)
            (inner (+ i 1) (+ count 1))
            (inner (+ i 1) count))))
  (inner 2 0))

(define (binomial-coefficient n k)
  (if (or (< k 0) (> k n))
      0
      (/ (factorial n)
         (* (factorial k) (factorial (- n k))))))

(define (monte-carlo-pi n)
  (define (inner count x y)
    (if (= count n)
        (* 4 (/ (+ x y) n))
        (inner (+ count 1)
               (if (< (random) (/ y x)) + x -)
               (if (< (random) (/ x y)) + y -))))
  (inner 0 1 0))

(define (checksum str)
  (let loop ((chars (string->list str))
              (sum 0))
    (if (null? chars)
        sum
        (loop (cdr chars)
              (+ sum (car chars))))))

(define (caesar-cipher str key)
  (string-map (lambda (char)
                (if (char-alphabetic? char)
                    (char (+ (char->integer char) key)
                              (if (char-upper-case? char) 'A 'a))
                    char))
              str))

(define (vigenere-cipher str key)
  (string-map (lambda (char)
                (if (char-alphabetic? char)
                    (char (+ (char->integer char)
                               (char->integer (string-ref key
                                                 (modulo (- (current-input-port)
                                                           (string-length key))
                                                        (string-length key)))))
                              (if (char-upper-case? char) 'A 'a))
                    char))
              str))

(define-syntax (with-open-file args expr ...)
  (syntax-rules ()
    ((_ (file-name | (port port)) #?@args) expr ...)
     (let ((port (if (symbol? file-name)
                    file-name
                    (open-input-file file-name args))))
       (with-input-from-port port expr ...)
       (close-input-port port)))))
```

Este código es un conjunto de funciones útiles en Scheme. Incluye funciones para calcular el factorial, la serie de Fibonacci, el máximo común divisor (MCD), el mínimo común múltiplo (MCM), comprobar si un número es primo, encontrar el enésimo primo, calcular el coeficiente binomial, estimar el valor de pi utilizando el método de Montecarlo, calcular la suma de comprobación de una cadena, cifrar una cadena utilizando el cifrado César o el cifrado de Vigenère, y abrir y cerrar un archivo para leer de él.