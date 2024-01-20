```scheme
; Definición de una función recursiva para calcular el factorial de un número.

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; Definición de una función para calcular la suma de los cuadrados de una lista de números.

(define (suma-cuadrados lista)
  (if (null? lista)
      0
      (+ (square (car lista)) (suma-cuadrados (cdr lista)))))

; Definición de una función para calcular la raíz cuadrada de un número usando el método de Newton-Raphson.

(define (sqrt x)
  (define (f y) (- y (/ x y)))
  (define (df y) (- y (/ x (expt y 2))))
  (newton-raphson f df x))

; Definición de una función para calcular la aproximación de una integral usando el método de Simpson.

(define (simpson f a b n)
  (define (h (/ (- b a) n))
    (define (f0 (/ (f a) 2))
      (define (fn (/ (f b) 2))
        (define (suma-f 0)
          (do ((i (+ a h)))
            ((>= i b) suma-f)
            (set! suma-f (+ suma-f (f i)))))
        (+ f0 fn suma-f)))))
  (* h simpson))

; Definición de una función para calcular la transformada de Fourier de una señal.

(define (fourier señal)
  (define (frec (f) (* f (expt (1i) (* 2 pi * f * tiempo)))))
  (cons
    (* señal)
    (apply list (map frec (range 1 16))))))

```

Explicación:

* La primera función, `factorial`, calcula el factorial de un número usando recursividad.
* La segunda función, `suma-cuadrados`, calcula la suma de los cuadrados de una lista de números.
* La tercera función, `sqrt`, calcula la raíz cuadrada de un número usando el método de Newton-Raphson.
* La cuarta función, `simpson`, calcula la aproximación de una integral usando el método de Simpson.
* La quinta función, `fourier`, calcula la transformada de Fourier de una señal.