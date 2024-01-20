```scheme

;; Importar módulos necesarios
(use (srfi srfi-1) (srfi srfi-69))

;; Definir alias
(define + (curry +))
(define - (curry -))
(define * (curry *))
(define / (curry /))
(define log (curry log))
(define exp (curry exp))
(define sqrt (curry sqrt))
(define abs (curry abs))

;; Definir funciones útiles
(define (es-par? x) (= (remainder x 2) 0))
(define (factorial x) (if (zero? x) 1 (* x (factorial (- x 1)))))
(define (fibonacci x) (if (< x 2) x (+ (fibonacci (- x 1)) (fibonacci (- x 2)))))
(define (gcd x y) (if (zero? y) x (gcd y (remainder x y))))
(define (lcm x y) (* (abs x) (abs y) (/ (gcd x y))))

;; Definir una clase de fracción
(define-struct fraccion (numerador denominador))

(define-syntax (make-fraccion numerador denominador)
  (cons 'fraccion (cons numerador denominador)))

(define-syntax (numerador fraccion)
  (car fraccion))

(define-syntax (denominador fraccion)
  (cdr fraccion))

(define (fraccion->real fraccion)
  (/ (numerador fraccion) (denominador fraccion)))

(define (fraccion? x)
  (cond ((consp x)
         (and (= (car x) 'fraccion)
              (integer? (numerador x))
              (integer? (denominador x))
              (> (denominador x) 0)))
        (else #f)))

(define (sumar-fracciones a b)
  (make-fraccion
   (+ (* (numerador a) (denominador b))
      (* (numerador b) (denominador a)))
   (* (denominador a) (denominador b))))

(define (restar-fracciones a b)
  (sumar-fracciones a (make-fraccion (- (numerador b))
                                     (denominador b))))

(define (multiplicar-fracciones a b)
  (make-fraccion (* (numerador a) (numerador b))
                 (* (denominador a) (denominador b))))

(define (dividir-fracciones a b)
  (multiplicar-fracciones a (make-fraccion (denominador b)
                                            (numerador b))))

;; Definir una clase de matriz
(define-struct matriz (filas columnas valores))

(define-syntax (make-matriz filas columnas valores)
  (cons 'matriz (cons (cons filas columnas) valores)))

(define-syntax (filas matriz)
  (car (car matriz)))

(define-syntax (columnas matriz)
  (cdr (car matriz)))

(define-syntax (valores matriz)
  (cdr matriz))

;; Definir operaciones de matriz
(define (trasponer matriz)
  (make-matriz (columnas matriz) (filas matriz)
               (map (lambda (fila) (map (lambda (x) (list x)) fila))
                    (valores matriz))))

(define (sumar-matrices a b)
  (make-matriz (filas a) (columnas a)
               (map (lambda (fila) (map (lambda (x y) (+ x y)) fila (cdr fila)))
                    (valores a) (valores b))))

(define (restar-matrices a b)
  (sumar-matrices a (map (lambda (fila) (map (lambda (x) (- x)) fila))
                         (valores b))))

(define (multiplicar-matrices a b)
  (let loop ((valores-a (valores a))
             (valores-b (valores b))
             (resultado '()))
    (if (empty? valores-a)
        resultado
        (let* ((fila (car valores-a))
               (resto (cdr valores-a))
               (fila-b (car valores-b))
               (resto-b (cdr valores-b)))
          (cons (map (lambda (x) (apply + (map (lambda (y) (* x y)) fila-b))) fila)
                (loop resto resto-b resultado)))))))

;; Definir una clase de polinomio
(define-struct polinomio (coeficientes))

(define-syntax (make-polinomio coeficientes)
  (cons 'polinomio (cons coeficientes)))

(define-syntax (coeficientes polinomio)
  (car polinomio))

(define (polinomio? x)
  (cond ((consp x)
         (and (= (car x) 'polinomio)
              (for-each integer? (coeficientes x))))
        (else #f)))

(define (grado polinomio)
  (- (length (coeficientes polinomio)) 1))

(define (evaluar-polinomio polinomio x)
  (apply + (map (lambda (coef exp) (* coef (expt x exp)))
                 (coeficientes polinomio) (iota (grado polinomio)))))

;; Definir operaciones de polinomio
(define (sumar-polinomios a b)
  (make-polinomio (map + (coeficientes a) (coeficientes b))))

(define (restar-polinomios a b)
  (sumar-polinomios a (make-polinomio (map (lambda (x) (- x))
                                           (coeficientes b)))))

(define (multiplicar-polinomios a b)
  (make-polinomio
   (for*/fold ((resultado (make-vector (+ (length (coeficientes a))
                                               (length (coeficientes b)))
                                        0))
                ((i 0) (j 0) (k 0))
                (when (>= j (length (coeficientes a)))
                  (set! j 0)
                  (set! k (+ k 1)))
                (when (>= k (length (coeficientes b)))
                  (return resultado))
                (when (< i (length resultado))
                  (set! (vector-ref resultado i)
                        (+ (vector-ref resultado i)
                           (* (vector-ref (coeficientes a) j)
                              (vector-ref (coeficientes b) k))))
                  (set! i (+ i 1))
                  (set! j (+ j 1)))))

;; Definir una clase de función
(define-struct funcion (nombre argumentos cuerpo entorno))

(define-syntax (make-funcion nombre argumentos cuerpo entorno)
  (cons 'funcion (cons (cons nombre argumentos) (cons cuerpo entorno))))

(define-syntax (nombre funcion)
  (car (car funcion)))

(define-syntax (argumentos funcion)
  (cdr (car funcion)))

(define-syntax (cuerpo funcion)
  (car (cdr funcion)))

(define-syntax (entorno funcion)
  (cdr (cdr funcion)))

(define (funcion? x)
  (cond ((consp x)
         (and (= (car x) 'funcion)
              (list? (argumentos x))
              (procedure? (cuerpo x))
              (alist? (entorno x))))
        (else #f)))

(define (aplicar-funcion funcion argumentos)
  ((cuerpo funcion)
   (map (lambda (arg) (eval arg (entorno funcion))) argumentos)))

;; Definir un intérprete
(define (interpretar expresion)
  (cond ((symbol? expresion)
         (lookup expresion (entorno (current-procedure))))
        ((list? expresion)
         (let ((operacion (car expresion)))
           (apply (lookup operacion (entorno (current-procedure)))
                  (map (lambda (arg) (interpretar arg)) (cdr expresion))))))
        (else expresion)))

;; Definir un intérprete de REPL
(define (repl)
  (display ">>> ")
  (flush-output-port (current-output-port))
  (interpretar (read-line)))

;; Bucle principal
(let loop ()
  (repl)
  (loop))

```