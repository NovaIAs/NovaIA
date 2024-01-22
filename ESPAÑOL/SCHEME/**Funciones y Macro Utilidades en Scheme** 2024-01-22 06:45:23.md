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
    (gcd b (remainder a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (is-prime? n)
  (if (<= n 1)
    false
    (let loop ((i 2))
      (if (= i (* i i))
        true
        (if (= (remainder n i) 0)
          false
          (loop (+ i 1)))))))

(define (sqrt x)
  (let loop ((guess 1))
    (if (< (abs (- guess (/ guess x))) 0.001)
      guess
      (loop (/ (+ guess (/ x guess)) 2)))))

(define (integrate f a b n)
  (/ (- (f b) (f a)) n))

(define (diff f x h)
  (/ (- (f (+ x h)) (f x)) h))

(define (solve-equation f x0)
  (let loop ((x x0))
    (if (= (f x) 0)
      x
      (loop (- x (/ (f x) (diff f x 0.001)))))))

(define (print-table data)
  (for-each (lambda (row) (for-each (lambda (item) (display item) (display " ")) row) (display "\n")) data))

(define-syntax-rule (do-while (cond body ...))
  (let loop continue-placeholder
    (syntax-case cond
      ((condition . body)
       (syntax
         (let continue
           (if condition
             (begin body continue)
             continue-placeholder))))
      (else
       (syntax-error "do/while requires a condition and a body expression")))))

(define-syntax-rule (do-until (cond body ...)
  (let loop continue-placeholder
    (syntax-case cond
      ((condition . body)
       (syntax
         (let continue
           (if (not condition)
             (begin body continue)
             continue-placeholder))))
      (else
       (syntax-error "do/until requires a condition and a body expression")))))

(define-syntax-rule (for ((var . expr) (rest . expr)) body ...)
  (let loop
    (if expr
      (syntax
        (let-values (((var (car expr)) (rest (cdr expr))))
          (for ((var (car rest)) (rest (cdr rest)))
            body ...)))
      (syntax
        (doall (list (cdr var))
          body ...)))))

(define-syntax-rule (for* ((var . expr) (rest . expr)) body ...)
  (let loop
    (syntax
      (for ((var (car expr)))
        (for* ((var (car rest)) (rest (cdr rest)))
          body ...)))))

(define-macro (macro-quote args &body body)
  (syntax-case args
    ((args*)
     (syntax
       (let arglist
         (syntax-case args
           ((args)
            (values 'list))
           (else
            (values 'list args)))))
    (else
     (syntax-error "macroquote: syntax error"))))

(define-macro (syntax-quote args &body body)
  (syntax-case args
    ((args*)
     (syntax
       (list 'syntax-quote (let arglist
                            (syntax-case args
                              ((args)
                               (values 'list))
                              (else
                               (values 'list args))))
             body ...)))
    (else
     (syntax-error "syntax-quote: syntax error"))))

(define-syntax-rule (and)
  (syntax-case (syntax->list)
    (null
     (syntax
       #t))
    ((a b ...)
     (syntax
       `(if ,a (if ,b ... #f)))))

(define-syntax-rule (or)
  (syntax-case (syntax->list)
    (null
     (syntax
       #f))
    ((a b ...)
     (syntax
       `(if ,a #t (if ,b ... #f)))))

(define-syntax-rule (let ((var . expr) ...) body ...)
  (let loop
    (if expr
      (syntax
        (let ((var (car expr)) (rest (cdr expr))))
          (let ((var (eval var)))
            (for ((var (car rest)) (rest (cdr rest)))
              body ...))))
      (syntax
        (doall (list (cdr var))
          body ...)))))

(define-syntax-rule (lambda (args . expr) body ...)
  (let loop
    (if expr
      (syntax
        (let ((arg (car args)) (rest (cdr args))))
          (lambda ((arg (eval arg)))
            (for ((arg (car rest)) (rest (cdr rest)))
              body ...))))
      (syntax
        (lambda ()
          body ...)))))

(define-syntax-rule (define (var args . expr) body ...)
  (let loop
    (if expr
      (syntax
        (let ((arg (car args)) (rest (cdr args))))
          (define-values ((arg (eval arg)))
            (for ((arg (car rest)) (rest (cdr rest)))
              body ...))))
      (syntax
        (define var
          (for ((var (car args)) (rest (cdr args)))
            body ...))))))
```

Este código contiene una variedad de funciones y macro utilidades en Scheme. Incluye funciones para calcular factorial, Fibonacci, máximo común divisor, mínimo común múltiplo, primalidad, raíz cuadrada, integración, derivación, resolución de ecuaciones, impresión de tablas, bucles while, until, for y for*, macroquote, syntax-quote, and, or, let, lambda y define. Estas funciones y macro utilidades pueden ser utilizadas para resolver una amplia gama de problemas de programación.