```scheme
(define-syntax convertir-a-numero
  (make-syntax-rule ()
    (lambda (exp)
      (let loop ((lst (cdr exp)) (result 0))
        (if (null? lst)
          result
          (loop (cdr lst) (+ (* 10 result) (number? (car lst)))))))))

(define-syntax saludar
  (make-syntax-rule ()
    (lambda (exp)
      (let ((nombre (cadr exp)))
        (displayln (format "Hola, %s!" nombre))))))

(define-syntax doblar-lista
  (make-syntax-rule ()
    (lambda (exp)
      (let ((lst (cadr exp)))
        (define doblar
          (lambda (lst result)
            (if (null? lst)
              result
              (doblar (cdr lst) (append result (list (* 2 (car lst)))))))

        (doblar lst '())))))

(define-syntax mostrar-mayor-que
  (make-syntax-rule ()
    (lambda (exp)
      (let ((lst (cadr exp)) (n (caddr exp)))
        (define filtrar
          (lambda (lst result)
            (if (null? lst)
              result
              (if (> (car lst) n)
                (filtrar (cdr lst) (cons (car lst) result))
                (filtrar (cdr lst) result)))))

        (define mostrar
          (lambda (lst)
            (displayln (format "Los números mayores que %d son: " n))
            (let loop ((lst lst))
              (if (null? lst)
                (displayln "No hay números mayores que " n)
                (begin
                  (display (car lst))
                  (display " ")
                  (loop (cdr lst))))))))

        (mostrar (filtrar lst '()))))))

(define-syntax fibonacci
  (make-syntax-rule ()
    (lambda (exp)
      (let ((n (cadr exp)))
        (define fib
          (lambda (n a b)
            (if (= n 0)
              a
              (if (= n 1)
                b
                (fib (- n 1) (+ a b) a)))))

        (fib n 0 1)))))

(define-syntax ordenar-lista
  (make-syntax-rule ()
    (lambda (exp)
      (let ((lst (cadr exp)))
        (define ordenar
          (lambda (lst)
            (if (null? lst)
              '()
              (let loop ((lst lst) (result nil))
                (if (null? lst)
                  (reverse result)
                  (loop (cdr lst) (cons (buscar-menor lst) result))))))

        (define buscar-menor
          (lambda (lst)
            (let loop ((lst lst) (menor (car lst)))
              (if (null? lst)
                menor
                (if (< (car lst) menor)
                  (loop (cdr lst) (car lst))
                  (loop (cdr lst) menor))))))

        (ordenar lst)))))
```

Explicación del código:

1. **convertir-a-número:** Esta sintaxis se utiliza para convertir una lista de caracteres que representan un número a un valor numérico.

2. **saludar:** Esta sintaxis se utiliza para generar una salida en la consola que saluda a una persona por su nombre.

3. **doblar-lista:** Esta sintaxis se utiliza para generar una nueva lista que contiene los elementos de la lista original duplicados.

4. **mostrar-mayor-que:** Esta sintaxis se utiliza para generar una salida en la consola que muestra los elementos de una lista que son mayores que un valor especificado.

5. **fibonacci:** Esta sintaxis se utiliza para generar el valor del número de Fibonacci en una posición dada.

6. **ordenar-lista:** Esta sintaxis se utiliza para ordenar los elementos de una lista en orden ascendente.