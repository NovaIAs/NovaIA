```scheme
(define funcion-factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (funcion-factorial (- n 1))))))

(define funcion-fibonacci
  (lambda (n)
    (cond
      ((= n 0) 0)
      ((= n 1) 1)
      (else (+ (funcion-fibonacci (- n 1)) (funcion-fibonacci (- n 2)))))))

(define funcion-suma-cuadrados
  (lambda (n)
    (if (= n 0)
        0
        (+ (expt n 2) (funcion-suma-cuadrados (- n 1))))))

(define funcion-ordenamiento-burbuja
  (lambda (lista)
    (let loop
          ((lista lista)
           (ordenada? #f))
      (if (ordenada?)
          lista
          (let ((par lista)
                (ordenada? #t))
            (do ((i (car par))
                 (par (cdr par)))
              ((null? par)
               ordenada?)
              (if (> i (car par))
                  (begin
                    (set-car! par i)
                    (set-car! (cdr par) (car par))
                    (set-car! (cdr par) i)
                    (set! ordenada? #f))
                  #t)))))))

(define funcion-ordenamiento-insercion
  (lambda (lista)
    (if (null? (cdr lista))
        lista
        (let ((primero (car lista))
              (resto (cdr lista)))
          (cons (funcion-insertar-en-orden primero (funcion-ordenamiento-insercion resto))
                resto)))))

(define funcion-insertar-en-orden
  (lambda (elemento lista)
    (if (null? lista)
        (cons elemento lista)
        (cond
          ((> elemento (car lista))
           (cons elemento lista))
          ((< elemento (car lista))
           (cons (car lista) (funcion-insertar-en-orden elemento (cdr lista))))
          (else
           lista)))))

(define funcion-busqueda-binaria
  (lambda (elemento lista)
    (define loop
      ((izquierda 0)
       (derecha (- (length lista) 1)))
    (cond
      ((>= derecha izquierda)
       (let ((medio (quotient (+ izquierda derecha) 2)))
         (cond
           ((= (car lista medio) elemento)
            medio)
           ((< (car lista medio) elemento)
            (loop (+ medio 1) derecha))
           (else
            (loop izquierda (- medio 1))))))
      (else
       -1))))

(define funcion-arboles-binarios-de-busqueda
  (lambda (lista)
    (let loop
          ((lista lista)
           (arbol '()))
      (if (null? lista)
          arbol
          (let ((medio (quotient (length lista) 2)))
            (cons (car lista)
                  (cons (loop (take medio lista))
                        (loop (drop (+ medio 1) lista)))))))))

(define (take n lista)
  (cond
    ((= n 0) '())
    (else (cons (car lista) (take (- n 1) (cdr lista))))))

(define (drop n lista)
  (cond
    ((= n 0) lista)
    (else (drop (- n 1) (cdr lista))))))
```

Explicación del código:

1. **Función `funcion-factorial`**: Calcula el factorial de un número usando recursión.

2. **Función `funcion-fibonacci`**: Calcula el valor de la serie de Fibonacci para un índice dado usando recursión.

3. **Función `funcion-suma-cuadrados`**: Calcula la suma de los cuadrados de los números desde 1 hasta el número dado usando recursión.

4. **Función `funcion-ordenamiento-burbuja`**: Implementa el algoritmo de ordenamiento por burbuja para ordenar una lista.

5. **Función `funcion-ordenamiento-insercion`**: Implementa el algoritmo de ordenamiento por inserción para ordenar una lista.

6. **Función `funcion-insertar-en-orden`**: Inserta un elemento en una lista ordenada en orden ascendente.

7. **Función `funcion-busqueda-binaria`**: Realiza una búsqueda binaria en una lista ordenada para encontrar un elemento.

8. **Función `funcion-arboles-binarios-de-busqueda`**: Crea un árbol binario de búsqueda a partir de una lista.

9. **Funciones auxiliares `take` y `drop`**: Usadas para dividir una lista en dos sublistas.