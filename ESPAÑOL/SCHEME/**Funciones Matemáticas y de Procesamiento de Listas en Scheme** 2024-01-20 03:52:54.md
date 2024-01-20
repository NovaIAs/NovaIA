```scheme

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (suma-cuadrados n)
  (if (= n 1)
      1
      (+ (* n n) (suma-cuadrados (- n 1)))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (potencia x n)
  (if (= n 0)
      1
      (* x (potencia x (- n 1)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (% a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (ordenar lista)
  (if (null? lista)
      ()
      (let loop ((lista (cdr lista))
                (resultado (car lista)))
        (if (null? lista)
            (list resultado)
            (cons resultado (loop (cdr lista) (max resultado (car lista)))))))

(define (buscar elemento lista)
  (if (null? lista)
      #f
      (if (= elemento (car lista))
          (car lista)
          (buscar elemento (cdr lista)))))

(define (eliminar elemento lista)
  (if (null? lista)
      ()
      (let loop ((lista (cdr lista))
                (resultado (car lista)))
        (if (= elemento resultado)
            (loop lista (car lista))
            (cons resultado (loop lista (car lista)))))))

(define (concatenar lista1 lista2)
  (if (null? lista1)
      lista2
      (cons (car lista1) (concatenar (cdr lista1) lista2))))

(define (invertir lista)
  (if (null? lista)
      ()
      (cons (car lista) (invertir (cdr lista)))))

(define (dec-a-bin numero)
  (if (= numero 0)
      "0"
      (let loop ((numero (quotient numero 2))
                 (resultado ""))
        (if (= numero 0)
            (reverse resultado)
            (let ((digito (remainder numero 2)))
              (loop (quotient numero 2) (cons (number->string digito) resultado)))))))

(define (bin-a-dec numero)
  (if (null? numero)
      0
      (+ (* (string->number (car numero)) (expt 2 (- (length numero) 1)))
         (bin-a-dec (cdr numero)))))

(define (es-primo numero)
  (if (< numero 2)
      #f
      (let loop ((divisor 2)
                 (es-primo #t))
        (if (>= divisor (sqrt numero))
            es-primo
            (if (= 0 (remainder numero divisor))
                #f
                (loop (+ divisor 1) es-primo))))))

(define (primos-hasta n)
  (if (= n 0)
      ()
      (cons n (primos-hasta (- n 1)))))

```

Este código implementa varias funciones matemáticas y de procesamiento de listas en Scheme.

* `factorial`: Calcula el factorial de un número.
* `suma-cuadrados`: Calcula la suma de los cuadrados de los números hasta un número dado.
* `fibonacci`: Calcula el n-ésimo número de Fibonacci.
* `potencia`: Calcula la potencia de un número a un exponente dado.
* `gcd`: Calcula el máximo común divisor de dos números.
* `lcm`: Calcula el mínimo común múltiplo de dos números.
* `ordenar`: Ordena una lista de números en orden ascendente.
* `buscar`: Busca un elemento en una lista.
* `eliminar`: Elimina un elemento de una lista.
* `concatenar`: Concatena dos listas.
* `invertir`: Invierte una lista.
* `dec-a-bin`: Convierte un número decimal a binario.
* `bin-a-dec`: Convierte un número binario a decimal.
* `es-primo`: Comprueba si un número es primo.
* `primos-hasta`: Genera una lista de números primos hasta un número dado.