;(1) Função para calcular o fatorial de um número
(define (fatorial n)
  (if (= n 0)
      1
      (* n (fatorial (- n 1)))))

;(2) Função para calcular a sequência de Fibonacci
(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

;(3) Função para verificar se um número é primo
(define (primo? n)
  (if (<= n 1)
      #f
      (define (divisivel? d)
        (= (modulo n d) 0))
      (define (iterador d)
        (cond ((> d (sqrt n)) #t)
              ((divisivel? d) #f)
              (else (iterador (+ d 1)))))
      (iterador 2)))

;(4) Função para verificar se uma string é um palíndromo
(define (palindromo? str)
  (define (inverter str)
    (if (null? str)
        '()
        (append (inverter (cdr str)) (list (car str)))))
  (equal? str (list->string (inverter (string->list str)))))

;(5) Função para calcular o máximo divisor comum (MDC) de dois números
(define (mdc a b)
  (define (iterador x y)
    (if (= y 0)
        x
        (iterador y (modulo x y))))
  (iterador (abs a) (abs b)))

;(6) Função para calcular o mínimo múltiplo comum (MMC) de dois números
(define (mmc a b)
  (/ (* (abs a) (abs b)) (mdc a b)))

;(7) Função para calcular a média de uma lista de números
(define (media lista)
  (/ (apply + lista) (length lista)))

;(8) Função para verificar se uma lista está ordenada de forma crescente
(define (ordenada? lista)
  (define (iterador lst)
    (cond ((null? lst) #t)
          ((null? (cdr lst)) #t)
          ((> (car lst) (cadr lst)) #f)
          (else (iterador (cdr lst)))))
  (iterador lista))

;(9) Função para calcular a potência de um número
(define (potencia base expoente)
  (if (= expoente 0)
      1
      (* base (potencia base (- expoente 1)))))

;(10) Função para calcular o quadrado de um número
(define (quadrado n)
  (* n n)))

;(11) Função para calcular a raiz quadrada de um número
(define (raiz-quadrada n)
  (define (iterador x)
    (if (<= (abs (- (* x x) n)) 0.0001)
        x
        (iterador (/ (+ (/ n x) x) 2))))
  (iterador 1.0))