Claro! Aqui está um exemplo de código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (= n 0)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular o número de Fibonacci de um dado índice
(define (fibonacci n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; Função para calcular o máximo divisor comum entre dois números
(define (mdc a b)
  (if (= b 0)
      a
      (mdc b (remainder a b))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (iter d)
    (cond ((= d 1) #t)
          ((= (remainder n d) 0) #f)
          (else (iter (- d 1)))))
  (iter (- n 1)))

;; Função para calcular a soma dos dígitos de um número
(define (soma-digitos n)
  (if (< n 10)
      n
      (+ (remainder n 10) (soma-digitos (quotient n 10)))))

;; Função para encontrar o menor elemento de uma lista
(define (menor-elemento lista)
  (define (iter menor restante)
    (if (null? restante)
        menor
        (iter (if (< (car restante) menor) (car restante) menor) (cdr restante))))
  (if (null? lista)
      (error "A lista está vazia!")
      (iter (car lista) (cdr lista))))

;; Exemplo de uso das funções

(display "O fatorial de 5 é: ")
(display (fatorial 5))
(newline)

(display "O número de Fibonacci de 8 é: ")
(display (fibonacci 8))
(newline)

(display "O MDC entre 24 e 36 é: ")
(display (mdc 24 36))
(newline)

(display "O número 7 é primo? ")
(display (primo? 7))
(newline)

(display "A soma dos dígitos de 12345 é: ")
(display (soma-digitos 12345))
(newline)

(display "O menor elemento da lista [4 2 8 1 5] é: ")
(display (menor-elemento '(4 2 8 1 5)))
(newline))
```

Neste exemplo, eu criei algumas funções em Scheme que realizam diferentes cálculos. A função `fatorial` calcula o fatorial de um número, a função `fibonacci` calcula o número de Fibonacci de um dado índice, a função `mdc` calcula o máximo divisor comum entre dois números, a função `primo?` verifica se um número é primo, a função `soma-digitos` calcula a soma dos dígitos de um número e a função `menor-elemento` encontra o menor elemento de uma lista.

No final do código, há exemplos de uso de cada função para demonstrar seu funcionamento.