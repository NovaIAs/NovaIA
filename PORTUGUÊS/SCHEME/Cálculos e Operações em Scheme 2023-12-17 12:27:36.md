Claro! Aqui está um exemplo complexo de código em Scheme:

```scheme
;;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;;; Função para calcular a sequência de Fibonacci
(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;;; Função para calcular a soma dos dígitos de um número
(define (soma-digitos n)
  (if (zero? n)
      0
      (+ (remainder n 10) (soma-digitos (quotient n 10)))))

;;; Função para verificar se um número é primo
(define (primo? n)
  (if (< n 2)
      #f
      (let loop ((i 2))
        (if (<= i (floor (sqrt n)))
            (if (zero? (remainder n i))
                #f
                (loop (+ i 1)))
            #t))))

;;; Função para inverter uma lista
(define (inverter-lista lst)
  (define (inverter lst resultado)
    (if (null? lst)
        resultado
        (inverter (cdr lst) (cons (car lst) resultado))))
  (inverter lst '()))

;;; Função para encontrar a mediana de uma lista
(define (mediana lst)
  (let ((tamanho (length lst)))
    (if (even? tamanho)
        (/ (+ (list-ref lst (quotient tamanho 2))
              (list-ref lst (- (quotient tamanho 2) 1)))
           2)
        (list-ref lst (quotient tamanho 2)))))

;;; Função para verificar se uma lista está ordenada
(define (lista-ordenada? lst)
  (let loop ((lst lst))
    (cond ((or (null? lst) (null? (cdr lst)))
           #t)
          ((> (car lst) (cadr lst))
           #f)
          (else
           (loop (cdr lst))))))

;;; Função para calcular o máximo divisor comum (MDC) de dois números
(define (mdc a b)
  (if (= b 0)
      a
      (mdc b (remainder a b))))

;;; Função para calcular o mínimo múltiplo comum (MMC) de dois números
(define (mmc a b)
  (/ (abs (* a b)) (mdc a b)))

;;; Função para gerar um número aleatório entre dois limites
(define (aleatorio limite-inferior limite-superior)
  (+ limite-inferior (random (- limite-superior limite-inferior))))

;;; Função principal para testar as funções acima
(define (main)
  (display "Digite um número inteiro positivo: ")
  (let ((numero (read)))
    (display "O fatorial de ")
    (display numero)
    (display " é ")
    (display (fatorial numero))
    (newline)
    
    (display "A sequência de Fibonacci de tamanho ")
    (display numero)
    (display " é ")
    (display (fibonacci numero))
    (newline)
    
    (display "A soma dos dígitos de ")
    (display numero)
    (display " é ")
    (display (soma-digitos numero))
    (newline)
    
    (display "O número ")
    (display numero)
    (if (primo? numero)
        (display " é primo.")
        (display " não é primo."))
    (newline)
    
    (display "A lista invertida de ")
    (display numero)
    (display " é ")
    (display (inverter-lista (number->list numero)))
    (newline)
    
    (display "A mediana da lista ")
    (display numero)
    (display " é ")
    (display (mediana (number->list numero)))
    (newline)
    
    (display "A lista ")
    (display numero)
    (if (lista-ordenada? (number->list numero))
        (display " está ordenada.")
        (display " não está ordenada."))
    (newline)
    
    (display "Digite outro número inteiro positivo: ")
    (let ((outro-numero (read)))
      (display "O MDC de ")
      (display numero)
      (display " e ")
      (display outro-numero)
      (display " é ")
      (display (mdc numero outro-numero))
      (newline)
      
      (display "O MMC de ")
      (display numero)
      (display " e ")
      (display outro-numero)
      (display " é ")
      (display (mmc numero outro-numero))
      (newline)
      
      (display "Um número aleatório entre ")
      (display numero)
      (display " e ")
      (display outro-numero)
      (display " é ")
      (display (aleatorio numero outro-numero))
      (newline)))))
      
(main)
```

Este código em Scheme contém várias funções diferentes para realizar diferentes cálculos e operações. Aqui está uma breve explicação das principais funções:

- `fatorial`: Calcula o fatorial de um número.
- `fibonacci`: Calcula a sequência de Fibonacci de um tamanho específico.
- `soma-digitos`: Calcula a soma dos dígitos de um número.
- `primo?`: Verifica se um número é primo.
- `inverter-lista`: Inverte uma lista.
- `mediana`: Calcula a mediana de uma lista.
- `lista-ordenada?`: Verifica se uma lista está ordenada.
- `mdc`: Calcula o máximo divisor comum (MDC) de dois números.
- `mmc`: Calcula o mínimo múltiplo comum (MMC) de dois números.
- `aleatorio`: Gera um número aleatório dentro de dois limites específicos.
- `main`: Função principal que permite testar todas as funções acima.

O código solicita que o usuário digite um número inteiro positivo e, em seguida, realiza uma série de cálculos e operações com esse número, exibindo os resultados na saída.