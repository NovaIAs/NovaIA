Claro! Aqui está um exemplo de código complexo em Scheme:

```scheme
;; Função para calcular o n-ésimo número de Fibonacci
(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (iter i)
    (if (> i (sqrt n))
        #t
        (if (zero? (modulo n i))
            #f
            (iter (+ i 1)))))
  (iter 2))

;; Função para calcular o fatorial de um número
(define (fatorial n)
  (define (iter i acc)
    (if (> i n)
        acc
        (iter (+ i 1) (* acc i))))
  (iter 1 1))

;; Função para calcular a soma dos dígitos de um número
(define (soma-digitos n)
  (define (iter num acc)
    (if (zero? num)
        acc
        (iter (quotient num 10) (+ acc (modulo num 10)))))
  (iter n 0))

;; Função para inverter uma lista
(define (inverter-lista lst)
  (define (iter l acc)
    (if (null? l)
        acc
        (iter (cdr l) (cons (car l) acc))))
  (iter lst '()))

;; Função para calcular o número de divisores de um número
(define (num-divisores n)
  (define (iter i count)
    (if (> i n)
        count
        (if (zero? (modulo n i))
            (iter (+ i 1) (+ count 1))
            (iter (+ i 1) count))))
  (iter 1 0))

;; Exemplo de uso das funções
(displayln "O 10º número de Fibonacci é:")
(displayln (fibonacci 10))
(newline)

(displayln "O número 13 é primo?")
(displayln (primo? 13))
(newline)

(displayln "O fatorial de 5 é:")
(displayln (fatorial 5))
(newline)

(displayln "A soma dos dígitos de 1234 é:")
(displayln (soma-digitos 1234))
(newline)

(displayln "A lista [1 2 3 4 5] invertida é:")
(displayln (inverter-lista '(1 2 3 4 5)))
(newline)

(displayln "O número 20 possui quantos divisores?")
(displayln (num-divisores 20))
(newline))
```

Neste exemplo, apresentei 6 funções diferentes em Scheme:

- A função `fibonacci` calcula o n-ésimo número de Fibonacci recursivamente.
- A função `primo?` verifica se um número é primo.
- A função `fatorial` calcula o fatorial de um número usando uma função auxiliar recursiva.
- A função `soma-digitos` calcula a soma dos dígitos de um número.
- A função `inverter-lista` inverte uma lista recursivamente.
- A função `num-divisores` calcula o número de divisores de um número.

Essas funções podem ser usadas individualmente para realizar diferentes tarefas matemáticas.