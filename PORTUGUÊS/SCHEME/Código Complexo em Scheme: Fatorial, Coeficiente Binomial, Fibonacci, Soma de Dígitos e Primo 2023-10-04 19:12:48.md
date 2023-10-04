Claro! Aqui está um exemplo de código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (= n 0)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular o coeficiente binomial utilizando a fórmula n! / (k! * (n - k)!)
(define (coeficiente-binomial n k)
  (/ (fatorial n) (* (fatorial k) (fatorial (- n k)))))


;; Função para gerar a sequência de números de Fibonacci até o enésimo termo
(define (fibonacci n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; Função para calcular a soma dos dígitos de um número
(define (soma-digitos n)
  (if (< n 10)
      n
      (+ (modulo n 10) (soma-digitos (quotient n 10)))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (iter i)
    (cond ((> i (sqrt n)) #t)
          ((= (modulo n i) 0) #f)
          (else (iter (+ i 1)))))
  (if (< n 2)
      #f
      (iter 2)))

;; Exemplo de uso das funções
(displayln "O fatorial de 5 é: " (fatorial 5))
(displayln "O coeficiente binomial de 5 e 2 é: " (coeficiente-binomial 5 2))
(displayln "Os primeiros 10 números de Fibonacci são: ")
(do ((i 0 (+ i 1)))
    ((= i 10))
  (display (fibonacci i))
  (display " "))
(newline)
(displayln "A soma dos dígitos de 1234 é: " (soma-digitos 1234))
(displayln "O número 17 é primo? " (primo? 17))
```

Neste código em Scheme, são definidas diversas funções. A função `fatorial` calcula o fatorial de um número utilizando recursão. A função `coeficiente-binomial` utiliza a fórmula do coeficiente binomial para calcular o valor. A função `fibonacci` gera a sequência de Fibonacci até o enésimo termo utilizando recursão. A função `soma-digitos` calcula a soma dos dígitos de um número utilizando divisão e módulo. E, por fim, a função `primo?` verifica se um número é primo utilizando um loop iterativo.

No final do código, são feitos exemplos de uso das funções, como calcular o fatorial de 5, o coeficiente binomial de 5 e 2, gerar os primeiros 10 números de Fibonacci, calcular a soma dos dígitos de 1234 e verificar se o número 17 é primo.