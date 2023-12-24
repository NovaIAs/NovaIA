Claro! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (= n 0)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular a sequência de Fibonacci
(define (fibonacci n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; Função principal para calcular a soma dos fatoriais dos números da sequência de Fibonacci
(define (soma-fatoriais-fibonacci n)
  (define (iter i sum)
    (if (>= i n)
        sum
        (iter (+ i 1) (+ sum (fatorial (fibonacci i))))))
  (iter 0 0))

;; Teste da função soma-fatoriais-fibonacci
(display "Digite um número inteiro não negativo: ")
(define n (read))
(if (>= n 0)
    (displayln (soma-fatoriais-fibonacci n))
    (displayln "Número inválido!"))
```

Neste código em Scheme, temos três funções principais:

1. `fatorial` é uma função recursiva que recebe um número `n` e retorna o fatorial desse número.

2. `fibonacci` é uma função recursiva que recebe um número `n` e retorna o `n`-ésimo número da sequência de Fibonacci.

3. `soma-fatoriais-fibonacci` é a função principal que recebe um número `n` e calcula a soma dos fatoriais dos números da sequência de Fibonacci até o `n`-ésimo número.

No final do código, há um teste para receber um número inteiro não negativo do usuário e chamar a função `soma-fatoriais-fibonacci` com esse número como argumento. O resultado é exibido na tela.

Espero que este exemplo atenda às suas expectativas!