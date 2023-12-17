Certamente! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Função que calcula o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Função que verifica se um número é primo
(define (primo? n)
  (define (iter i)
    (cond ((= i 1) #t)
          ((= (remainder n i) 0) #f)
          (else (iter (- i 1)))))
  (iter (floor (sqrt n))))

;; Função que retorna uma lista com todos os números primos menores que n
(define (primos-ate n)
  (define (iter i acc)
    (if (= i 1)
        acc
        (if (primo? i)
            (iter (- i 1) (cons i acc))
            (iter (- i 1) acc))))
  (iter n '()))

;; Função que verifica se um número é perfeito
(define (perfeito? n)
  (define (iter i acc)
    (cond ((= i 0) (= acc n))
          ((= (remainder n i) 0) (iter (- i 1) (+ acc i)))
          (else (iter (- i 1) acc))))
  (iter (- n 1) 0))

;; Função que retorna uma lista com todos os números perfeitos menores que n
(define (perfeitos-ate n)
  (define (iter i acc)
    (if (= i 1)
        acc
        (if (perfeito? i)
            (iter (- i 1) (cons i acc))
            (iter (- i 1) acc))))
  (iter n '()))

;; Função que retorna o número de Fibonacci na posição n
(define (fibonacci n)
  (define (iter i a b)
    (if (= i n)
        a
        (iter (+ i 1) b (+ a b))))
  (iter 0 0 1))

;; Exemplo de uso das funções
(displayln "Fatorial de 5:")
(displayln (fatorial 5))

(displayln "Números primos menores que 50:")
(displayln (primos-ate 50))

(displayln "Números perfeitos menores que 1000:")
(displayln (perfeitos-ate 1000))

(displayln "Fibonacci na posição 10:")
(displayln (fibonacci 10))
```

Explicação do código:

Aqui temos uma série de funções em Scheme que realizam diferentes cálculos matemáticos. 

A função `fatorial` recebe um número `n` e retorna o fatorial desse número. Ela utiliza uma expressão condicional (`if`) para verificar se `n` é menor ou igual a 1. Se for, retorna 1. Caso contrário, multiplica `n` pelo fatorial de `n-1` e retorna o resultado.

A função `primo?` verifica se um número `n` é primo. Ela utiliza uma função auxiliar `iter` que realiza uma iteração de 1 até a raiz quadrada de `n`, verificando se `n` é divisível por algum número. Se `n` for divisível por algum número, retorna falso (`#f`). Caso contrário, retorna verdadeiro (`#t`).

A função `primos-ate` recebe um número `n` e retorna uma lista com todos os números primos menores que `n`. A função utiliza uma função auxiliar `iter` que itera de `n` até 1, verificando se cada número é primo. Se for primo, adiciona-o à lista de primos (`acc`). No final, retorna a lista de primos.

A função `perfeito?` verifica se um número `n` é perfeito. Ela utiliza uma função auxiliar `iter` que realiza uma iteração de `n-1` até 0, verificando se cada número é um divisor de `n`. Se for divisor, soma-o à variável `acc`. No final, verifica se `acc` é igual a `n`. Se for, retorna verdadeiro (`#t`). Caso contrário, retorna falso (`#f`).

A função `perfeitos-ate` recebe um número `n` e retorna uma lista com todos os números perfeitos menores que `n`. A função utiliza uma função auxiliar `iter` que itera de `n` até 1, verificando se cada número é perfeito. Se for perfeito, adiciona-o à lista de perfeitos (`acc`). No final, retorna a lista de perfeitos.

A função `fibonacci` recebe um número `n` e retorna o número de Fibonacci na posição `n`. Ela utiliza uma função auxiliar `iter` que realiza uma iteração de 0 até `n`, calculando o próximo número de Fibonacci com base nos dois anteriores. No final, retorna o número de Fibonacci na posição `n`.

No final do código, há exemplos de uso das funções, com a impressão dos resultados na saída.