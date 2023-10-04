Claro! Aqui está um código complexo em Scheme:

```
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (= n 0)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular a sequência de Fibonacci
(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (iter i)
    (cond ((= i 1) #t)
          ((= (remainder n i) 0) #f)
          (else (iter (- i 1)))))
  (iter (floor (sqrt n))))

;; Função para inverter uma lista
(define (inverter-lista lst)
  (define (iter lst resultado)
    (if (null? lst)
        resultado
        (iter (cdr lst) (cons (car lst) resultado))))
  (iter lst '()))

;; Função para calcular a soma dos elementos de uma lista
(define (soma-lista lst)
  (define (iter lst soma)
    (if (null? lst)
        soma
        (iter (cdr lst) (+ (car lst) soma))))
  (iter lst 0))

;; Exemplos de uso das funções
(display "O fatorial de 5 é: ")
(display (fatorial 5))
(newline)

(display "O 7º número de Fibonacci é: ")
(display (fibonacci 7))
(newline)

(display "O número 13 é primo? ")
(display (primo? 13))
(newline)

(display "A lista (1 2 3 4 5) invertida é: ")
(display (inverter-lista '(1 2 3 4 5)))
(newline)

(display "A soma dos elementos da lista (1 2 3 4 5) é: ")
(display (soma-lista '(1 2 3 4 5)))
(newline))
```

Explicação do código:

1. A função `fatorial` recebe um número `n` e retorna o fatorial desse número. Ela utiliza uma chamada recursiva para multiplicar `n` pelo fatorial do número anterior, até chegar a 0.

2. A função `fibonacci` calcula o n-ésimo número da sequência de Fibonacci, utilizando chamadas recursivas para somar os dois números anteriores.

3. A função `primo?` verifica se um número `n` é primo. Ela utiliza uma função auxiliar `iter` que testa se `n` é divisível por algum número, começando pelo maior número menor que `n`. Se encontrar algum divisor, retorna falso, caso contrário, retorna verdadeiro.

4. A função `inverter-lista` recebe uma lista e retorna uma nova lista com os elementos invertidos. Ela utiliza uma função auxiliar `iter` que percorre a lista original, adicionando cada elemento no início da nova lista.

5. A função `soma-lista` recebe uma lista de números e retorna a soma de todos os elementos. Ela utiliza uma função auxiliar `iter` que percorre a lista, somando cada elemento ao acumulador `soma`.

No final do código, há exemplos de uso de cada uma das funções, imprimindo resultados na tela.