Claro! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (= n 0)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular a sequência de Fibonacci até um determinado termo
(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (iter i)
    (cond ((< i 2) #t)
          ((= (modulo n i) 0) #f)
          (else (iter (- i 1)))))
  (iter (- n 1)))

;; Função para encontrar todos os números primos em um intervalo
(define (primos-entre a b)
  (if (> a b)
      '()
      (if (primo? a)
          (cons a (primos-entre (+ a 1) b))
          (primos-entre (+ a 1) b))))

;; Função principal que utiliza as funções anteriores
(define (main)
  (display "Digite um número para calcular o fatorial: ")
  (define n (string->number (read-line)))
  (display "O fatorial de ")
  (display n)
  (display " é ")
  (display (fatorial n))
  (newline)
  
  (display "Digite um número para calcular a sequência de Fibonacci: ")
  (define m (string->number (read-line)))
  (display "Os primeiros ")
  (display m)
  (display " termos da sequência de Fibonacci são: ")
  (display (fibonacci m))
  (newline)
  
  (display "Digite o limite inferior do intervalo: ")
  (define lower (string->number (read-line)))
  (display "Digite o limite superior do intervalo: ")
  (define upper (string->number (read-line)))
  (display "Os números primos entre ")
  (display lower)
  (display " e ")
  (display upper)
  (display " são: ")
  (display (primos-entre lower upper))
  (newline))

(main)
```

Explicação do código:

1. A função `fatorial` recebe um número `n` e retorna o valor do fatorial de `n`.
2. A função `fibonacci` recebe um número `n` e retorna o n-ésimo termo da sequência de Fibonacci.
3. A função `primo?` recebe um número `n` e retorna um valor booleano indicando se `n` é primo.
4. A função `primos-entre` recebe dois números `a` e `b` e retorna uma lista contendo todos os números primos entre `a` e `b`.
5. A função `main` é a função principal que interage com o usuário. Ela solicita que o usuário insira números e exibe os resultados dos cálculos de fatorial, sequência de Fibonacci e números primos.
6. Na função `main`, as mensagens são exibidas utilizando a função `display`, e os números inseridos pelo usuário são lidos usando `read-line` e convertidos para números usando `string->number`.
7. No final, a função `main` é chamada para iniciar a execução do programa.