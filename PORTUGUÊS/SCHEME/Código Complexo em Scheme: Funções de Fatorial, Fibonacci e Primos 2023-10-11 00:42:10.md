Claro! Aqui está um código complexo em Scheme:

```scheme
;; Função para encontrar o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Função para encontrar o n-ésimo número de Fibonacci
(define (fibonacci n)
  (cond ((<= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (iter i)
    (cond ((= i n) #t)
          ((= (modulo n i) 0) #f)
          (else (iter (+ i 1)))))
  (iter 2))

;; Função para encontrar todos os números primos em um intervalo
(define (primos-no-intervalo inicio fim)
  (define (iter n)
    (if (> n fim)
        '()
        (if (primo? n)
            (cons n (iter (+ n 1)))
            (iter (+ n 1)))))
  (iter inicio))

;; Exemplo de uso das funções
(displayln "O fatorial de 5 é: ")
(displayln (fatorial 5))
(displayln "Os primeiros 10 números de Fibonacci são: ")
(displayln (fibonacci 10))
(displayln "Os números primos entre 1 e 100 são: ")
(displayln (primos-no-intervalo 1 100))
```

Neste código em Scheme, temos algumas funções definidas. A função `fatorial` calcula o fatorial de um número dado. A função `fibonacci` retorna o n-ésimo número da sequência de Fibonacci. A função `primo?` verifica se um número é primo ou não. Por fim, a função `primos-no-intervalo` retorna todos os números primos em um dado intervalo.

No exemplo de uso das funções, mostramos o fatorial de 5, os primeiros 10 números de Fibonacci e os números primos entre 1 e 100. A saída será impressa no console.