Claro! Aqui está um código complexo em Scheme:

```
;; Função auxiliar para calcular o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular o número de Fibonacci de um determinado índice
(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1))
                 (fibonacci (- n 2))))))

;; Função para criar uma lista de números primos até um determinado limite
(define (gerar-primos limite)
  (define (eh-primo? n)
    (define (divisores-ate m)
      (if (< m 2)
          '()
          (if (= (modulo n m) 0)
              (cons m (divisores-ate (- m 1)))
              (divisores-ate (- m 1)))))
    (if (null? (divisores-ate (- n 1)))
        #t
        #f))
  (define (gerar-primos-aux n)
    (if (<= n limite)
        (if (eh-primo? n)
            (cons n (gerar-primos-aux (+ n 1)))
            (gerar-primos-aux (+ n 1)))
        '()))
  (gerar-primos-aux 2))

;; Exemplo de uso das funções
(display "Fatorial de 5: ")
(display (fatorial 5))
(newline)

(display "10 primeiros números de Fibonacci: ")
(display (map fibonacci (range 0 10)))
(newline)

(display "Primos até o número 20: ")
(display (gerar-primos 20))
(newline))
```

Neste código, temos três funções principais:

1. A função `fatorial` recebe um número `n` e retorna o fatorial desse número, utilizando recursão.
2. A função `fibonacci` recebe um número `n` e retorna o número de Fibonacci correspondente ao índice `n`, também utilizando recursão.
3. A função `gerar-primos` recebe um limite máximo e retorna uma lista com todos os números primos até esse limite. Ela utiliza a função auxiliar `eh-primo?` para verificar se um número é primo ou não, e a função auxiliar `divisores-ate` para gerar uma lista de divisores possíveis de um número.

No exemplo de uso das funções, são mostrados os resultados do fatorial de 5, os 10 primeiros números de Fibonacci e os números primos até o número 20.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, é só me perguntar.