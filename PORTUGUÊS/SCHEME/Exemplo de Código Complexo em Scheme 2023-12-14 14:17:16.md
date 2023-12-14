Claro! Aqui está um exemplo de código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número usando recursão de cauda
(define (fatorial n)
  (letrec ((fatorial-aux (lambda (n acc)
                           (if (= n 0)
                               acc
                               (fatorial-aux (- n 1) (* acc n))))))
    (fatorial-aux n 1)))

;; Função para calcular a sequência de Fibonacci até o n-ésimo termo
(define (fibonacci n)
  (letrec ((fibonacci-aux (lambda (n a b)
                            (if (= n 0)
                                a
                                (fibonacci-aux (- n 1) b (+ a b))))))
    (fibonacci-aux n 0 1)))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (divisivel? n divisor)
    (if (= divisor 1)
        #f
        (if (= (remainder n divisor) 0)
            #t
            (divisivel? n (- divisor 1)))))
  (if (= n 1)
      #f
      (not (divisivel? n (- (sqrt n) 1)))))

;; Função para calcular o produto escalar de dois vetores
(define (produto-escalar v1 v2)
  (if (or (null? v1) (null? v2))
      0
      (+ (* (car v1) (car v2)) (produto-escalar (cdr v1) (cdr v2)))))

;; Função para verificar se uma palavra é um palíndromo
(define (palindromo? palavra)
  (let* ((palavra-lower (string-downcase palavra))
         (reverter (lambda (str)
                     (if (null? str)
                         ""
                         (string-append (reverter (cdr str)) (string (car str)))))))
    (string=? palavra-lower (reverter (string->list palavra-lower)))))

;; Exemplos de uso das funções
(displayln "Fatorial de 5: " (fatorial 5))
(displayln "Sequência de Fibonacci até o 10º termo: " (fibonacci 10))
(displayln "É primo? 17: " (primo? 17))
(displayln "Produto escalar de [1 2 3] e [4 5 6]: " (produto-escalar '(1 2 3) '(4 5 6)))
(displayln "É palíndromo? 'arara': " (palindromo? "arara"))
```

Neste código em Scheme, temos a implementação de várias funções com diferentes propósitos:

1. A função `fatorial` calcula o fatorial de um número usando recursão de cauda.
2. A função `fibonacci` calcula a sequência de Fibonacci até o n-ésimo termo usando recursão de cauda.
3. A função `primo?` verifica se um número é primo.
4. A função `produto-escalar` calcula o produto escalar entre dois vetores.
5. A função `palindromo?` verifica se uma palavra é um palíndromo.

No final do código, são exibidos alguns exemplos de uso das funções.