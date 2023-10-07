Claro, vou gerar um código complexo em Scheme para você:

```scheme
;; Função recursiva para calcular o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Função para gerar a sequência de Fibonacci
(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1))
                 (fibonacci (- n 2))))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (divisivel? x)
    (if (= (remainder n x) 0)
        #t
        #f))
  (define (verificar-divisores x)
    (cond ((<= x 1) #t)
          ((divisivel? x) #f)
          (else (verificar-divisores (- x 1)))))
  (verificar-divisores (- (sqrt n) 1)))

;; Função para ordenar uma lista em ordem crescente
(define (ordenar lista)
  (cond ((null? lista) '())
        ((null? (cdr lista)) lista)
        (else
         (let ((pivo (car lista))
               (menores (filter (lambda (x) (< x pivo)) (cdr lista)))
               (maiores (filter (lambda (x) (>= x pivo)) (cdr lista))))
           (append (ordenar menores) (list pivo) (ordenar maiores))))))

;; Exemplos de uso das funções
(displayln "Fatorial de 5: " (fatorial 5))
(displayln "Fibonacci de 10: " (fibonacci 10))
(displayln "É primo? 7: " (primo? 7))
(displayln "Lista ordenada: " (ordenar '(5 2 8 1 9 3)))
```

Este código em Scheme contém quatro funções diferentes:
1. A função `fatorial` recebe um número `n` e calcula o fatorial dele de forma recursiva.
2. A função `fibonacci` recebe um número `n` e retorna o `n`-ésimo número da sequência de Fibonacci.
3. A função `primo?` recebe um número `n` e verifica se ele é primo.
4. A função `ordenar` recebe uma lista e a ordena em ordem crescente usando o algoritmo de ordenação por seleção.

O código também faz exemplos de uso das funções, mostrando o resultado de cada uma delas.