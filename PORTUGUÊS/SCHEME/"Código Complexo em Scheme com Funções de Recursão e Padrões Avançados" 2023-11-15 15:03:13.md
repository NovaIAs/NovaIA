Claro! Aqui está um código complexo em Scheme que utiliza a recursão e padrões avançados:

```scheme
;; Definindo uma função fatorial
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Definindo uma função fibonacci
(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

;; Definindo uma função de busca binária
(define (busca-binaria lista chave inicio fim)
  (if (>= inicio fim)
      #f
      (let* ((meio (quotient (+ inicio fim) 2))
             (valor-meio (list-ref lista meio)))
        (cond ((= valor-meio chave) meio)
              ((> valor-meio chave) (busca-binaria lista chave inicio meio))
              (else (busca-binaria lista chave (+ meio 1) fim))))))

;; Definindo uma função para verificar se um número é primo
(define (e-primo? n)
  (cond ((or (= n 2) (= n 3)) #t)
        ((or (= n 1) (even? n))) #f
        (else (let ((limite (sqrt n))
                    (i 2))
                (do ((i i (+ i 2)))
                    ((> i limite) #t)
                    (if (zero? (modulo n i)) (return #f))))))

;; Definindo uma função para ordenar uma lista usando o algoritmo de bubble sort
(define (bubble-sort lista)
  (let ((n (length lista)))
    (do ((i 0 (+ i 1)))
        ((= i (- n 1)) lista)
        (do ((j 0 (+ j 1)))
            ((= j (- n i 1)))
            (let ((x (list-ref lista j))
                  (y (list-ref lista (+ j 1))))
          (if (> x y)
              (begin
                (list-set! lista j y)
                (list-set! lista (+ j 1) x)))))))

;; Exemplos de uso das funções definidas

(displayln "Fatorial de 5:")
(displayln (fatorial 5))
(newline)

(displayln "Sequência de Fibonacci até o 10º termo:")
(do ((i 0 (+ i 1)))
    ((= i 10))
    (display (fibonacci i))
    (display " ")))
(newline)

(displayln "Busca pelo número 5 na lista (1 2 3 4 5 6 7 8 9 10):")
(displayln (busca-binaria '(1 2 3 4 5 6 7 8 9 10) 5 0 10))
(newline)

(displayln "O número 7 é primo?")
(displayln (e-primo? 7))
(newline)

(displayln "Ordenando a lista (5 3 1 4 2 6) usando bubble sort:")
(displayln (bubble-sort '(5 3 1 4 2 6)))
(newline)
```

Este código complexo em Scheme possui várias funções que executam diferentes tarefas. 

A primeira função é `fatorial`, que calcula o fatorial de um número utilizando recursão. 

A segunda função é `fibonacci`, que calcula a sequência de Fibonacci até o n-ésimo termo utilizando recursão. 

A terceira função é `busca-binaria`, que realiza uma busca binária em uma lista ordenada, retornando o índice do elemento encontrado ou #f caso o elemento não esteja presente na lista. 

A quarta função é `e-primo?`, que verifica se um número é primo. 

A quinta função é `bubble-sort`, que utiliza o algoritmo de ordenação bubble sort para ordenar uma lista. 

No código de exemplo, são feitos testes com cada função para demonstrar seu funcionamento.