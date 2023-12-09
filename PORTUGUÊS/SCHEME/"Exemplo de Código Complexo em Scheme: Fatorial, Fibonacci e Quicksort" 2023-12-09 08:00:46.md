Aqui está um exemplo de código complexo em Scheme:

```scheme
;; Definição da função fatorial iterativo
(define (fatorial-iterativo n)
  (define (iterativo n acc)
    (if (<= n 1)
        acc
        (iterativo (- n 1) (* acc n))))
  (iterativo n 1))

;; Definição da função fibonacci iterativo
(define (fibonacci-iterativo n)
  (define (iterativo n a b)
    (if (= n 0)
        a
        (iterativo (- n 1) b (+ a b))))
  (iterativo n 0 1))

;; Definição da função de ordenação quicksort
(define (quicksort lista)
  (if (null? lista)
      '()
      (let ((pivo (car lista))
            (menores (quicksort (filter (lambda (x) (< x pivo)) (cdr lista))))
            (maiores (quicksort (filter (lambda (x) (>= x pivo)) (cdr lista)))))
        (append menores (list pivo) maiores))))

;; Teste das funções
(displayln "Fatorial de 5: ")
(displayln (fatorial-iterativo 5))

(displayln "Fibonacci de 10: ")
(displayln (fibonacci-iterativo 10))

(displayln "Ordenação do vetor (7 2 5 1 8 3): ")
(displayln (quicksort '(7 2 5 1 8 3)))
```

Neste código, definimos três funções em Scheme:

1. `fatorial-iterativo`: calcula o fatorial de um número de forma iterativa, utilizando uma função auxiliar `iterativo` que realiza o cálculo utilizando um acumulador.
2. `fibonacci-iterativo`: calcula o n-ésimo termo da sequência de Fibonacci de forma iterativa, utilizando uma função auxiliar `iterativo` que utiliza dois acumuladores para calcular os termos da sequência.
3. `quicksort`: implementa o algoritmo de ordenação quicksort, utilizando uma abordagem recursiva. A função recebe uma lista e divide-a em duas partes, uma com elementos menores que o pivô e outra com elementos maiores ou iguais ao pivô. Em seguida, realiza a ordenação recursiva das duas partes e concatena os resultados.

No final do código, realizamos alguns testes com as funções, calculando o fatorial de 5, o décimo termo da sequência de Fibonacci e ordenando um vetor utilizando o quicksort. Os resultados são exibidos no console.