Claro, vou gerar um código complexo em Scheme para você. Aqui está o código:

```scheme
;; Definição da função fatorial
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Definição da função soma-impares
(define (soma-impares a b)
  (if (> a b)
      0
      (if (odd? a)
          (+ a (soma-impares (+ a 2) b))
          (soma-impares (+ a 1) b))))

;; Definição da função fibonacci
(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

;; Definição da função quadrado-perfeito?
(define (quadrado-perfeito? n)
  (let ((raiz (sqrt n)))
    (if (integer? raiz)
        #t
        #f)))

;; Exemplo de uso das funções
(displayln "Fatorial de 5 é " (fatorial 5))
(displayln "Soma dos ímpares entre 1 e 10 é " (soma-impares 1 10))
(displayln "O 6º número de Fibonacci é " (fibonacci 6))
(displayln "O número 16 é um quadrado perfeito? " (quadrado-perfeito? 16))
```

Neste código, defini quatro funções diferentes em Scheme: `fatorial`, `soma-impares`, `fibonacci` e `quadrado-perfeito?`. 

A função `fatorial` calcula o fatorial de um número dado, usando recursão. A função `soma-impares` retorna a soma de todos os números ímpares entre dois números dados. A função `fibonacci` retorna o n-ésimo número da sequência de Fibonacci. E a função `quadrado-perfeito?` verifica se um número é um quadrado perfeito.

No exemplo de uso das funções, são realizadas chamadas para cada uma delas, com parâmetros específicos. Os resultados são exibidos na tela usando a função `displayln`.

Espero que este código atenda às suas expectativas e demonstre um exemplo de código complexo em Scheme.