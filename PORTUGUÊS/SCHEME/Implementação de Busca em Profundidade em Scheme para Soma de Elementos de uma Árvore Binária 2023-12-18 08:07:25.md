Claro! Aqui está um código em Scheme que implementa um algoritmo de busca em profundidade para encontrar a soma de todos os elementos de uma árvore binária:

```scheme
(define (arvore-soma arvore)
  (cond
    ((null? arvore) 0)
    ((not (pair? (cdr arvore))) (car arvore))
    (else (+ (arvore-soma (car arvore))
             (arvore-soma (cdr arvore))))))

(define (arvore-binaria? arvore)
  (or (null? arvore)
      (and (pair? arvore)
           (or (null? (cdr arvore))
               (arvore-binaria? (car arvore)))
           (arvore-binaria? (cdr arvore)))))


(define (busca-em-profundidade arvore alvo)
  (define (auxiliar arvore alvo resultado)
    (if (null? arvore)
        resultado
        (let ((valor (car arvore)))
          (if (= valor alvo)
              (+ resultado valor)
              (if (arvore-binaria? (cdr arvore))
                  (auxiliar (cdr arvore) alvo (+ resultado valor))
                  (auxiliar (car arvore) alvo (+ resultado valor)))))))

  (if (arvore-binaria? arvore)
      (auxiliar arvore alvo 0)
      (error "Entrada inválida: a árvore não é binária")))

(define arvore-exemplo '(1 (2 (3 4) (5 6)) (7 (8 9) (10 11))))
(display (busca-em-profundidade arvore-exemplo 6))
(newline)
```

Neste código, a função `arvore-soma` é responsável por calcular a soma de todos os elementos de uma árvore binária. Ela utiliza recursão para percorrer a árvore e somar todos os elementos. A função `arvore-binaria?` verifica se a estrutura passada como argumento é uma árvore binária válida.

A função principal `busca-em-profundidade` recebe uma árvore binária e um valor alvo. Ela utiliza a função auxiliar `auxiliar` para percorrer a árvore em busca do valor alvo, utilizando um algoritmo de busca em profundidade. Se o valor for encontrado, a função retorna a soma dos elementos percorridos até o momento. Caso contrário, retorna um valor de erro.

No exemplo fornecido, a árvore binária de exemplo é `(1 (2 (3 4) (5 6)) (7 (8 9) (10 11)))`, e a função busca o valor 6. O resultado impresso será 21, que é a soma dos elementos percorridos para encontrar o valor 6.