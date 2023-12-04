Claro! Aqui está um código complexo em Scheme que envolve a implementação de uma árvore binária de busca:

```scheme
(define (arvore-busca inserir buscar)
  (define (nova-arvore valor esq dir)
    (list valor esq dir))

  (define (criar-arvore)
    '())

  (define (arvore-vazia? arvore)
    (null? arvore))

  (define (valor-arvore arvore)
    (car arvore))

  (define (esq-arvore arvore)
    (cadr arvore))

  (define (dir-arvore arvore)
    (caddr arvore))

  (define (inserir-elemento valor arvore)
    (if (arvore-vazia? arvore)
        (nova-arvore valor '() '())
        (if (< valor (valor-arvore arvore))
            (nova-arvore (valor-arvore arvore)
                         (inserir-elemento valor (esq-arvore arvore))
                         (dir-arvore arvore))
            (if (> valor (valor-arvore arvore))
                (nova-arvore (valor-arvore arvore)
                             (esq-arvore arvore)
                             (inserir-elemento valor (dir-arvore arvore)))
                arvore))))

  (define (buscar-elemento valor arvore)
    (if (arvore-vazia? arvore)
        #f
        (if (= valor (valor-arvore arvore))
            #t
            (if (< valor (valor-arvore arvore))
                (buscar-elemento valor (esq-arvore arvore))
                (buscar-elemento valor (dir-arvore arvore))))))

  (define (imprimir-arvore arvore)
    (if (arvore-vazia? arvore)
        (display "Árvore vazia.")
        (begin
          (display "Valor: ")
          (display (valor-arvore arvore))
          (newline)
          (display "Esquerda: ")
          (display (esq-arvore arvore))
          (newline)
          (display "Direita: ")
          (display (dir-arvore arvore))
          (newline))))

  (list inserir-elemento buscar-elemento imprimir-arvore))
```

Neste código, implementamos uma árvore binária de busca utilizando uma estrutura de dados em Scheme. A árvore é representada por uma lista, onde cada elemento possui um valor, uma subárvore esquerda e uma subárvore direita.

A função `arvore-busca` recebe como parâmetros as funções `inserir` e `buscar`. A função `inserir-elemento` insere um novo valor na árvore, mantendo a ordem de forma que os valores menores são colocados à esquerda e os maiores à direita. A função `buscar-elemento` verifica se um valor está presente na árvore e retorna `#t` caso esteja e `#f` caso contrário. A função `imprimir-arvore` exibe na tela o valor, a subárvore esquerda e a subárvore direita de um nó da árvore.

Você pode utilizar esse código para criar, inserir e buscar elementos em uma árvore binária de busca em Scheme.