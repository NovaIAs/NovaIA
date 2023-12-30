Claro! Aqui está um exemplo de um código complexo em Scheme, que envolve a implementação de uma estrutura de dados de árvore binária de busca:

```scheme
;; Definição de um nó da árvore
(define-struct no (valor esquerda direita))

;; Função para criar um novo nó
(define (novo-no valor)
  (make-no valor '() '()))

;; Função para verificar se a árvore está vazia
(define (arvore-vazia? arvore)
  (null? arvore))

;; Função para inserir um valor na árvore
(define (inserir-arvore valor arvore)
  (cond ((arvore-vazia? arvore)
         (novo-no valor))
        ((< valor (no-valor arvore))
         (make-no (no-valor arvore)
                  (inserir-arvore valor (no-esquerda arvore))
                  (no-direita arvore)))
        ((> valor (no-valor arvore))
         (make-no (no-valor arvore)
                  (no-esquerda arvore)
                  (inserir-arvore valor (no-direita arvore))))
        (else
         arvore)))

;; Função para buscar um valor na árvore
(define (buscar-arvore valor arvore)
  (cond ((arvore-vazia? arvore)
         #f)
        ((= valor (no-valor arvore))
         #t)
        ((< valor (no-valor arvore))
         (buscar-arvore valor (no-esquerda arvore)))
        ((> valor (no-valor arvore))
         (buscar-arvore valor (no-direita arvore)))))

;; Exemplo de uso
(define arvore (novo-no 10))
(set! arvore (inserir-arvore 5 arvore))
(set! arvore (inserir-arvore 15 arvore))
(set! arvore (inserir-arvore 3 arvore))
(set! arvore (inserir-arvore 7 arvore))
(set! arvore (inserir-arvore 12 arvore))

(displayln (buscar-arvore 3 arvore))
(displayln (buscar-arvore 8 arvore))
```

Neste código, definimos uma estrutura de dados chamada `no`, que representa um nó da árvore binária de busca. Cada nó possui um valor, uma referência para o nó à esquerda e uma referência para o nó à direita.

Em seguida, temos as funções `novo-no`, `arvore-vazia?`, `inserir-arvore` e `buscar-arvore`. A função `novo-no` cria um novo nó com o valor especificado. A função `arvore-vazia?` verifica se a árvore está vazia. A função `inserir-arvore` insere um novo valor na árvore, respeitando a ordem da árvore binária de busca. E a função `buscar-arvore` busca um valor na árvore e retorna verdadeiro se o valor for encontrado, ou falso caso contrário.

No exemplo de uso, criamos uma árvore vazia e inserimos alguns valores nela. Em seguida, realizamos duas buscas na árvore para verificar se os valores estão presentes.

Espero que este exemplo atenda às suas expectativas de complexidade e diferenciação. Caso tenha alguma dúvida ou necessite de mais informações, estou à disposição!