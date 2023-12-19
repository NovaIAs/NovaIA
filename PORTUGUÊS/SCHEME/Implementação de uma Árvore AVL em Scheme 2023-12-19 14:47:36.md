Claro! Aqui está um código complexo em Scheme que envolve uma implementação de uma estrutura de dados de árvore binária de busca balanceada chamada AVL Tree. É importante destacar que o código abaixo é um exemplo simplificado e não representa uma implementação completa e robusta.

```scheme
;; Definição da estrutura de dados do Nó da AVL Tree
(define-struct node (value left right height))

;; Função para criar um novo nó da AVL Tree
(define (make-node value left right)
  (let ((left-height (if (null? left) -1 (node-height left)))
        (right-height (if (null? right) -1 (node-height right))))
    (make-node value left right (+ 1 (max left-height right-height)))))

;; Função para obter a altura de um nó
(define (node-height node)
  (if (null? node) -1 (node-height node)))

;; Função para rotacionar um nó para a esquerda
(define (rotate-left node)
  (let ((right (node-right node)))
    (make-node (node-value right)
               (make-node (node-value node)
                          (node-left node)
                          (node-left right))
               (node-right right))))

;; Função para rotacionar um nó para a direita
(define (rotate-right node)
  (let ((left (node-left node)))
    (make-node (node-value left)
               (node-left left)
               (make-node (node-value node)
                          (node-right left)
                          (node-right node)))))

;; Função para balancear um nó da AVL Tree
(define (balance-node node)
  (let ((left (node-left node))
        (right (node-right node)))
    (cond
      ((> (node-height left) (+ (node-height right) 1))
       (let ((left-left (node-left left))
             (left-right (node-right left)))
         (if (> (node-height left-left) (node-height left-right))
             (rotate-right node)
             (rotate-left (make-node (node-value node)
                                     (rotate-left left)
                                     right)))))
      ((> (node-height right) (+ (node-height left) 1))
       (let ((right-left (node-left right))
             (right-right (node-right right)))
         (if (> (node-height right-right) (node-height right-left))
             (rotate-left node)
             (rotate-right (make-node (node-value node)
                                      left
                                      (rotate-right right))))))
      (else node))))

;; Função para inserir um valor em uma AVL Tree
(define (insert-value value tree)
  (if (null? tree)
      (make-node value '() '())
      (let ((current-value (node-value tree)))
        (if (< value current-value)
            (balance-node (make-node current-value
                                     (insert-value value (node-left tree))
                                     (node-right tree)))
            (balance-node (make-node current-value
                                     (node-left tree)
                                     (insert-value value (node-right tree))))))))

;; Função para imprimir os valores de uma AVL Tree em ordem
(define (inorder-print tree)
  (if (null? tree)
      '()
      (begin
        (inorder-print (node-left tree))
        (display (node-value tree))
        (display " ")
        (inorder-print (node-right tree)))))

;; Exemplo de uso da AVL Tree
(define avl-tree '())
(set! avl-tree (insert-value 5 avl-tree))
(set! avl-tree (insert-value 3 avl-tree))
(set! avl-tree (insert-value 7 avl-tree))
(set! avl-tree (insert-value 1 avl-tree))
(set! avl-tree (insert-value 4 avl-tree))
(set! avl-tree (insert-value 6 avl-tree))
(set! avl-tree (insert-value 9 avl-tree))

;; Imprime os valores da AVL Tree em ordem
(inorder-print avl-tree)
```

Neste código, implementamos uma AVL Tree, que é uma árvore binária de busca balanceada. A AVL Tree é balanceada através das rotações de nós para a esquerda e para a direita, de modo a garantir que a diferença entre as alturas dos nós filhos à esquerda e à direita nunca seja maior que 1.

A função `make-node` cria um novo nó da AVL Tree com um valor, nó filho à esquerda, nó filho à direita e altura calculada com base nas alturas dos nós filhos. As funções `rotate-left` e `rotate-right` realizam as rotações necessárias, enquanto a função `balance-node` verifica as alturas dos nós filhos e realiza as rotações apropriadas para balancear o nó.

A função `insert-value` insere um novo valor na AVL Tree, mantendo-a balanceada, e a função `inorder-print` imprime os valores da árvore em ordem.

Para testar o código, criamos uma AVL Tree vazia `avl-tree`. Em seguida, inserimos os valores 5, 3, 7, 1, 4, 6 e 9 na árvore usando a função `insert-value`. Por fim, usamos a função `inorder-print` para imprimir os valores da árvore em ordem.