Claro! Aqui está um código complexo em Scheme que implementa uma árvore binária de busca balanceada (AVL). 

```scheme
;; Definindo o tipo de dado 'arvore' que representa um nó de uma árvore binária.
(define-struct no (valor esq dir altura))

;; Função para obter a altura de um nó.
(define (altura no)
  (if (no? no)
      (no-altura no)
      -1))

;; Função para obter o fator de equilíbrio de um nó.
(define (fator-equilibrio no)
  (- (altura (no-dir no)) (altura (no-esq no))))

;; Função para realizar a rotação simples à direita.
(define (rotacao-simples-direita no)
  (let ((no-esq (no-esq no)))
    (make-no (no-valor no-esq)
             (no-dir no-esq)
             (make-no (no-valor no)
                      (no-esq (no-dir no))
                      (no-dir no))
             (+ 1 (max (altura (no-esq no)) (altura (no-dir no)))))))

;; Função para realizar a rotação simples à esquerda.
(define (rotacao-simples-esquerda no)
  (let ((no-dir (no-dir no)))
    (make-no (no-valor no-dir)
             (make-no (no-valor no)
                      (no-esq no)
                      (no-esq no-dir))
             (no-dir no-dir)
             (+ 1 (max (altura (no-esq no)) (altura (no-dir no)))))))

;; Função para realizar a rotação dupla à direita.
(define (rotacao-dupla-direita no)
  (let ((no-esq (no-esq no))
        (no-dir (no-dir no)))
    (make-no (no-valor (no-dir no-esq))
             (make-no (no-valor no)
                      (no-esq no)
                      (no-esq no-esq))
             (make-no (no-valor no-dir)
                      (no-dir no-esq)
                      (no-dir no))
             (+ 1 (max (altura (no-esq no)) (altura (no-dir no)))))))

;; Função para realizar a rotação dupla à esquerda.
(define (rotacao-dupla-esquerda no)
  (let ((no-dir (no-dir no))
        (no-esq (no-esq no)))
    (make-no (no-valor (no-esq no-dir))
             (make-no (no-valor no-esq)
                      (no-esq no)
                      (no-esq no-dir))
             (make-no (no-valor no)
                      (no-dir no-dir)
                      (no-dir no))
             (+ 1 (max (altura (no-esq no)) (altura (no-dir no)))))))

;; Função para inserir um valor em uma árvore AVL.
(define (inserir valor arvore)
  (cond ((not (no? arvore))
         (make-no valor '() '() 0))
        ((< valor (no-valor arvore))
         (let ((esq (no-esq arvore))
               (dir (no-dir arvore)))
           (if (no? esq)
               (make-no (no-valor arvore)
                        (make-no valor '() '() 0)
                        dir
                        (+ 1 (max (altura (make-no valor '() '() 0)) (altura dir))))
               (let ((nova-esq (inserir valor esq)))
                 (if (> (fator-equilibrio (make-no (no-valor arvore) nova-esq dir 0))) 
                     (rotacao-simples-direita (make-no (no-valor arvore) nova-esq dir 0))
                     (if (< (fator-equilibrio (make-no (no-valor nova-esq) (no-esq nova-esq) dir 0))) 
                         (rotacao-dupla-direita (make-no (no-valor arvore) nova-esq dir 0))
                         (make-no (no-valor arvore) nova-esq dir (+ 1 (max (altura nova-esq) (altura dir)))))))))
        ((> valor (no-valor arvore))
         (let ((esq (no-esq arvore))
               (dir (no-dir arvore)))
           (if (no? dir)
               (make-no (no-valor arvore)
                        esq
                        (make-no valor '() '() 0)
                        (+ 1 (max (altura esq) (altura (make-no valor '() '() 0)))))
               (let ((nova-dir (inserir valor dir)))
                 (if (< (fator-equilibrio (make-no (no-valor arvore) esq nova-dir 0))) 
                     (rotacao-simples-esquerda (make-no (no-valor arvore) esq nova-dir 0))
                     (if (> (fator-equilibrio (make-no (no-valor nova-dir) esq (no-dir nova-dir) 0))) 
                         (rotacao-dupla-esquerda (make-no (no-valor arvore) esq nova-dir 0))
                         (make-no (no-valor arvore) esq nova-dir (+ 1 (max (altura esq) (altura nova-dir))))))))))))

;; Exemplo de uso:
(define arvore '())
(set! arvore (inserir 10 arvore))
(set! arvore (inserir 5 arvore))
(set! arvore (inserir 15 arvore))
(set! arvore (inserir 3 arvore))
(set! arvore (inserir 7 arvore))
(display arvore)
```

Este código implementa uma árvore binária de busca balanceada (AVL) em Scheme. A estrutura de dados `no` representa um nó da árvore, contendo um valor, uma subárvore esquerda, uma subárvore direita e a altura do nó.

A função `altura` retorna a altura de um nó. A função `fator-equilibrio` calcula o fator de equilíbrio de um nó, que é a diferença entre a altura da subárvore direita e a altura da subárvore esquerda.

As funções `rotacao-simples-direita`, `rotacao-simples-esquerda`, `rotacao-dupla-direita` e `rotacao-dupla-esquerda` são responsáveis por realizar as rotações necessárias para balancear a árvore.

A função `inserir` insere um valor na árvore, mantendo-a balanceada. Ela utiliza as rotações simples e duplas quando necessário, para garantir que o fator de equilíbrio de cada nó esteja dentro do intervalo -1, 0 ou 1.

No exemplo de uso no final do código, são inseridos alguns valores na árvore e, em seguida, ela é exibida na saída.