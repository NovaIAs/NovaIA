Claro! Aqui está um código complexo em Scheme que utiliza uma abordagem recursiva para realizar uma busca em profundidade em uma árvore binária. O código é bem extenso e usa comentários para explicar cada etapa:

```scheme
;; Definição de uma árvore binária
(define-struct no-binario (valor esquerda direita))

;; Função que verifica se um nó possui filhos
(define (tem-filhos? no)
  (and (no-binario-esquerda no) (no-binario-direita no)))

;; Função que busca em profundidade em uma árvore binária
(define (busca-em-profundidade arvore valor)
  ;; Função auxiliar que realiza a busca em profundidade recursivamente
  (define (busca-recursiva no)
    ;; Verifica se o nó atual possui o valor desejado
    (if (= (no-binario-valor no) valor)
        #t ;; Valor encontrado, retorna verdadeiro
        ;; Caso contrário, verifica se o nó possui filhos para continuar a busca
        (if (tem-filhos? no)
            (or (busca-recursiva (no-binario-esquerda no))
                (busca-recursiva (no-binario-direita no)))
            #f))) ;; Nó sem filhos, retorna falso

  ;; Inicia a busca a partir da raiz da árvore
  (busca-recursiva arvore))

;; Exemplo de uso
(define arvore-exemplo
  (no-binario 10
              (no-binario 5
                          (no-binario 2
                                      (no-binario 1 #f #f)
                                      #f)
                          (no-binario 7
                                      (no-binario 6 #f #f)
                                      (no-binario 8 #f #f)))
              (no-binario 15
                          (no-binario 12 #f #f)
                          (no-binario 20
                                      (no-binario 18 #f #f)
                                      (no-binario 25 #f #f)))))

(display (busca-em-profundidade arvore-exemplo 6))
```

O código acima define uma estrutura de dados para representar uma árvore binária e implementa a função `busca-em-profundidade`, que realiza uma busca em profundidade para encontrar um valor específico na árvore. O exemplo de uso cria uma árvore binária de exemplo e realiza uma busca pelo valor 6, imprimindo o resultado na tela.

Espero que esse código atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais informações, estou à disposição.