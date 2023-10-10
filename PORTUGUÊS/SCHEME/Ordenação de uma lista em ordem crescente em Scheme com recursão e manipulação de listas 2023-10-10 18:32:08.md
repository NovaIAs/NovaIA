Claro! A seguir está um exemplo de código complexo em Scheme que envolve a manipulação de listas e recursão:

```scheme
;; Função para obter o maior elemento de uma lista
(define (maior-elemento lista)
  (cond ((null? (cdr lista)) (car lista)) ; Caso base: apenas um elemento na lista
        ((> (car lista) (maior-elemento (cdr lista))) ; Caso em que o primeiro elemento é maior
         (car lista))
        (else ; Caso em que o maior elemento está na cauda da lista
         (maior-elemento (cdr lista)))))

;; Função para ordenar uma lista em ordem crescente
(define (ordena-lista lista)
  (if (null? lista) ; Caso base: lista vazia
      '() ; Retornar lista vazia
      (let ((maior (maior-elemento lista))) ; Encontrar o maior elemento da lista
        (cons maior (ordena-lista (remove maior lista)))))) ; Adicionar o maior elemento à lista ordenada e remover da lista original

;; Função auxiliar para remover um elemento de uma lista
(define (remove elemento lista)
  (cond ((null? lista) '()) ; Caso base: lista vazia
        ((equal? elemento (car lista)) (cdr lista)) ; Caso em que o elemento é encontrado na cabeça da lista
        (else (cons (car lista) (remove elemento (cdr lista)))))) ; Caso em que o elemento está na cauda da lista

;; Exemplo de uso
(displayln "Insira uma lista de números inteiros:")
(let ((lista (read))) ; Ler a lista fornecida pelo usuário
  (displayln "Lista original:")
  (displayln lista)
  (displayln "Lista ordenada:")
  (displayln (ordena-lista lista)))
```
Neste código em Scheme, definimos três funções principais:

1. A função `maior-elemento` recebe uma lista de números inteiros e retorna o maior elemento dessa lista, utilizando recursão para comparar cada elemento com o próximo.
2. A função `ordena-lista` recebe uma lista de números inteiros e retorna a lista ordenada em ordem crescente. Ela utiliza a função `maior-elemento` para encontrar o maior elemento e a função `remove` para remover esse elemento da lista original, repetindo o processo até que a lista esteja vazia.
3. A função `remove` recebe um elemento e uma lista e retorna uma nova lista sem o elemento fornecido.

No exemplo de uso fornecido, o programa solicita ao usuário que insira uma lista de números inteiros. Em seguida, exibe a lista original e a lista ordenada usando as funções definidas anteriormente.

É importante mencionar que esse código é apenas um exemplo para ilustrar a complexidade e as funcionalidades do Scheme. Dependendo do tamanho e da complexidade da lista fornecida pelo usuário, o desempenho do código pode variar.