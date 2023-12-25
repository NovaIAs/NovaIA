Claro! Aqui está um exemplo de código complexo em Scheme, que realiza uma implementação recursiva do algoritmo de ordenação Merge Sort:

```scheme
;; Função auxiliar: une duas listas ordenadas em uma nova lista ordenada
(define (merge-listas lista1 lista2)
  (cond
    ((null? lista1) lista2)
    ((null? lista2) lista1)
    ((< (car lista1) (car lista2)) (cons (car lista1) (merge-listas (cdr lista1) lista2)))
    (else (cons (car lista2) (merge-listas lista1 (cdr lista2))))))

;; Função principal: divide a lista em duas partes, ordena-as recursivamente e depois as une
(define (merge-sort lista)
  (cond
    ((or (null? lista) (null? (cdr lista))) lista)
    (else
      (let* ((metade (quotient (length lista) 2))
             (esquerda (list-tail lista metade))
             (direita (list-drop lista metade)))
        (merge-listas (merge-sort esquerda) (merge-sort direita))))))

;; Exemplo de uso
(display "Informe uma lista de números para ordenar: ")
(let ((lista (read)))
  (display "Lista ordenada: ")
  (display (merge-sort lista)))
```

Neste código, a função `merge-listas` é uma função auxiliar que recebe duas listas ordenadas e retorna uma nova lista ordenada com todos os elementos das duas listas. Essa função é usada pela função principal `merge-sort`, que realiza a ordenação da lista utilizando o algoritmo Merge Sort.

A função `merge-sort` é implementada de forma recursiva. Primeiro, ela verifica se a lista está vazia ou contém apenas um elemento. Nesses casos, a lista já está ordenada e pode ser retornada diretamente.

Caso contrário, a lista é dividida em duas partes, aproximadamente de mesmo tamanho, utilizando a função `list-tail` e `list-drop`. Em seguida, as duas partes são ordenadas recursivamente chamando a função `merge-sort`. Por fim, as duas partes ordenadas são unidas utilizando a função `merge-listas`.

No exemplo de uso, o programa solicita ao usuário que informe uma lista de números para ser ordenada. Em seguida, a função `merge-sort` é chamada passando essa lista como argumento, e o resultado é exibido na saída padrão.

Espero que esse código atenda às suas expectativas!