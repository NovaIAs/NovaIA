Claro! Aqui está um exemplo de código em Scheme que implementa o algoritmo de ordenação Merge Sort:

```scheme
; Função auxiliar para dividir uma lista ao meio
(define (split-list lst)
  (let ((half (quotient (length lst) 2)))
    (values (take lst half) (drop lst half))))

; Função auxiliar para mesclar duas listas ordenadas
(define (merge-lists lst1 lst2)
  (cond ((null? lst1) lst2)
        ((null? lst2) lst1)
        ((< (car lst1) (car lst2))
         (cons (car lst1) (merge-lists (cdr lst1) lst2)))
        (else
         (cons (car lst2) (merge-lists lst1 (cdr lst2))))))

; Função principal de Merge Sort
(define (merge-sort lst)
  (cond ((null? lst) lst)
        ((null? (cdr lst)) lst)
        (else
         (let-values (((left right) (split-list lst)))
           (merge-lists (merge-sort left) (merge-sort right))))))

; Exemplo de uso
(display "Digite a lista de números a serem ordenados: ")
(define numbers (read))
(display "A lista ordenada é: ")
(merge-sort numbers)
```

Neste código, implementamos o algoritmo de ordenação Merge Sort usando Scheme. 

A função `split-list` recebe uma lista e a divide em duas partes iguais utilizando a função `take` e `drop`. 

A função `merge-lists` recebe duas listas ordenadas e as mescla em uma única lista ordenada. 

A função `merge-sort` é a função principal, que divide recursivamente a lista em duas partes e chama a função `merge-lists` para mesclar as partes ordenadas. 

Por fim, temos um exemplo de uso, onde o usuário digita uma lista de números e o código chama a função `merge-sort` para ordenar a lista e exibi-la na saída.