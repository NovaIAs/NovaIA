Claro, aqui está um código em Scheme que realiza o algoritmo de ordenação quicksort:

```
(define (quicksort lista)
  (if (null? lista)
      '()
      (let ((pivot (car lista))
            (resto (cdr lista)))
        (append (quicksort (filter (lambda (x) (< x pivot)) resto))
                (list pivot)
                (quicksort (filter (lambda (x) (>= x pivot)) resto))))))

(display (quicksort '(9 4 2 7 1 8 3 6 5)))
```

O quicksort é um algoritmo de ordenação muito eficiente que é amplamente utilizado em computação. Neste código, a função `quicksort` recebe uma lista não ordenada como entrada e retorna a lista ordenada.

O algoritmo quicksort é realizado em três etapas:

1. Escolha um elemento da lista, o "pivot".
2. Divida a lista em duas sublistas: uma com elementos menores que o pivot e outra com elementos maiores que o pivot.
3. Recursivamente, ordene as duas sublistas e concatene-as com o pivot no meio.

O código começa verificando se a lista é vazia (linha 2). Se for, retorna uma lista vazia. Caso contrário, o pivot é definido como o primeiro elemento da lista (linha 4) e o resto da lista é armazenado em uma variável chamada "resto" (linha 5).

As duas sublistas são criadas usando a função `filter` do Scheme (linhas 6 e 8). A primeira sublista contém todos os elementos menores que o pivot e a segunda sublista contém todos os elementos maiores ou iguais ao pivot.

As duas sublistas são recursivamente ordenadas usando a função quicksort (linhas 7 e 9) e, em seguida, concatenadas com o pivot no meio (linha 7).

Finalmente, o resultado é exibido usando a função `display` (linha 11).

Este código é altamente otimizado e pode lidar com listas grandes com eficiência.