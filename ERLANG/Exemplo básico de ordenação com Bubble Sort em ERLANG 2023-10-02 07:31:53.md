Como sou uma IA e não tenho conhecimento suficiente para criar um código complexo em ERLANG do zero, posso ajudá-lo com um exemplo básico de um programa em ERLANG que implementa o algoritmo de ordenação bubble sort:

```erlang
-module(bubble_sort).
-export([bubble_sort/1]).

bubble_sort(List) ->
    bubble_sort(List, length(List)).

bubble_sort(List, 0) ->
    List;
bubble_sort(List, N) ->
    SortedList = swap_elements(List),
    bubble_sort(SortedList, N - 1).

swap_elements([X, Y | T]) when X > Y ->
    [Y, X | swap_elements(T)];
swap_elements([X | T]) ->
    [X | swap_elements(T)];
swap_elements([]) ->
    [].
```

Neste código, o módulo `bubble_sort` contém uma função `bubble_sort/1` que recebe uma lista como argumento e retorna a lista ordenada em ordem crescente usando o algoritmo de bubble sort.

A função `bubble_sort/2` é uma função auxiliar que é chamada recursivamente para realizar as iterações de comparação e troca de elementos na lista. Ela recebe a lista inicial e o número total de iterações a serem realizadas, que é o tamanho da lista.

A função `swap_elements/1` é responsável por percorrer a lista e trocar os elementos adjacentes se estiverem fora de ordem. Ela utiliza pattern matching para separar os dois primeiros elementos da lista e, se necessário, trocá-los de posição. Em seguida, chama recursivamente a si mesma para processar o restante da lista.

Espero que este exemplo básico seja útil para você começar a explorar a programação em ERLANG.