Claro! Aqui está um exemplo de código em Erlang que implementa um algoritmo de ordenação chamado Quicksort. O Quicksort é um algoritmo de ordenação eficiente que utiliza a estratégia de dividir e conquistar.

```erlang
-module(quicksort).
-export([sort/1]).

%% Função principal que chama o algoritmo de ordenação
sort(List) ->
    quicksort(List, 1, length(List)).

%% Função recursiva que realiza a ordenação
quicksort(List, Low, High) ->
    case Low < High of
        true ->
            PivotIndex = partition(List, Low, High),
            quicksort(List, Low, PivotIndex - 1),
            quicksort(List, PivotIndex + 1, High);
        false ->
            List
    end.

%% Função auxiliar que particiona a lista em relação a um pivô
partition(List, Low, High) ->
    Pivot = element(High, List),
    partition(List, Low, High, Low-1).

partition(List, Low, High, I) ->
    case Low < High of
        true ->
            case element(Low, List) =< Pivot of
                true ->
                    NewI = I + 1,
                    swap(List, NewI, Low),
                    partition(List, Low + 1, High, NewI);
                false ->
                    partition(List, Low + 1, High, I)
            end;
        false ->
            NewI = I + 1,
            swap(List, NewI, High),
            NewI
    end.

%% Função auxiliar para trocar elementos de posição na lista
swap(List, I, J) ->
    [H|T] = List,
    swap(List, I, J, H, T).

swap(List, I, J, H, T) when I =:= 1, J =:= 2 ->
    [H2|T2] = T,
    [element(J, List), H2 | lists:nthtail(2, T2)];
swap(List, I, J, H, T) when I =:= 1 ->
    [H2|T2] = T,
    [element(J, List) | swap(List, I, J - 1, H2, T2)];
swap(List, I, J, H, T) ->
    [H2|T2] = T,
    [H | swap(List, I - 1, J - 1, H2, T2)].
```

Este código implementa o algoritmo Quicksort em Erlang. A função `sort/1` é a função principal que chama o algoritmo de ordenação `quicksort/3`. A função `quicksort/3` é uma função recursiva que realiza a ordenação.

O algoritmo de Quicksort é baseado na ideia de escolher um elemento pivô da lista e particionar a lista em relação a esse pivô. Os elementos menores que o pivô são colocados à esquerda, e os elementos maiores são colocados à direita. Em seguida, o algoritmo é aplicado recursivamente nas sublistas à esquerda e à direita do pivô.

A função `partition/4` é responsável por particionar a lista em relação ao pivô. Ela recebe a lista, os índices de início (`Low`) e fim (`High`) da sublistas a serem particionadas, e um índice `I` que representa a posição corrente do pivô na lista. A função `swap/3` é uma função auxiliar que troca elementos de posição na lista.

Este código é uma implementação básica do Quicksort em Erlang e pode ser usado para ordenar qualquer lista de elementos comparáveis. Vale ressaltar que existem várias formas de implementar o Quicksort e este é apenas um exemplo.