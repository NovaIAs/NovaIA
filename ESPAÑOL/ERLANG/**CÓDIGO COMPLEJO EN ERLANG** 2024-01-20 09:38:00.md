```erlang
-module(complex_erlang_code).

-export([factorial/1, fibonacci/1, count_sort/2, merge_sort/1, binary_search/3]).

% Calcula el factorial de un número
factorial(N) ->
    factorial(N, 1).

factorial(0, Acc) ->
    Acc;
factorial(N, Acc) ->
    factorial(N-1, Acc*N).

% Calcula el n-esimo número de Fibonacci
fibonacci(N) ->
    fibonacci(N, 0, 1).

fibonacci(0, _, _) ->
    0;
fibonacci(1, _, _) ->
    1;
fibonacci(N, A, B) ->
    fibonacci(N-1, B, A+B).

% Ordena una lista de números usando el algoritmo de ordenación por recuento
count_sort(List, Max) ->
    Counts = lists:duplicate(Max+1, 0),
    CountsList = lists:zipwith(fun(_, Count) -> Count end, List, Counts),
    SortedList = lists:foldl(fun({N, Count}, Acc) ->
        lists:duplicate(Count, N) ++ Acc
    end, [], CountsList),
    SortedList.

% Ordena una lista de números usando el algoritmo de ordenación por mezcla
merge_sort([]) ->
    [];
merge_sort([H|T]) ->
    {L1, L2} = lists:split(length(T) div 2, [H|T]),
    merge(merge_sort(L1), merge_sort(L2)).

merge([], L2) ->
    L2;
merge(L1, []) ->
    L1;
merge([H1|T1], [H2|T2]) ->
    if
        H1 < H2 ->
            [H1|merge(T1, [H2|T2])];
        true ->
            [H2|merge([H1|T1], T2)]
    end.

% Busca un número en una lista ordenada usando el algoritmo de búsqueda binaria
binary_search(N, [H|_], 0) ->
    -1;
binary_search(N, [H|T], I) ->
    if
        N == H ->
            I;
        N < H ->
            binary_search(N, T, I-1);
        true ->
            binary_search(N, T, I+1)
    end.
```

Explicación:

* La función `factorial/1` calcula el factorial de un número entero no negativo utilizando recursión.
* La función `fibonacci/1` calcula el n-esimo número de la secuencia de Fibonacci utilizando recursión.
* La función `count_sort/2` ordena una lista de números enteros no negativos utilizando el algoritmo de ordenación por recuento.
* La función `merge_sort/1` ordena una lista de números enteros utilizando el algoritmo de ordenación por mezcla.
* La función `binary_search/3` busca un número entero en una lista ordenada de números enteros utilizando el algoritmo de búsqueda binaria.