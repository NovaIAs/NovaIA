```erlang
% Definición de la función recursiva factorial
factorial(N) when N > 0 -> N * factorial(N-1);
factorial(0) -> 1.

% Definición de la función recursiva fibonacci
fibonacci(N) when N > 1 -> fibonacci(N-1) + fibonacci(N-2);
fibonacci(1) -> 1;
fibonacci(0) -> 0.

% Definición de la función para encontrar el máximo común divisor de dos números
gcd(A, B) ->
    case B > 0 of
        true -> gcd(B, A rem B);
        false -> abs(A)
    end.

% Definición de la función para ordenar una lista
sort(List) ->
    merge_sort(List, []).

merge_sort([], Result) -> Result;
merge_sort([H|T], Result) ->
    {L1, L2} = split_list(T),
    merge_sort(L1, L),
    merge_sort(L2, R),
    merge(L, R, Result).

split_list(List) ->
    Length = length(List) div 2,
    Left = lists:sublist(List, Length),
    Right = lists:reverse(lists:sublist(List, Length + 1)),
    {Left, Right}.

merge([], L, Result) -> [L | Result];
merge(R, [], Result) -> [R | Result];
merge([H|T], [H1|T1], Result) when H =< H1 ->
    merge(T, [H1|T1], [H|Result]);
merge([H|T], [H1|T1], Result) ->
    merge([H|T], T1, [H1|Result]).

% Definición de la función para invertir una cadena
reverse_string(String) ->
    reverse_string(String, "").

reverse_string([], Result) -> Result;
reverse_string([H|T], Result) ->
    reverse_string(T, [H|Result]).

% Función para encontrar el mayor número en una lista
max_number(List) ->
    max_number(List, List:hd()).

max_number([], CurrentMax) -> CurrentMax;
max_number([H|T], CurrentMax) when H > CurrentMax -> max_number(T, H);
max_number([H|T], CurrentMax) -> max_number(T, CurrentMax).

% Función para encontrar el menor número en una lista
min_number(List) ->
    min_number(List, List:hd()).

min_number([], CurrentMin) -> CurrentMin;
min_number([H|T], CurrentMin) when H < CurrentMin -> min_number(T, H);
min_number([H|T], CurrentMin) -> min_number(T, CurrentMin).

% Función para encontrar la suma de los números en una lista
sum_list(List) ->
    sum_list(List, 0).

sum_list([], Sum) -> Sum;
sum_list([H|T], Sum) -> sum_list(T, Sum + H).

% Función para encontrar el producto de los números en una lista
product_list(List) ->
    product_list(List, 1).

product_list([], Product) -> Product;
product_list([H|T], Product) -> product_list(T, Product * H).

% Función para contar el número de elementos en una lista
length_list(List) ->
    length_list(List, 0).

length_list([], Count) -> Count;
length_list([H|T], Count) -> length_list(T, Count + 1).

% Función para encontrar el elemento en una lista por índice
element_at(List, Index) ->
    element_at(List, Index, 1).

element_at([], _Index, _CurrentIndex) -> throw("Índice fuera de rango");
element_at([H|_T], Index, CurrentIndex) when Index = CurrentIndex -> H;
element_at([_H|T], Index, CurrentIndex) -> element_at(T, Index, CurrentIndex + 1).

% Función para eliminar un elemento de una lista por índice
remove_at(List, Index) ->
    remove_at(List, Index, 1, []).

remove_at([], _Index, _CurrentIndex, Result) -> Result;
remove_at([H|T], Index, CurrentIndex, Result) when Index = CurrentIndex -> Result ++ T;
remove_at([H|T], Index, CurrentIndex, Result) -> remove_at(T, Index, CurrentIndex + 1, [H|Result]).

% Función para insertar un elemento en una lista en un índice específico
insert_at(List, Index, Element) ->
    insert_at(List, Index, Element, 1, []).

insert_at([], _Index, _Element, _CurrentIndex, Result) -> Result;
insert_at([H|T], Index, Element, CurrentIndex, Result) when Index = CurrentIndex -> [Element | (Result ++ T)];
insert_at([H|T], Index, Element, CurrentIndex, Result) -> [H | insert_at(T, Index, Element, CurrentIndex + 1, Result)].

% Función para encontrar todos los índices de un elemento en una lista
find_all(List, Element) ->
    find_all(List, Element, 1, []).

find_all([], _Element, _CurrentIndex, Result) -> Result;
find_all([H|T], Element, CurrentIndex, Result) when H = Element -> [CurrentIndex | find_all(T, Element, CurrentIndex + 1, Result)];
find_all([_H|T], Element, CurrentIndex, Result) -> find_all(T, Element, CurrentIndex + 1, Result).

% Función para eliminar todos los elementos duplicados de una lista
remove_duplicates(List) ->
    remove_duplicates(List, []).

remove_duplicates([], Result) -> Result;
remove_duplicates([H|T], Result) ->
    case lists:member(H, Result) of
        true -> remove_duplicates(T, Result);
        false -> remove_duplicates(T, [H|Result])
    end.

% Función para encontrar la unión de dos listas
union(List1, List2) ->
    union(List1, List2, []).

union([], List2, Result) -> Result ++ List2;
union(List1, [], Result) -> Result ++ List1;
union([H1|T1], [H2|T2], Result) ->
    case lists:member(H1, Result) of
        true -> union(T1, T2, Result);
        false -> union(T1, T2, [H1|Result])
    end.

% Función para encontrar la intersección de dos listas
intersection(List1, List2) ->
    intersection(List1, List2, []).

intersection([], _List2, Result) -> Result;
intersection(_List1, [], Result) -> Result;
intersection([H1|T1], [H2|T2], Result) ->
    case lists:member(H1, List2) of
        true -> intersection(T1, T2, [H1|Result]);
        false -> intersection(T1, T2, Result)
    end.

% Función para encontrar la diferencia de dos listas
difference(List1, List2) ->
    difference(List1, List2, []).

difference([], _List2, Result) -> Result;
difference(List1, [], Result) -> Result;
difference([H1|T1], [H2|T2], Result) ->
    case lists:member(H1, List2) of
        true -> difference(T1, T2, Result);
        false -> difference(T1, T2, [H1|Result])
    end.
```

Explicación:

1. Factorial: Esta función calcula el factorial de un número.
2. Fibonacci: Esta función calcula el n-ésimo número de Fibonacci.
3. Máximo común divisor: Esta función calcula el máximo común divisor de dos números.
4. Ordenar una lista: Esta función ordena una lista en orden ascendente.
5. Invertir una cadena: Esta función invierte una cadena.
6. Encontrar el mayor número en una lista: Esta función encuentra el mayor número en una lista.
7. Encontrar el menor número en una lista: Esta función encuentra el menor número en una lista.
8. Sumar los números en una lista: Esta función suma todos los números en una lista.
9. Multiplicar los números en una lista: Esta función multiplica todos los números en una lista.
10. Contar el número de elementos en una lista: Esta función cuenta el número de elementos en una lista.
11. Encontrar el elemento en una lista por índice: Esta función encuentra el elemento en una lista por índice.
12. Eliminar un elemento de una lista por índice: Esta función elimina un elemento de una lista por índice.
13. Insertar un elemento en una lista en un índice específico: Esta función inserta un elemento en una lista en un índice específico.
14. Encontrar todos los índices de un elemento en una lista: Esta función encuentra todos los índices de un elemento en una lista.
15. Eliminar todos los elementos duplicados de una lista: Esta función elimina todos los elementos duplicados de una lista.
16. Encontrar la unión de dos listas: Esta función encuentra la unión de dos listas.
17. Encontrar la intersección de dos listas: Esta función encuentra la intersección de dos listas.
18. Encontrar la diferencia de dos listas: Esta función encuentra la diferencia de dos listas.