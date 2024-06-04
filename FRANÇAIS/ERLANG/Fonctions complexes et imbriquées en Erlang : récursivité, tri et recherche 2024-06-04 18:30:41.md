```erlang

-module(complex_erlang_code).

-export([main/0]).

main() ->
  % Définir des fonctions récursives imbriquées
  fibonacci(N) ->
    case N of
      0 -> 0;
      1 -> 1;
      _ -> fibonacci(N-1) + fibonacci(N-2)
    end;

  factorial(N) ->
    case N of
      0 -> 1;
      _ -> N * factorial(N-1)
    end;

  % Définir une fonction de tri par fusion
  merge_sort(List) ->
    Len = length(List),
    case Len of
      0 -> [];
      1 -> List;
      _ ->
        Mid = Len div 2,
        Left = merge_sort(lists:sublist(List, 1, Mid)),
        Right = merge_sort(lists:sublist(List, Mid+1, Len)),
        merge(Left, Right)
    end;

  merge([], []) -> [];
  merge([], Right) -> Right;
  merge(Left, []) -> Left;
  merge([L1|LT], [R1|RT]) ->
    case L1 < R1 of
      true -> [L1|merge(LT, [R1|RT])];
      false -> [R1|merge([L1|LT], RT)]
    end;

  % Définir une fonction de recherche binaire
  binary_search(Target, List) ->
    binary_search(Target, List, 1, length(List));

  binary_search(_, [], _, _) ->
    {error, element_not_found};

  binary_search(Target, [Head|_], Start, End) ->
    case Head of
      Target -> {ok, Start};
      _ -> binary_search(Target, lists:nthtail(1, [Head|_]), Start+1, End)
    end;

  % Générer des nombres aléatoires
  random_numbers(N) ->
    lists:map(fun(_) -> random:uniform() end, lists:seq(1, N));

  % Exécuter les fonctions et afficher les résultats
  io:format("Fibonacci de 10 : ~p~n", [fibonacci(10)]),
  io:format("Factorielle de 5 : ~p~n", [factorial(5)]),
  io:format("Liste triée : ~p~n", [merge_sort([5, 1, 7, 4, 9, 2, 8, 3, 6])]),
  io:format("Recherche binaire de 4 dans [5, 1, 7, 4, 9, 2, 8, 3, 6] : ~p~n", [binary_search(4, [5, 1, 7, 4, 9, 2, 8, 3, 6])]),
  io:format("Nombres aléatoires : ~p~n", [random_numbers(10)]).

```

**Explications du code :**

* Les fonctions `fibonacci` et `factorial` sont des fonctions récursives imbriquées qui calculent respectivement la suite de Fibonacci et la factorielle.
* La fonction `merge_sort` est une fonction de tri par fusion qui divise une liste en deux, trie récursivement chaque partie et fusionne les résultats triés.
* La fonction `binary_search` est une fonction de recherche binaire qui recherche une cible dans une liste triée en divisant la liste en deux jusqu'à ce que la cible soit trouvée ou que la liste soit vide.
* La fonction `random_numbers` génère une liste de nombres aléatoires.

Ce code montre des exemples de fonctions complexes et imbriquées en Erlang, couvrant la récursivité, le tri et la recherche.