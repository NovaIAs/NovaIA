```erlang
-module(complex_erlang_code).

-export([
  primes_below/1,
  euclidean_distance/2,
  linear_search/2,
  binary_search/2,
  factorial/1,
  fibonacci/1,
  quicksort/1,
  mergesort/1,
  tower_of_hanoi/3,
  knapsack/2,
  traveling_salesman/2
]).

% Calculate the list of prime numbers below a given number `N` using the Sieve of Eratosthenes
primes_below(N) ->
    Primes = lists:seq(2, N),
    sieve(Primes, Primes).

sieve([], _) ->
    [];
sieve([Prime | Primes], Remaining) ->
    [Prime | sieve(
        lists:filter(fun(X) -> X rem Prime /= 0 end, Remaining),
        lists:delete(Prime, Primes))].

% Calculate the Euclidean distance between two points represented as tuples
euclidean_distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

% Find the index of an element in a sorted list using linear search
linear_search(Element, List) ->
    linear_search_helper(Element, List, 0).

linear_search_helper(_, [], _Index) ->
    -1;
linear_search_helper(Element, [H | T], Index) ->
    case Element == H of
        true -> Index;
        false -> linear_search_helper(Element, T, Index + 1)
    end.

% Find the index of an element in a sorted list using binary search
binary_search(Element, List) ->
    binary_search_helper(Element, List, 0, length(List) - 1).

binary_search_helper(_, [], _, _) ->
    -1;
binary_search_helper(Element, [H | T], Low, High) ->
    Mid = (Low + High) div 2,
    Compare = Element - lists:nth(Mid + 1, List),
    case Compare of
        0 -> Mid;
        N when N > 0 -> binary_search_helper(Element, T, Mid + 1, High);
        _ -> binary_search_helper(Element, List, Low, Mid - 1)
    end.

% Calculate the factorial of a non-negative integer `N`
factorial(N) when N < 0 ->
    erlang:error(badarg, [N]);
factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N - 1).

% Calculate the Nth Fibonacci number using recursion
fibonacci(N) when N < 0 ->
    erlang:error(badarg, [N]);
fibonacci(0) ->
    0;
fibonacci(1) ->
    1;
fibonacci(N) ->
    fibonacci(N - 2) + fibonacci(N - 1).

% Sort a list using the quicksort algorithm
quicksort([]) ->
    [];
quicksort([Pivot | Rest]) ->
    [X || X <- Rest, X < Pivot] ++ [Pivot] ++ [X || X <- Rest, X >= Pivot].

% Sort a list using the mergesort algorithm
mergesort([]) ->
    [];
mergesort([Pivot | Rest]) ->
    merge(mergesort(lists:sublist(Rest, 0, length(Rest) div 2)), mergesort(lists:sublist(Rest, length(Rest) div 2 + 1, length(Rest)))).

merge([], Acc) ->
    Acc;
merge(Acc, []) ->
    Acc;
merge([H1 | T1], [H2 | T2]) ->
    case H1 < H2 of
        true -> merge(T1, [H1 | Acc]);
        false -> merge([H2 | T2], Acc)
    end.

% Solve the Tower of Hanoi problem with `N` disks
tower_of_hanoi(N, From, To) ->
    Aux = 3 - From - To,
    tower_of_hanoi_helper(N, From, Aux, To).

tower_of_hanoi_helper(1, From, _, To) ->
    io:format("Move disk 1 from ~p to ~p~n", [From, To]);
tower_of_hanoi_helper(N, From, Aux, To) ->
    tower_of_hanoi_helper(N - 1, From, To, Aux),
    io:format("Move disk ~p from ~p to ~p~n", [N, From, To]),
    tower_of_hanoi_helper(N - 1, Aux, From, To).

% Solve the 0/1 Knapsack problem using dynamic programming
knapsack(Items, Capacity) ->
    knapsack_helper(Items, Capacity, 0, 0).

knapsack_helper([], _, _, Acc) ->
    Acc;
knapsack_helper([{Item, Weight, Value} | Rest], Capacity, CurrentWeight, Acc) ->
    case CurrentWeight + Weight =< Capacity of
        true ->
            knapsack_helper(Rest, Capacity, CurrentWeight + Weight, Acc + Value);
        false ->
            knapsack_helper(Rest, Capacity, CurrentWeight, Acc)
    end.

% Solve the Traveling Salesman Problem using the nearest neighbor algorithm
traveling_salesman(Cities, StartingPoint) ->
    tour = [StartingPoint | _],
    traveling_salesman_helper(Cities -- [StartingPoint], tour, StartingPoint).

traveling_salesman_helper([], Tour, _) ->
    Tour;
traveling_salesman_helper(RemainingCities, Tour, CurrentCity) ->
    NextCity = find_nearest_city(CurrentCity, RemainingCities),
    traveling_salesman_helper(RemainingCities -- [NextCity], Tour ++ [NextCity], NextCity).

find_nearest_city(CurrentCity, RemainingCities) ->
    lists:min(RemainingCities, fun(City) -> euclidean_distance(CurrentCity, City) end).
```
**Explanation:**
- `primes_below/1`: Finds all prime numbers up to a given number `N`.

- `euclidean_distance/2`: Calculates the Euclidean distance between two given points.

- `linear_search/2` and `binary_search/2`: Implement linear and binary search algorithms to find an element in a list.

- `factorial/1` and `fibonacci/1`: Calculate the factorial and Fibonacci number of a given integer `N`.

- `quicksort/1` and `mergesort/1`: Implement the quicksort and mergesort algorithms for sorting a list.

- `tower_of_hanoi/3`: Solves the Tower of Hanoi problem with `N` disks.

- `knapsack/2`: Solves the 0/1 Knapsack problem using dynamic programming.

- `traveling_salesman/2`: Implements a simple nearest neighbor algorithm for the Traveling Salesman Problem.