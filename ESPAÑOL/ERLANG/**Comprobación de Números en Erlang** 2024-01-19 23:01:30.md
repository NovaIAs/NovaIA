```erlang
-module(mi_modulo).
-export([mi_función/1, mi_otra_función/2]).

mi_función(N) ->
    if
        N > 0 ->
            io:format("El número ~p es mayor que cero.~n", [N]);
        N < 0 ->
            io:format("El número ~p es menor que cero.~n", [N]);
        N =:= 0 ->
            io:format("El número ~p es cero.~n", [N])
    end.

mi_otra_función(N, M) ->
    if
        N > M ->
            io:format("El número ~p es mayor que el número ~p.~n", [N, M]);
        N < M ->
            io:format("El número ~p es menor que el número ~p.~n", [N, M]);
        N =:= M ->
            io:format("Los números ~p y ~p son iguales.~n", [N, M])
    end.
```