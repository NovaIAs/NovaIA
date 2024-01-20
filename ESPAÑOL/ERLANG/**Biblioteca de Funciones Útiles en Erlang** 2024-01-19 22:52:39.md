```erlang
-module(mi_primer_modulo).

-export([hola/0, suma/2, longitud/1, mayor_elemento/1, invertir/1, concatenar/2, busqueda_binaria/3, ordenamiento_burbuja/1, ordenamiento_mezcla/1, ordenamiento_rapido/1]).

-import(lists, [reverse/1]).

hola() ->
    io:fwrite("¡Hola, mundo!").

suma(A, B) ->
    A + B.

longitud([]) ->
    0;
longitud([_|T]) ->
    1 + longitud(T).

mayor_elemento(L) ->
    mayor_elemento(L, element(1, L)).

mayor_elemento([], M) ->
    M;
mayor_elemento([H|T], M) when H > M ->
    mayor_elemento(T, H);
mayor_elemento([_|T], M) ->
    mayor_elemento(T, M).

invertir(L) ->
    reverse(L).

concatenar(L1, L2) ->
    L1 ++ L2.

busqueda_binaria(X, [], _) ->
    -1;
busqueda_binaria(X, [H|T], I) when X == H ->
    I;
busqueda_binaria(X, [H|T], I) when X < H ->
    busqueda_binaria(X, T, I-1);
busqueda_binaria(X, [_|T], I) ->
    busqueda_binaria(X, T, I+1).

ordenamiento_burbuja(L) ->
    ordenamiento_burbuja(L, []).

ordenamiento_burbuja([], S) ->
    S;
ordenamiento_burbuja([H|T], S) ->
    ordenamiento_burbuja(T, insertar_burbuja(H, S)).

insertar_burbuja(X, []) ->
    [X];
insertar_burbuja(X, [H|T]) when X <= H ->
    [X, H|T];
insertar_burbuja(X, [H|T]) ->
    [H|insertar_burbuja(X, T)].

ordenamiento_mezcla(L) ->
    ordenamiento_mezcla(L, []).

ordenamiento_mezcla([], S) ->
    S;
ordenamiento_mezcla([H|T], S) ->
    ordenamiento_mezcla(T, insertar_mezcla(H, S)).

insertar_mezcla(X, []) ->
    [X];
insertar_mezcla(X, [H|T]) when X <= H ->
    [X, H|T];
insertar_mezcla(X, [H|T]) ->
    [H|insertar_mezcla(X, T)].

ordenamiento_rapido(L) ->
    ordenamiento_rapido(L, []).

ordenamiento_rapido([], S) ->
    S;
ordenamiento_rapido([H|T], S) ->
    ordenamiento_rapido(T, insertar_rapido(H, S)).

insertar_rapido(X, []) ->
    [X];
insertar_rapido(X, [H|T]) when X <= H ->
    [X, H|T];
insertar_rapido(X, [H|T]) ->
    [H|insertar_rapido(X, T)].
```