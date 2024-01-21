```prolog
% Definición de la regla para sumar dos números.
sumar(X, Y, Z) :-
    Z is X + Y.

% Definición de la regla para restar dos números.
restar(X, Y, Z) :-
    Z is X - Y.

% Definición de la regla para multiplicar dos números.
multiplicar(X, Y, Z) :-
    Z is X * Y.

% Definición de la regla para dividir dos números.
dividir(X, Y, Z) :-
    Z is X / Y.

% Definición de la regla para calcular el factorial de un número.
factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Definición de la regla para comprobar si un número es primo.
primo(2).
primo(P) :-
    P > 1,
    \+ (between(2, P-1, X), P mod X = 0).

% Definición de la regla para generar una lista de números primos hasta un número dado.
primos_hasta(N, L) :-
    findall(P, (between(2, N, P), primo(P)), L).

% Definición de la regla para comprobar si una lista está ordenada.
ordenada([]).
ordenada([H|T]) :-
    ordenada(T),
    (\+ (member(X, T), X < H)).

% Definición de la regla para ordenar una lista.
ordenar(L, S) :-
    sort(L, S).

% Definición de la regla para buscar un elemento en una lista.
buscar(X, [X|_]) :- !.
buscar(X, [_|T]) :-
    buscar(X, T).

% Definición de la regla para eliminar un elemento de una lista.
eliminar(X, [], []).
eliminar(X, [X|T], T).
eliminar(X, [H|T], [H|S]) :-
    X \= H,
    eliminar(X, T, S).

% Definición de la regla para concatenar dos listas.
concatenar([], L, L).
concatenar([H|T], L, [H|S]) :-
    concatenar(T, L, S).

% Definición de la regla para invertir una lista.
invertir([], []).
invertir([H|T], R) :-
    invertir(T, R1),
    concatenar(R1, [H], R).
```

Este código en PROLOG incluye una variedad de reglas para realizar operaciones matemáticas básicas, como sumar, restar, multiplicar y dividir, así como calcular el factorial de un número. También incluye reglas para comprobar si un número es primo y para generar una lista de números primos hasta un número dado. Además, incluye reglas para comprobar si una lista está ordenada, ordenarla, buscar un elemento en una lista, eliminar un elemento de una lista y concatenar dos listas. Finalmente, incluye una regla para invertir una lista.