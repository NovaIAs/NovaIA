```prolog

% Definición de la regla de inferencia.
sí(X) :-
    verdad(X).
sí(X) :-
    sí(Y),
    implica(Y, X).

% Definición de la regla de inferencia.
no(X) :-
    falso(X).
no(X) :-
    no(Y),
    implica(X, Y).

% Definición de la regla de inferencia.
tal_vez(X) :-
    no(no(X)).

% Definición de la regla de inferencia.
implica(X, Y) :-
    sí(X) -> sí(Y).

% Definición de la base de conocimiento (hechos).
verdad(ser_humano(juan)).
falso(ser_humano(piedra)).

% Definición de una consulta (meta).
meta(ser_humano(juan)).

% Ejecución de la consulta.
objetivo :-
    meta(X),
    sí(X).

```