```prolog

% Este código busca una solución al problema de la n-reinas, utilizando un algoritmo de búsqueda en anchura.

% El predicado n_reinas(N, Solución) busca una solución al problema de las N-reinas, devolviendo en la variable Solución una lista con las posiciones de las reinas en el tablero.

n_reinas(N, Solución) :-
    findall(S,
            (   between(1, N, Col),
                findall(R,
                        (   between(1, N, R),
                            valido(N, R, Col)
                        ), Rutas),
                permutation(Rutas, Solución)
            ), Soluciones),
    member(Objetivo, Soluciones),
    !.

% El predicado valido(N, R, C) comprueba si una reina puede colocarse en la fila R y la columna C del tablero de tamaño N sin amenazar a ninguna otra reina.

valido(N, R, C) :-
    no_amenaza_fila(N, R),
    no_amenaza_diagonal(N, R, C),
    no_amenaza_columna(N, C).

% El predicado no_amenaza_fila(N, R) comprueba si una reina puede colocarse en la fila R del tablero de tamaño N sin amenazar a ninguna otra reina.

no_amenaza_fila(N, R) :-
    between(1, N, C),
    not(amenaza(R, C)).

% El predicado no_amenaza_diagonal(N, R, C) comprueba si una reina puede colocarse en la fila R y la columna C del tablero de tamaño N sin amenazar a ninguna otra reina en las diagonales.

no_amenaza_diagonal(N, R, C) :-
    between(1, N, C1),
    between(1, N, R1),
    diferencia(R, C, R1, C1) =/= 0,
    not(amenaza(R, C)).

% El predicado no_amenaza_columna(N, C) comprueba si una reina puede colocarse en la columna C del tablero de tamaño N sin amenazar a ninguna otra reina.

no_amenaza_columna(N, C) :-
    between(1, N, R),
    not(amenaza(R, C)).

% El predicado amenaza(R1, C1) comprueba si una reina colocada en la fila R1 y la columna C1 amenaza a alguna otra reina.

amenaza(R1, C1) :-
    between(1, 8, R2),
    between(1, 8, C2),
    diferencia(R1, C1, R2, C2) = 0,
    R1 \= R2,
    C1 \= C2.

% El predicado diferencia(X1, Y1, X2, Y2) calcula la diferencia entre dos pares de coordenadas.

diferencia(X1, Y1, X2, Y2) :-
    X1 - X2 = DifX,
    Y1 - Y2 = DifY,
    DifX * DifX + DifY * DifY = Dif.

```