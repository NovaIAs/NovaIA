```
:- dynamic amo/2.
:- dynamic existe/1.

% Amo de A es B
amo(A, B) :-
    existe(A),
    existe(B),
    A \== B.

% A no tiene amo
libre(A) :-
    \+ amo(A, _).

% Todos los objetos posibles
objetos([a, b, c, d, e, f, g, h, i, j, k, l, m, n, ñ, o, p, q, r, s, t, u, v, w, x, y, z]).

% Crea todos los objetos posibles y los hace libres
crear_objetos :-
    objetos(Objetos),
    forall(member(Objeto, Objetos),
        assert(existe(Objeto)),
        assert(libre(Objeto))).

% Busca un objeto libre y lo asigna como amo del objeto A
asignar_amo(A) :-
    libre(B),
    retract(libre(B)),
    assert(amo(A, B)).

% Libera el objeto A de su amo
liberar_amo(A) :-
    amo(A, B),
    retract(amo(A, B)),
    assert(libre(A)).

% Busca un ciclo en el grafo de amores
ciclo :-
    existe(A),
    ciclo(A, [A]).

ciclo(A, Visitados) :-
    amo(A, B),
    \+ member(B, Visitados),
    ciclo(B, [B|Visitados]).

% Encuentra todos los ciclos en el grafo de amores
todos_los_ciclos :-
    findall(Ciclo, ciclo(A, _), Ciclos),
    sort(Ciclos, TodosLosCiclos).

% Muestra todos los ciclos encontrados
mostrar_ciclos :-
    todos_los_ciclos(Ciclos),
    forall(member(Ciclo, Ciclos),
        write(Ciclo), nl).

% Crea los objetos y los hace todos libres
crear_objetos.

% Asigna un amo a cada objeto que no lo tenga
forall(existe(A), (libre(A) -> asignar_amo(A); true)).

% Muestra los ciclos encontrados
mostrar_ciclos.
```