```prolog
:- dynamic clausulas/2.
:- dynamic objetivo/1.
:- dynamic hechos/1.
:- dynamic variables/1.

% Definición de la cláusula "padre"
clausulas(padre(pedro, juan)).
clausulas(padre(juan, maria)).
clausulas(padre(maria, luis)).

% Definición del objetivo
objetivo(padre(X, maria)).

% Definición de los hechos
hechos(padre(pedro, juan)).
hechos(padre(juan, maria)).
hechos(padre(maria, luis)).

% Definición de las variables
variables(X).
variables(Y).
variables(Z).

% Regla de inferencia para "padre"
padre(X, Y) :- clausulas(padre(X, Y)).
padre(X, Y) :- clausulas(padre(Y, Z)), padre(X, Z).

% Regla de inferencia para "objetivo"
objetivo(X) :- hechos(X).
objetivo(X) :- clausulas(X), variables(Y), objetivo(Y).

% Búsqueda en profundidad
buscar :-
    objetivo(X),
    writeln(X),
    fail.

% Programa principal
main :-
    buscar.
```

Explicación del código:

* La primera línea define el predicado `clausulas/2`, que se utiliza para representar las cláusulas de Horn.
* La segunda línea define el predicado `objetivo/1`, que se utiliza para representar el objetivo de la consulta.
* La tercera línea define el predicado `hechos/1`, que se utiliza para representar los hechos conocidos.
* La cuarta línea define el predicado `variables/1`, que se utiliza para representar las variables de la consulta.
* La quinta línea define la regla de inferencia para el predicado `padre/2`.
* La sexta línea define la regla de inferencia para el predicado `objetivo/1`.
* La séptima línea define el predicado `buscar/0`, que se utiliza para realizar una búsqueda en profundidad en el árbol de búsqueda.
* La octava línea define el predicado `main/0`, que es el punto de entrada del programa.

Cuando se ejecuta el programa, se llama al predicado `main/0`. Este predicado llama al predicado `buscar/0`, que realiza una búsqueda en profundidad en el árbol de búsqueda. La búsqueda se realiza siguiendo las reglas de inferencia definidas para los predicados `padre/2` y `objetivo/1`. Cuando se encuentra una solución, se imprime en la pantalla y se llama al predicado `fail/0` para continuar la búsqueda.