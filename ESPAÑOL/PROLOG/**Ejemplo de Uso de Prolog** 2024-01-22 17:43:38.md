```prolog
:- dynamic regla/2.
:- dynamic hecho/1.

% Define una regla con cabeza H y cuerpo B.
regla(H, B) :-
    assert(regla(H, B)).

% Agrega un hecho H a la base de conocimientos.
hecho(H) :-
    assert(hecho(H)).

% Consulta la base de conocimientos para determinar si H es verdadero.
consulta(H) :-
    hecho(H).
consulta(H) :-
    regla(H, B),
    consulta_lista(B).

% Consulta una lista de metas B.
consulta_lista([]).
consulta_lista([H|T]) :-
    consulta(H),
    consulta_lista(T).

% Muestra en pantalla el resultado de una consulta.
mostrar_resultado(H) :-
    format('El resultado de la consulta es: ~w~n', [H]).

% Ejemplo de uso
hecho(perro(firulais)).
regla(ladra(X), [perro(X)]).
consulta(ladra(firulais)).
mostrar_resultado(H).
```

Explicación:

* El código utiliza los predicados `regla/2` y `hecho/1` para representar el conocimiento en la base de conocimientos.
* El predicado `regla/2` define una regla con cabeza H y cuerpo B.
* El predicado `hecho/1` agrega un hecho H a la base de conocimientos.
* El predicado `consulta/1` consulta la base de conocimientos para determinar si H es verdadero.
* El predicado `consulta_lista/1` consulta una lista de metas B.
* El predicado `mostrar_resultado/1` muestra en pantalla el resultado de una consulta.
* En el ejemplo de uso, se define un hecho `perro(firulais)` y una regla `ladra(X), [perro(X)]`.
* Luego, se consulta la base de conocimientos para determinar si `ladra(firulais)` es verdadero.
* Finalmente, se muestra en pantalla el resultado de la consulta.