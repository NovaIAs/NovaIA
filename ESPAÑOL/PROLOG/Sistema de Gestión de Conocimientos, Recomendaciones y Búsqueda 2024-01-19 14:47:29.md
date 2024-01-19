```prolog
:- dynamic conocimiento/2.

% Añadir un nuevo conocimiento a la base de conocimiento
agregar_conocimiento(Atomo, Valor) :-
    assert(conocimiento(Atomo, Valor)).

% Consultar la base de conocimiento para un átomo determinado
consultar_conocimiento(Atomo, Valor) :-
    conocimiento(Atomo, Valor).

% Borrar un conocimiento de la base de conocimiento
borrar_conocimiento(Atomo) :-
    retract(conocimiento(Atomo, _)).

% Listar todos los conocimientos de la base de conocimiento
listar_conocimientos :-
    findall(Conocimiento, conocimiento(Conocimiento, _), Conocimientos),
    sort(Conocimientos, ConocimientosOrdenados),
    maplist(write, ConocimientosOrdenados),
    nl.

% Definir las reglas del sistema de recomendaciones
:- dynamic recomendacion/2.

% Generar una recomendación para un usuario determinado
generar_recomendacion(Usuario, Recomendacion) :-
    findall(Item, (
        consultar_conocimiento(usuario_gustos(Usuario, Item)),
        not(consultar_conocimiento(usuario_recomendaciones(Usuario, Item)))
    ), Items),
    sort(Items, ItemsOrdenados),
    first(Recomendacion, ItemsOrdenados).

% Añadir una recomendación a la base de conocimiento
agregar_recomendacion(Usuario, Recomendacion) :-
    assert(recomendacion(Usuario, Recomendacion)).

% Consultar las recomendaciones para un usuario determinado
consultar_recomendaciones(Usuario, Recomendaciones) :-
    findall(Recomendacion, recomendacion(Usuario, Recomendacion), Recomendaciones).

% Borrar una recomendación de la base de conocimiento
borrar_recomendacion(Usuario, Recomendacion) :-
    retract(recomendacion(Usuario, Recomendacion)).

% Listar todas las recomendaciones de la base de conocimiento
listar_recomendaciones :-
    findall(Recomendacion, recomendacion(_, Recomendacion), Recomendaciones),
    sort(Recomendaciones, RecomendacionesOrdenadas),
    maplist(write, RecomendacionesOrdenadas),
    nl.

% Definir las reglas del sistema de búsqueda
:- dynamic busqueda/2.

% Realizar una búsqueda en la base de conocimiento
realizar_busqueda(Query, Resultados) :-
    findall(Resultado, (
        consultar_conocimiento(Resultado, Query)
    ), Resultados).

% Añadir un resultado de búsqueda a la base de conocimiento
agregar_resultado_busqueda(Query, Resultado) :-
    assert(busqueda(Query, Resultado)).

% Consultar los resultados de búsqueda para una consulta determinada
consultar_resultados_busqueda(Query, Resultados) :-
    findall(Resultado, busqueda(Query, Resultado), Resultados).

% Borrar un resultado de búsqueda de la base de conocimiento
borrar_resultado_busqueda(Query, Resultado) :-
    retract(busqueda(Query, Resultado)).

% Listar todos los resultados de búsqueda de la base de conocimiento
listar_resultados_busqueda :-
    findall(Resultado, busqueda(_, Resultado), Resultados),
    sort(Resultados, ResultadosOrdenados),
    maplist(write, ResultadosOrdenados),
    nl.
```

Explicación del código:

* Se definen los predicados `agregar_conocimiento/2`, `consultar_conocimiento/2`, `borrar_conocimiento/1`, `listar_conocimientos/0`, `generar_recomendacion/2`, `agregar_recomendacion/2`, `consultar_recomendaciones/2`, `borrar_recomendacion/2`, `listar_recomendaciones/0`, `realizar_busqueda/2`, `agregar_resultado_busqueda/2`, `consultar_resultados_busqueda/2`, `borrar_resultado_busqueda/2` y `listar_resultados_busqueda/0` para gestionar los conocimientos, las recomendaciones y los resultados de búsqueda.
* Se definen las reglas del sistema de recomendaciones y del sistema de búsqueda.
* Se añade un nuevo conocimiento a la base de conocimiento utilizando el predicado `agregar_conocimiento/2`.
* Se consulta la base de conocimiento para un átomo determinado utilizando el predicado `consultar_conocimiento/2`.
* Se borra un conocimiento de la base de conocimiento utilizando el predicado `borrar_conocimiento/1`.
* Se listan todos los conocimientos de la base de conocimiento utilizando el predicado `listar_conocimientos/0`.
* Se genera una recomendación para un usuario determinado utilizando el predicado `generar_recomendacion/2`.
* Se añade una recomendación a la base de conocimiento utilizando el predicado `agregar_recomendacion/2`.
* Se consultan las recomendaciones para un usuario determinado utilizando el predicado `consultar_recomendaciones/2`.
* Se borra una recomendación de la base de conocimiento utilizando el predicado `borrar_recomendacion/2`.
* Se listan todas las recomendaciones de la base de conocimiento utilizando el predicado `listar_recomendaciones/0`.
* Se realiza una búsqueda en la base de conocimiento utilizando el predicado `realizar_busqueda/2`.
* Se añade un resultado de búsqueda a la base de conocimiento utilizando el predicado `agregar_resultado_busqueda/2`.
* Se consultan los resultados de búsqueda para una consulta determinada utilizando el predicado `consultar_resultados_busqueda/2`.
* Se borra un resultado de búsqueda de la base de conocimiento utilizando el predicado `borrar_resultado_busqueda/2`.
* Se listan todos los resultados de búsqueda de la base de conocimiento utilizando el predicado `listar_resultados_busqueda/0`.