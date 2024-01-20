```prolog
:- dynamic jugador/3.
:- dynamic resultado/3.

% Agregar un jugador a la base de conocimientos
agregar_jugador(Nombre, Nivel, Equipo) :-
    assert(jugador(Nombre, Nivel, Equipo)).

% Obtener los datos de un jugador
obtener_jugador(Nombre, Nivel, Equipo) :-
    jugador(Nombre, Nivel, Equipo).

% Establecer el resultado de un partido
establecer_resultado(Equipo1, Equipo2, Goles1, Goles2) :-
    assert(resultado(Equipo1, Equipo2, [Goles1, Goles2])).

% Obtener el resultado de un partido
obtener_resultado(Equipo1, Equipo2, Goles1, Goles2) :-
    resultado(Equipo1, Equipo2, [Goles1, Goles2]).

% Calcular la diferencia de goles de un equipo en un partido
diferencia_goles(Equipo, Partido, Diferencia) :-
    obtener_resultado(Equipo, _, Goles1, Goles2),
    Diferencia is Goles1 - Goles2.

% Obtener los partidos jugados por un equipo
partidos_jugados(Equipo, Partidos) :-
    findall(Partido, resultado(Equipo, _, Partido), Partidos).

% Obtener los partidos ganados por un equipo
partidos_ganados(Equipo, Partidos) :-
    findall(Partido, (resultado(Equipo, _, Partido), diferencia_goles(Equipo, Partido, Diferencia), Diferencia > 0), Partidos).

% Obtener los partidos perdidos por un equipo
partidos_perdidos(Equipo, Partidos) :-
    findall(Partido, (resultado(Equipo, _, Partido), diferencia_goles(Equipo, Partido, Diferencia), Diferencia < 0), Partidos).

% Obtener los partidos empatados por un equipo
partidos_empatados(Equipo, Partidos) :-
    findall(Partido, (resultado(Equipo, _, Partido), diferencia_goles(Equipo, Partido, Diferencia), Diferencia = 0), Partidos).

% Calcular el número de puntos de un equipo
puntos(Equipo, Puntos) :-
    partidos_ganados(Equipo, PartidosGanados),
    partidos_empatados(Equipo, PartidosEmpatados),
    Puntos is 3 * PartidosGanados + PartidosEmpatados.

% Obtener la clasificación de los equipos
clasificacion(Clasificacion) :-
    findall(Equipo, (jugador(Equipo, _, _), puntos(Equipo, Puntos)), Equipos),
    sort(Puntos, Equipos, Clasificacion).
```

Este código implementa un sistema básico de gestión de datos sobre partidos de fútbol en PROLOG. Incluye funciones para agregar jugadores, establecer los resultados de los partidos, calcular la diferencia de goles, obtener los partidos jugados, ganados, perdidos y empatados por un equipo, calcular el número de puntos de un equipo y obtener la clasificación de los equipos.

El código utiliza la base de conocimientos dinámica de PROLOG para almacenar los datos sobre jugadores y partidos. Las reglas de PROLOG se utilizan para definir las relaciones entre los datos y para realizar cálculos sobre ellos.

El código incluye varios ejemplos de consultas que se pueden utilizar para obtener información sobre los jugadores, los partidos y la clasificación de los equipos.

A continuación se muestran algunos ejemplos de consultas que se pueden realizar con este código:

* Obtener los datos de un jugador:

```prolog
?- obtener_jugador("Juan Pérez", Nivel, Equipo).
```

* Establecer el resultado de un partido:

```prolog
?- establecer_resultado("Real Madrid", "Barcelona", 3, 1).
```

* Obtener el resultado de un partido:

```prolog
?- obtener_resultado("Real Madrid", "Barcelona", Goles1, Goles2).
```

* Calcular la diferencia de goles de un equipo en un partido:

```prolog
?- diferencia_goles("Real Madrid", "Barcelona", Diferencia).
```

* Obtener los partidos jugados por un equipo:

```prolog
?- partidos_jugados("Real Madrid", Partidos).
```

* Obtener los partidos ganados por un equipo:

```prolog
?- partidos_ganados("Real Madrid", Partidos).
```

* Obtener los partidos perdidos por un equipo:

```prolog
?- partidos_perdidos("Real Madrid", Partidos).
```

* Obtener los partidos empatados por un equipo:

```prolog
?- partidos_empatados("Real Madrid", Partidos).
```

* Calcular el número de puntos de un equipo:

```prolog
?- puntos("Real Madrid", Puntos).
```

* Obtener la clasificación de los equipos:

```prolog
?- clasificacion(Clasificacion).
```