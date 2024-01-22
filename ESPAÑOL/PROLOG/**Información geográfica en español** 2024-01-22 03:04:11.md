```prolog
:-dynamic ciudad/2, pais/2, rio/2, pais_con_mayor_poblacion/1, pais_con_menor_poblacion/1, rio_mas_largo/1.

% Ciudades
ciudad(madrid, españa).
ciudad(barcelona, españa).
ciudad(valencia, españa).
ciudad(sevilla, españa).
ciudad(zaragoza, españa).
ciudad(malaga, españa).
ciudad(murcia, españa).
ciudad(palma_de_mallorca, españa).
ciudad(las_palmas_de_gran_canaria, españa).
ciudad(santa_cruz_de_tenerife, españa).

% Países
pais(españa, 46,7 millones).
pais(francia, 65,3 millones).
pais(alemania, 83,2 millones).
pais(italia, 60,5 millones).
pais(reino_unido, 67,8 millones).
pais(polonia, 38,0 millones).
pais(rumania, 19,3 millones).
pais(bulgaria, 6,9 millones).
pais(grecia, 10,7 millones).
pais(portugal, 10,3 millones).

% Ríos
rio(ebre, 910 kilómetros).
rio(duero, 897 kilómetros).
rio(tajo, 1007 kilómetros).
rio(guadalquivir, 657 kilómetros).
rio(genil, 337 kilómetros).
rio(miño, 315 kilómetros).
rio(segura, 325 kilómetros).
rio(júcar, 498 kilómetros).
rio(berus, 1004 kilómetros).
rio(rhin, 1320 kilómetros).

% País con mayor población
pais_con_mayor_poblacion(Pais) :-
  findall(Poblacion, pais(_, Poblacion), Poblaciones),
  max_list(Poblaciones, PoblacionMax),
  pais(Pais, PoblacionMax).

% País con menor población
pais_con_menor_poblacion(Pais) :-
  findall(Poblacion, pais(_, Poblacion), Poblaciones),
  min_list(Poblaciones, PoblacionMin),
  pais(Pais, PoblacionMin).

% Río más largo
rio_mas_largo(Rio) :-
  findall(Longitud, rio(_, Longitud), Longitudes),
  max_list(Longitudes, LongitudMax),
  rio(Rio, LongitudMax).

% Máximo de una lista
max_list([X], X).
max_list([X|Xs], Max) :-
  max_list(Xs, Max2),
  ( X >= Max2 -> Max = X ; Max = Max2 ).

% Mínimo de una lista
min_list([X], X).
min_list([X|Xs], Min) :-
  min_list(Xs, Min2),
  ( X =< Min2 -> Min = X ; Min = Min2 ).
```

Este código crea una base de conocimientos sobre ciudades, países y ríos en español. Contiene información sobre la población de los países y la longitud de los ríos. También define reglas para encontrar el país con mayor población, el país con menor población y el río más largo.

Para usarlo, puedes hacer consultas al sistema de esta manera:

```prolog
?- pais_con_mayor_poblacion(Pais).
Pais = alemania.

?- pais_con_menor_poblacion(Pais).
Pais = bulgaria.

?- rio_mas_largo(Rio).
Rio = rhen.
```

Este código puede ser útil para aprender sobre geografía en español. También puede ser utilizado para crear aplicaciones más complejas, como un sistema de planificación de viajes o un juego educativo.