```prolog
:- op(950, fy, !).

% Hechos
perro(fido).
gato(minino).
ave(canario).
humano(juan).
humano(maria).

% Regla deducción
ser_vivo(X) :- perro(X) | gato(X) | ave(X) | humano(X).

% Regla de inferencia
% no(P) si no se puede probar P
no(P) :- \+ P.

% Regla de recursión
% ancestro(X, Y) si X es padre o madre de Y o si X es ancestro de un ancestro de Y
ancestro(X, Y) :- padre(X, Y) | madre(X, Y) | ancestro(X, Z), ancestro(Z, Y).

% Regla de herencia
% heredero(X, Y) si X es hijo o hija de Y y no tiene hermanos o hermanas
heredero(X, Y) :- hijo(X, Y), no(hermano(X, Z)), no(hermana(X, Z)).

% Regla de composición
% familia(X, Y) si X es padre o madre de Y o si X es cónyuge de Y
familia(X, Y) :- padre(X, Y) | madre(X, Y) | conyuge(X, Y).

% Regla de descomposición
% cónyuges(X, Y) si X es marido de Y y Y es mujer de X
conyuges(X, Y) :- marido(X, Y), mujer(Y, X).

% Regla de enumeración de casos
% edad(X, Edad) si X es un humano y tiene Edad años
edad(X, Edad) :- humano(X), Edad >= 0.

% Regla de agregación
% lista_nombres(Nombres) si Nombres es una lista de nombres de los objetos que cumplen la condición ser_vivo
lista_nombres(Nombres) :-
    findall(X, ser_vivo(X), Nombres).

% Regla de existencia
% existe(X) si existe un objeto X que cumple la condición ser_vivo
existe(X) :- ser_vivo(X).

% Regla de universalidad
% todos(X, P(X)) si P(X) se cumple para todos los objetos X que cumplen la condición ser_vivo
todos(X, P(X)) :-
    ser_vivo(X),
    P(X).

% Regla de implicación
% si P entonces Q si P implica Q
si(P, Q) :- P -> Q.

% Regla de equivalencia
% P si y sólo si Q si P es equivalente a Q
si_y_sólo_si(P, Q) :- P <=> Q.

% Regla de negación por fallo
% no_es_humano(X) si X no es un humano
no_es_humano(X) :- \+ humano(X).

% Regla de clausura del mundo
% lista_seres_vivos(SeresVivos) si SeresVivos es una lista de todos los objetos que cumplen la condición ser_vivo
lista_seres_vivos(SeresVivos) :-
    findall(X, ser_vivo(X), SeresVivos).

% Regla de minimización de costos
% mejor_ruta(Origen, Destino, Ruta, Costo) si Ruta es la ruta con menor costo desde Origen hasta Destino
mejor_ruta(Origen, Destino, Ruta, Costo) :-
    findall([Ruta, Costo], camino(Origen, Destino, Ruta, Costo), Respuestas),
    minimo(Respuestas, Ruta, Costo).

% Regla de maximización de beneficios
% mejor_inversión(Inversión, Beneficio) si Inversión es la inversión con mayor beneficio
mejor_inversión(Inversión, Beneficio) :-
    findall([Inversión, Beneficio], inversion(Inversión, Beneficio), Respuestas),
    maximo(Respuestas, Inversión, Beneficio).

% Regla de heurística
% mejor_movimiento(EstadoInicial, EstadoFinal, Movimiento) si Movimiento es el mejor movimiento para ir de EstadoInicial a EstadoFinal
mejor_movimiento(EstadoInicial, EstadoFinal, Movimiento) :-
    findall([Movimiento, Costo], movimiento(EstadoInicial, EstadoFinal, Movimiento, Cost