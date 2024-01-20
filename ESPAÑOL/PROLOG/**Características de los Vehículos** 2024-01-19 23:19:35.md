```prolog
% Base de conocimiento

% Definimos los tipos de vehículos
tipo_vehiculo(coche).
tipo_vehiculo(moto).
tipo_vehiculo(camion).
tipo_vehiculo(autobus).

% Definimos las características de los vehículos
caracteristica(coche, ruedas, 4).
caracteristica(coche, asientos, 5).
caracteristica(coche, combustible, gasolina).
caracteristica(moto, ruedas, 2).
caracteristica(moto, asientos, 1).
caracteristica(moto, combustible, gasolina).
caracteristica(camion, ruedas, 10).
caracteristica(camion, asientos, 3).
caracteristica(camion, combustible, diesel).
caracteristica(autobus, ruedas, 12).
caracteristica(autobus, asientos, 50).
caracteristica(autobus, combustible, diesel).

% Definimos las reglas de inferencia
regla1:-
    tipo_vehiculo(X),
    caracteristica(X, ruedas, R),
    R > 4,
    write(X), write(' tiene más de 4 ruedas').

regla2:-
    tipo_vehiculo(X),
    caracteristica(X, asientos, A),
    A > 2,
    write(X), write(' tiene más de 2 asientos').

regla3:-
    tipo_vehiculo(X),
    caracteristica(X, combustible, C),
    C = 'gasolina',
    write(X), write(' utiliza gasolina').

% Consulta

% Buscamos todos los vehículos que tengan más de 4 ruedas
?- findall(X, regla1, L1).

% Buscamos todos los vehículos que tengan más de 2 asientos
?- findall(X, regla2, L2).

% Buscamos todos los vehículos que utilicen gasolina
?- findall(X, regla3, L3).

% Mostramos los resultados
write(L1), nl.
write(L2), nl.
write(L3), nl.
```

Explicación del código:

* **Base de conocimiento:** Definimos los tipos de vehículos, las características de los vehículos y las reglas de inferencia.
* **Reglas de inferencia:** Definimos tres reglas de inferencia que nos permiten deducir información sobre los vehículos.
* **Consulta:** Utilizamos la cláusula `findall` para buscar todos los vehículos que satisfacen cada una de las reglas de inferencia.
* **Salida:** Mostramos los resultados de la consulta.