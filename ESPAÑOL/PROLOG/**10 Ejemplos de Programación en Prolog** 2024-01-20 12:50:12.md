```prolog
:- op(100, fy, :-).

%Declaración de una regla
animal :- mamífero.
animal :- ave.
animal :- pez.

%Declaración de los hechos
mamífero :- perro.
mamífero :- gato.
mamífero :- vaca.

ave :- canario.
ave :- loro.
ave :- gaviota.

pez :- trucha.
pez :- salmón.
pez :- atún.

%Consulta de una regla
?- animal.
true

%Consulta de un hecho
?- mamífero(perro).
true

%Consulta de una regla con variables
?- animal(X).
X = perro
X = gato
X = vaca
X = canario
X = loro
X = gaviota
X = trucha
X = salmón
X = atún

%Consulta de un hecho con variables
?- mamífero(X, X).

%Definición de una regla recursiva
factorial(1, 1).
factorial(N, F) :-
    N > 1,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

%Consulta de una regla recursiva
?- factorial(5, F).
F = 120

%Definición de una regla que utiliza cortes
menor_que(X, Y) :-
    X < Y, !.
menor_que(X, Y) :-
    X >= Y,
    fail.

%Consulta de una regla que utiliza cortes
?- menor_que(3, 5).
true
```

Explicación del código:

1. Declaración de una regla: La primera línea define una regla llamada `animal`. Una regla es una declaración que define una relación entre dos términos. En este caso, la regla afirma que `animal` es un término que puede unificarse con uno de los tres términos `mamífero`, `ave` o `pez`.
2. Declaración de los hechos: Las siguientes tres líneas declaran hechos sobre los términos `mamífero`, `ave` y `pez`. Un hecho es una declaración que afirma que un término tiene una cierta propiedad. En este caso, los hechos declaran que `perro`, `gato` y `vaca` son mamíferos, que `canario`, `loro` y `gaviota` son aves, y que `trucha`, `salmón` y `atún` son peces.
3. Consulta de una regla: La siguiente línea consulta la regla `animal`. La consulta pregunta si existe un término que pueda unificarse con `animal`. En este caso, la regla se unifica con los tres términos `mamífero`, `ave` y `pez`, por lo que la consulta devuelve `true`.
4. Consulta de un hecho: La siguiente línea consulta el hecho `mamífero(perro)`. La consulta pregunta si el término `perro` tiene la propiedad `mamífero`. En este caso, el hecho se unifica con el término `perro`, por lo que la consulta devuelve `true`.
5. Consulta de una regla con variables: La siguiente línea consulta la regla `animal(X)`. La consulta pregunta si existe un término que pueda unificarse con `animal(X)`. En este caso, la regla se unifica con los siguientes nueve términos: `perro`, `gato`, `vaca`, `canario`, `loro`, `gaviota`, `trucha`, `salmón` y `atún`. La consulta devuelve los nueve términos como resultado.
6. Consulta de un hecho con variables: La siguiente línea consulta el hecho `mamífero(X, X)`. La consulta pregunta si existe un término que pueda unificarse con `mamífero(X, X)`. En este caso, el hecho no se unifica con ningún término, por lo que la consulta devuelve `false`.
7. Definición de una regla recursiva: Las siguientes líneas definen una regla recursiva llamada `factorial`. Una regla recursiva es una regla que se llama a sí misma. En este caso, la regla `factorial` se llama a sí misma para calcular el factorial de un número. El factorial de un número es el producto de todos los números enteros positivos menores o iguales que el número.
8. Consulta de una regla recursiva: La siguiente línea consulta la regla `factorial(5, F)`. La consulta pregunta cuál es el factorial de 5. La regla `factorial` se llama a sí misma para calcular el factorial de 5. La primera llamada a la regla calcula el factorial de 4. La segunda llamada a la regla calcula el factorial de 3. La tercera llamada a la regla calcula el factorial de 2. La cuarta llamada a la regla calcula el factorial de 1. La quinta llamada a la regla devuelve el factorial de 1, que es 1. La cuarta llamada a la regla multiplica el factorial de 1 por 2, que es 2. La tercera llamada a la regla multiplica el factorial de 2 por 3, que es 6. La segunda llamada a la regla multiplica el factorial de 3 por 4, que es 24. La primera llamada a la regla multiplica el factorial de 4 por 5, que es 120. La consulta devuelve 120 como resultado.
9. Definición de una regla que utiliza cortes: Las siguientes líneas definen una regla llamada `menor_que` que utiliza cortes. Un corte es un marcador que se utiliza para detener la búsqueda de soluciones alternativas. En este caso, el corte se utiliza para detener la búsqueda de soluciones alternativas si la primera condición de la regla se cumple.
10. Consulta de una regla que utiliza cortes: La siguiente línea consulta la regla `menor_que(3, 5)`. La consulta pregunta si 3 es menor que 5. La primera condición de la regla se cumple, por lo que el corte se activa y la búsqueda de soluciones alternativas se detiene. La consulta devuelve `true` como resultado.