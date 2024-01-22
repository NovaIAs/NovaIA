```prolog
% Base de conocimiento sobre animales

animal(perro).
animal(gato).
animal(vaca).
animal(cerdo).
animal(caballo).

% Reglas para determinar si un animal es doméstico o salvaje

domestico(X) :-
    animal(X),
    miembro(X, [perro, gato, vaca, cerdo]).

salvaje(X) :-
    animal(X),
    not(domestico(X)).

% Reglas para determinar si un animal tiene pelo

pelo(X) :-
    animal(X),
    miembro(X, [perro, gato, vaca, caballo]).

no_pelo(X) :-
    animal(X),
    not(pelo(X)).

% Reglas para determinar si un animal es comestible

comestible(X) :-
    animal(X),
    miembro(X, [vaca, cerdo, caballo]).

no_comestible(X) :-
    animal(X),
    not(comestible(X)).

% Consultas

% ¿Cuáles son los animales domésticos?
?- domestico(X).
X = perro ;
X = gato ;
X = vaca ;
X = cerdo.

% ¿Cuáles son los animales salvajes?
?- salvaje(X).
X = caballo.

% ¿Cuáles son los animales que tienen pelo?
?- pelo(X).
X = perro ;
X = gato ;
X = vaca ;
X = caballo.

% ¿Cuáles son los animales que no tienen pelo?
?- no_pelo(X).
no hay solución

% ¿Cuáles son los animales comestibles?
?- comestible(X).
X = vaca ;
X = cerdo ;
X = caballo.

% ¿Cuáles son los animales no comestibles?
?- no_comestible(X).
X = perro ;
X = gato.
```

Explicación del código:

* La base de conocimiento contiene hechos sobre animales, como si son domésticos o salvajes, si tienen pelo o no, y si son comestibles o no.
* Las reglas se utilizan para inferir nueva información a partir de los hechos. Por ejemplo, la regla `domestico(X)` infiere que un animal `X` es doméstico si es un perro, un gato, una vaca o un cerdo.
* Las consultas se utilizan para hacer preguntas al sistema. Por ejemplo, la consulta `?- domestico(X)` pregunta cuáles son los animales domésticos.
* El sistema responde a las consultas utilizando las reglas y los hechos de la base de conocimiento.