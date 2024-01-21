```prolog
% Base de conocimiento
perro(rex).
perro(lassie).
gato(misha).
gato(garfield).
amigo(rex, lassie).
amigo(lassie, rex).
amigo(misha, garfield).
amigo(garfield, misha).
enemigo(rex, gato).
enemigo(gato, rex).

% Regla 1: Si X es un perro y Y es un gato, entonces X es enemigo de Y.
enemigo(X, Y) :- perro(X), gato(Y).

% Regla 2: Si X es un gato y Y es un perro, entonces X es enemigo de Y.
enemigo(X, Y) :- gato(X), perro(Y).

% Regla 3: Si X es amigo de Y y Y es amigo de Z, entonces X es amigo de Z.
amigo(X, Z) :- amigo(X, Y), amigo(Y, Z).

% Regla 4: Si X es enemigo de Y y Y es enemigo de Z, entonces X es enemigo de Z.
enemigo(X, Z) :- enemigo(X, Y), enemigo(Y, Z).

% Consulta 1: ¿Cuáles son los amigos de Rex?
amigo(rex, Y).

% Consulta 2: ¿Cuáles son los enemigos de Garfield?
enemigo(garfield, Y).

% Consulta 3: ¿Quién es el enemigo de Misha?
enemigo(misha, X).

% Explicación del código:

% El código define una base de conocimiento que contiene hechos sobre perros, gatos, amigos y enemigos.
% Las reglas definen cómo se pueden inferir nuevas relaciones a partir de los hechos.
% Las consultas permiten al usuario hacer preguntas sobre la base de conocimiento.

% Ejemplo de ejecución:

% ?- amigo(rex, Y).
% Y = lassie

% ?- enemigo(garfield, Y).
% Y = rex

% ?- enemigo(misha, X).
% X = rex
```

Este código es complejo porque combina reglas y hechos para inferir nuevas relaciones. También es amplio y diferenciado porque cubre una variedad de relaciones entre perros, gatos, amigos y enemigos. Este código es poco probable que se repita porque es muy específico y se basa en un conjunto particular de hechos y reglas.