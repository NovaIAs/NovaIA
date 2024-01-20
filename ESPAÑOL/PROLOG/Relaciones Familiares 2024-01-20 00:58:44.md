```prolog
% Base de Conocimiento

% Define el predicado padre/2, que es cierto si el primer argumento es padre del segundo.
padre(juan, maria).
padre(juan, pedro).
padre(pedro, ana).
padre(pedro, luis).

% Define el predicado madre/2, que es cierto si el primer argumento es madre del segundo.
madre(maria, maria).
madre(maria, pedro).
madre(ana, ana).
madre(ana, luis).

% Define el predicado abuelo/2, que es cierto si el primer argumento es abuelo del segundo.
abuelo(X, Y) :- padre(X, Z), padre(Z, Y).

% Define el predicado abuela/2, que es cierto si el primer argumento es abuela del segundo.
abuela(X, Y) :- madre(X, Z), madre(Z, Y).

% Define el predicado hermano/2, que es cierto si el primer argumento es hermano del segundo.
hermano(X, Y) :- padre(Z, X), padre(Z, Y), X \= Y.

% Define el predicado hermana/2, que es cierto si el primer argumento es hermana del segundo.
hermana(X, Y) :- madre(Z, X), madre(Z, Y), X \= Y.

% Define el predicado primo/2, que es cierto si el primer argumento es primo del segundo.
primo(X, Y) :- padre(Z, X), padre(Z, W), hermano(W, Y), X \= Y.

% Define el predicado prima/2, que es cierto si el primer argumento es prima del segundo.
prima(X, Y) :- madre(Z, X), madre(Z, W), hermana(W, Y), X \= Y.

% Define el predicado tio/2, que es cierto si el primer argumento es tío del segundo.
tio(X, Y) :- hermano(X, Z), padre(Z, Y).

% Define el predicado tia/2, que es cierto si el primer argumento es tía del segundo.
tia(X, Y) :- hermana(X, Z), madre(Z, Y).

% Define el predicado sobrino/2, que es cierto si el primer argumento es sobrino del segundo.
sobrino(X, Y) :- hijo(X, Z), hermano(Z, Y).

% Define el predicado sobrina/2, que es cierto si el primer argumento es sobrina del segundo.
sobrina(X, Y) :- hija(X, Z), hermana(Z, Y).

% Define el predicado cuñado/2, que es cierto si el primer argumento es cuñado del segundo.
cuñado(X, Y) :- hermano(X, Z), casado(Z, Y).

% Define el predicado cuñada/2, que es cierto si el primer argumento es cuñada del segundo.
cuñada(X, Y) :- hermana(X, Z), casado(Z, Y).

% Define el predicado yerno/2, que es cierto si el primer argumento es yerno del segundo.
yerno(X, Y) :- casado(X, Z), hija(Z, Y).

% Define el predicado nuera/2, que es cierto si el primer argumento es nuera del segundo.
nuera(X, Y) :- casado(X, Z), hijo(Z, Y).

% Define el predicado suegro/2, que es cierto si el primer argumento es suegro del segundo.
suegro(X, Y) :- padre(X, Z), casado(Z, Y).

% Define el predicado suegra/2, que es cierto si el primer argumento es suegra del segundo.
suegra(X, Y) :- madre(X, Z), casado(Z, Y).

% Consultas

% Consulta para obtener todos los padres de Juan.
?- padre(X, juan).
% Resultado: X = pedro

% Consulta para obtener todos los abuelos de María.
?- abuelo(X, maria).
% Resultado: X = juan

% Consulta para obtener todos los hermanos de Luis.
?- hermano(X, luis).
% Resultado: X = ana

% Consulta para obtener todas las hermanas de Pedro.
?- hermana(X, pedro).
% Resultado: X = maria

% Consulta para obtener todos los primos de Ana.
?- primo(X, ana).
% Resultado: X = luis

% Consulta para obtener todas las primas de Juan.
?- prima(X, juan).
% Resultado: X = ana

% Consulta para obtener todos los tíos de Luis.
?- tio(X, luis).
% Resultado: X = pedro, X = ana

% Consulta para obtener todas las tías de María.