```prolog
:- dynamic padre/2, madre/2, hijo/2, hija/2, hermano/2, hermana/2, abuelo/2, abuela/2, nieto/2, nieta/2, suegro/2, suegra/2, yerno/2, nuera/2, cuñado/2, cuñada/2, primo/2, prima/2, sobrino/2, sobrina/2, tío/2, tía/2.

% Relaciones inmediatas

padre(X, Y) :- hijo(Y, X).
madre(X, Y) :- hija(Y, X).
hijo(X, Y) :- padre(Y, X).
hija(X, Y) :- madre(Y, X).

% Relaciones con terceras personas

hermano(X, Y) :- padre(P, X), padre(P, Y), X \== Y.
hermana(X, Y) :- padre(P, X), padre(P, Y), X \== Y.

abuelo(X, Y) :- padre(X, Z), padre(Z, Y).
abuela(X, Y) :- madre(X, Z), madre(Z, Y).

nieto(X, Y) :- hijo(X, Z), padre(Y, Z).
nieta(X, Y) :- hija(X, Z), madre(Y, Z).

% Relaciones matrimoniales

suegro(X, Y) :- padre(X, Z), marido(Y, Z).
suegra(X, Y) :- padre(X, Z), mujer(Y, Z).

yerno(X, Y) :- marido(X, Z), madre(Y, Z).
nuera(X, Y) :- mujer(X, Z), padre(Y, Z).

cuñado(X, Y) :- hermano(X, Z), marido(Y, Z).
cuñada(X, Y) :- hermano(X, Z), mujer(Y, Z).

% Relaciones de parentesco

primo(X, Y) :- tio(X, Z), hijo(Y, Z).
prima(X, Y) :- tia(X, Z), hija(Y, Z).

sobrino(X, Y) :- hijo(X, Z), hermano(Y, Z).
sobrina(X, Y) :- hija(X, Z), hermano(Y, Z).

tío(X, Y) :- hermano(X, Z), padre(Z, Y).
tía(X, Y) :- hermana(X, Z), madre(Z, Y).
```

Explicación:

Este código en PROLOG define una serie de relaciones familiares en español, incluyendo relaciones inmediatas (padre, madre, hijo, hija), relaciones con terceras personas (hermano, hermana, abuelo, abuela, nieto, nieta), relaciones matrimoniales (suegro, suegra, yerno, nuera, cuñado, cuñada) y relaciones de parentesco (primo, prima, sobrino, sobrina, tío, tía).

El código utiliza la cláusula `dynamic` para indicar que las relaciones pueden ser modificadas durante la ejecución del programa.

Para definir las relaciones, se utilizan cláusulas del tipo `relación(X, Y) :- condición(X, Y).`. Por ejemplo, la cláusula `padre(X, Y) :- hijo(Y, X).` define la relación padre-hijo como la inversa de la relación hijo-padre.

También se utilizan cláusulas del tipo `relación(X, Y) :- condición1(X, Y), condición2(X, Y), ...`. Por ejemplo, la cláusula `abuelo(X, Y) :- padre(X, Z), padre(Z, Y).` define la relación abuelo-nieto como la composición de las relaciones padre-hijo y padre-hijo.

El código también incluye cláusulas para definir las relaciones matrimoniales y las relaciones de parentesco. Por ejemplo, la cláusula `primo(X, Y) :- tio(X, Z), hijo(Y, Z).` define la relación primo-primo como la composición de las relaciones tío-sobrino y padre-hijo.

Este código puede ser utilizado para crear una base de conocimientos sobre las relaciones familiares y para realizar inferencias sobre dichas relaciones. Por ejemplo, si se sabe que Juan es el padre de María y que María es la madre de Pedro, entonces se puede inferir que Juan es el abuelo de Pedro.