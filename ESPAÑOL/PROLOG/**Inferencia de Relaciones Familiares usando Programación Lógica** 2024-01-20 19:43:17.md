```prolog
% Conocimiento general sobre el mundo real
hombre(juan).
mujer(maria).
padre_de(juan, pedro).
madre_de(maria, pedro).
abuelo_de(juan, nieto).
abuela_de(maria, nieto).
hermano_de(pedro, juan).
hermana_de(pedro, maria).

% Reglas para inferir nuevos hechos a partir de los conocimientos existentes
padre_de(X, Y) :- hombre(X), hijo_de(Y, X).
madre_de(X, Y) :- mujer(X), hija_de(Y, X).
abuelo_de(X, Y) :- padre_de(X, Z), padre_de(Z, Y).
abuela_de(X, Y) :- madre_de(X, Z), madre_de(Z, Y).
hermano_de(X, Y) :- hombre(X), padre_de(Z, X), padre_de(Z, Y), X \== Y.
hermana_de(X, Y) :- mujer(X), padre_de(Z, X), padre_de(Z, Y), X \== Y.

% Consulta al sistema para obtener información específica
?- padre_de(juan, pedro).
true

?- abuelo_de(juan, nieto).
true

?- hermano_de(pedro, maria).
true

?- hermana_de(pedro, juan).
false

Explicación del código:

1. **Hechos:**
   - hombre(juan): Juan es un hombre.
   - mujer(maria): María es una mujer.
   - padre_de(juan, pedro): Juan es el padre de Pedro.
   - madre_de(maria, pedro): María es la madre de Pedro.
   - abuelo_de(juan, nieto): Juan es el abuelo de su nieto.
   - abuela_de(maria, nieto): María es la abuela de su nieto.
   - hermano_de(pedro, juan): Pedro es el hermano de Juan.
   - hermana_de(pedro, maria): Pedro es la hermana de María.

2. **Reglas:**
   - padre_de(X, Y): X es el padre de Y si X es un hombre y Y es hijo de X.
   - madre_de(X, Y): X es la madre de Y si X es una mujer e Y es hija de X.
   - abuelo_de(X, Y): X es el abuelo de Y si X es el padre de Z y Z es el padre de Y.
   - abuela_de(X, Y): X es la abuela de Y si X es la madre de Z y Z es la madre de Y.
   - hermano_de(X, Y): X es el hermano de Y si X es un hombre, Z es el padre de X y Y, y X no es igual a Y.
   - hermana_de(X, Y): X es la hermana de Y si X es una mujer, Z es el padre de X y Y, y X no es igual a Y.

3. **Consulta:**
   - ?- padre_de(juan, pedro).: ¿Juan es el padre de Pedro?
     - Respuesta: true (verdadero)
   - ?- abuelo_de(juan, nieto).: ¿Juan es el abuelo de su nieto?
     - Respuesta: true (verdadero)
   - ?- hermano_de(pedro, maria).: ¿Pedro es el hermano de María?
     - Respuesta: true (verdadero)
   - ?- hermana_de(pedro, juan).: ¿Pedro es la hermana de Juan?
     - Respuesta: false (falso)