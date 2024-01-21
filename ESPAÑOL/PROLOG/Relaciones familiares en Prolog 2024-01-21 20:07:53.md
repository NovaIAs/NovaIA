```prolog
% Base de conocimiento

% Hechos
padre(juan, pedro).
padre(juan, maria).
madre(maria, pedro).
madre(maria, maria).
abuelo(X, Y) :- padre(X, Z), padre(Z, Y).
abuela(X, Y) :- madre(X, Z), madre(Z, Y).
hermano(X, Y) :- padre(Z, X), padre(Z, Y), X \= Y.
hermana(X, Y) :- madre(Z, X), madre(Z, Y), X \= Y.
tio(X, Y) :- hermano(X, Z), padre(Z, Y).
tia(X, Y) :- hermana(X, Z), madre(Z, Y).
primo(X, Y) :- tio(Z, X), padre(Z, Y).
prima(X, Y) :- tia(Z, X), madre(Z, Y).

% Consultas
?- abuelo(juan, pedro).
% Sí

?- abuela(maria, pedro).
% Sí

?- hermano(pedro, maria).
% Sí

?- hermana(maria, pedro).
% No

?- tio(juan, pedro).
% Sí

?- tia(maria, pedro).
% Sí

?- primo(pedro, maria).
% No

?- prima(maria, pedro).
% No
```

Explicación del código:

* La primera línea del código es un comentario. Los comentarios en Prolog empiezan con el carácter `%` y se extienden hasta el final de la línea. Se utilizan para explicar el código y hacerlo más legible.
* La segunda línea del código es una directiva de Prolog que le dice al compilador que debe cargar las siguientes líneas de código como una base de conocimiento.
* Las siguientes líneas del código son hechos. Un hecho es una declaración que es siempre cierta. En Prolog, los hechos se escriben como `nombre_del_hecho(argumentos)`. Por ejemplo, el hecho `padre(juan, pedro)` significa que Juan es el padre de Pedro.
* Las siguientes líneas del código son reglas. Una regla es una declaración que se puede probar cierta o falsa. En Prolog, las reglas se escriben como `nombre_de_la_regla(argumentos) :- cuerpo_de_la_regla`. Por ejemplo, la regla `abuelo(X, Y) :- padre(X, Z), padre(Z, Y)` significa que X es el abuelo de Y si X es el padre de Z y Z es el padre de Y.
* Las últimas líneas del código son consultas. Una consulta es una pregunta que se le hace al compilador. En Prolog, las consultas se escriben como `?- nombre_de_la_consulta`. Por ejemplo, la consulta `?- abuelo(juan, pedro)` pregunta si Juan es el abuelo de Pedro.