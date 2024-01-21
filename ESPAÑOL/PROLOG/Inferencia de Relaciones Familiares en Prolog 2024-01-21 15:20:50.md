```prolog
/* Base de datos de relaciones familiares en Prolog */

/* Familiares directos */
padre(juan, pedro).
madre(ana, pedro).
padre(carlos, maria).
madre(luisa, maria).
padre(francisco, juan).
madre(isabel, juan).
padre(diego, ana).

/* Reglas para la inferencia de relaciones familiares */

/* Ancestros */
ancestro(X, Y) :-
    padre(X, Y).
ancestro(X, Y) :-
    madre(X, Y).
ancestro(X, Y) :-
    padre(X, Z),
    ancestro(Z, Y).
ancestro(X, Y) :-
    madre(X, Z),
    ancestro(Z, Y).

/* Descendientes */
descendiente(X, Y) :-
    padre(Y, X).
descendiente(X, Y) :-
    madre(Y, X).
descendiente(X, Y) :-
    padre(Z, X),
    descendiente(Z, Y).
descendiente(X, Y) :-
    madre(Z, X),
    descendiente(Z, Y).

/* Hermanos */
hermano(X, Y) :-
    padre(Z, X),
    padre(Z, Y),
    X \= Y.
hermana(X, Y) :-
    madre(Z, X),
    madre(Z, Y),
    X \= Y.

/* Tíos */
tio(X, Y) :-
    padre(Z, X),
    hermano(Z, W),
    padre(W, Y),
    X \= Y.

/* Sobrinos */
sobrino(X, Y) :-
    padre(Z, X),
    descendiente(Z, Y),
    X \= Y.


/* Consulta */

/* ¿Quién es el padre de Pedro? */
padre_de_pedro(X) :-
    padre(X, pedro).

/* ¿Quiénes son los hermanos de María? */
hermanos_de_maria(X) :-
    hermano(X, maria).

/* ¿Quiénes son los tíos de Juan? */
tios_de_juan(X) :-
    tio(X, juan).

/* ¿Quién es el sobrino de Carlos? */
sobrino_de_carlos(X) :-
    sobrino(X, carlos).
```

**Explicación del código:**

1. **Base de datos de relaciones familiares:** Esta sección del código define la base de datos de relaciones familiares. Contiene hechos que representan relaciones directas entre individuos, como padre/madre e hijo/hija.

2. **Reglas para la inferencia de relaciones familiares:** Esta sección del código define reglas para inferir relaciones familiares a partir de las relaciones directas. Por ejemplo, la regla `ancestro(X, Y)` infiere que X es un ancestro de Y si X es el padre o la madre de Y, o si X es un ancestro de un ancestro de Y.

3. **Consulta:** Esta sección del código muestra cómo consultar la base de datos de relaciones familiares para obtener información específica. Por ejemplo, la consulta `padre_de_pedro(X)` devuelve el nombre del padre de Pedro, y la consulta `tios_de_juan(X)` devuelve los nombres de los tíos de Juan.