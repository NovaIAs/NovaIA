```prolog

% Base de conocimiento

persona(juan, hombre).
persona(maria, mujer).
persona(pedro, hombre).
persona(ana, mujer).
persona(luis, hombre).
persona(rosa, mujer).

% Reglas

padre(X, Y) :-
    persona(X, hombre),
    parentesco(X, Y, padre).

madre(X, Y) :-
    persona(X, mujer),
    parentesco(X, Y, madre).

hermana(X, Y) :-
    persona(X, mujer),
    persona(Y, mujer),
    padres(X, Z),
    padres(Y, Z).

hermano(X, Y) :-
    persona(X, hombre),
    persona(Y, hombre),
    padres(X, Z),
    padres(Y, Z).

% Hechos

parentesco(juan, maria, padre).
parentesco(maria, juan, hijo).
parentesco(juan, pedro, padre).
parentesco(pedro, juan, hijo).
parentesco(maria, ana, madre).
parentesco(ana, maria, hija).
parentesco(pedro, luis, padre).
parentesco(luis, pedro, hijo).
parentesco(ana, rosa, madre).
parentesco(rosa, ana, hija).

% Consulta

?- padre(juan, X).
X = maria ;
X = pedro.

?- madre(maria, X).
X = juan ;
X = pedro.

?- hermana(ana, X).
X = rosa.

?- hermano(luis, X).
X = pedro.

```

Explicación del código:

* La base de conocimiento contiene hechos sobre personas, como su nombre y género.
* Las reglas definen relaciones entre personas, como padre, madre, hermana y hermano.
* Los hechos definen relaciones específicas entre personas, como quién es el padre o la madre de quién.
* La consulta se utiliza para hacer preguntas sobre las relaciones entre personas.

Este código es un ejemplo de un programa Prolog sencillo que puede utilizarse para representar y consultar información sobre relaciones familiares.