```prolog
/*
** Programa:  Programación lógica en Prolog
** Autor:    [Tu Nombre]
** Fecha:     [Fecha]
** Descripción: Este programa demuestra las capacidades de la programación lógica
**             en Prolog a través de una serie de ejemplos.
*/

% Aserción de hechos

% Definición de una relación padre/hijo
padre(juan, pedro).
padre(juan, maria).
padre(pedro, luis).
padre(pedro, ana).
padre(maria, carlos).
padre(maria, julia).

% Definición de una relación abuelo/nieto
abuelo(X, Y) :- padre(X, Z), padre(Z, Y).

% Definición de una relación hermano/hermana
hermano(X, Y) :- padre(Z, X), padre(Z, Y), X \= Y.

% Definición de una relación primo/prima
primo(X, Y) :- padre(Z, X), padre(W, Y), hermano(Z, W), X \= Y.

% Aserción de reglas

% Regla para calcular la edad de una persona
edad(Persona, Edad) :- edad(Persona, Edad, now).

% Regla para calcular la edad de una persona en una fecha determinada
edad(Persona, Edad, Fecha) :-
    fecha_nacimiento(Persona, FechaNacimiento),
    FechaNac2 is FechaNacimiento + year,
    Fecha3 is Fecha - FechaNac2,
    Edad is Fecha3 div 365.25.

% Base de conocimiento

% Aserción de hechos sobre las fechas de nacimiento de las personas

fecha_nacimiento(juan, "1960-01-01").
fecha_nacimiento(pedro, "1980-03-08").
fecha_nacimiento(maria, "1982-07-15").
fecha_nacimiento(luis, "2000-11-23").
fecha_nacimiento(ana, "2002-05-19").
fecha_nacimiento(carlos, "2004-09-12").
fecha_nacimiento(julia, "2006-12-25").

% Consultas

% Consulta para obtener la edad de una persona en la fecha actual
?- edad(pedro, Edad).

% Consulta para obtener la edad de una persona en una fecha determinada
?- edad(maria, Edad, "2023-07-15").

% Consulta para obtener el abuelo de una persona
?- abuelo(juan, Nieto).

% Consulta para obtener los hermanos de una persona
?- hermano(pedro, Hermano).

% Consulta para obtener los primos de una persona
?- primo(luis, Primo).
```

Explicación del código:

* **Aserción de hechos:** Se utilizan hechos para almacenar información sobre el mundo real. En este ejemplo, los hechos representan las relaciones entre personas (padre/hijo, abuelo/nieto, hermano/hermana y primo/prima) y las fechas de nacimiento de las personas.
* **Aserción de reglas:** Se utilizan reglas para definir nuevos hechos a partir de los hechos existentes. En este ejemplo, las reglas se utilizan para calcular la edad de una persona en una fecha determinada y para obtener información sobre las relaciones familiares.
* **Base de conocimiento:** La base de conocimiento es el conjunto de hechos y reglas que se utilizan para resolver las consultas. En este ejemplo, la base de conocimiento incluye información sobre las relaciones familiares y las fechas de nacimiento de las personas.
* **Consultas:** Las consultas se utilizan para obtener información de la base de conocimiento. En este ejemplo, las consultas se utilizan para obtener la edad de una persona, el abuelo de una persona, los hermanos de una persona y los primos de una persona.

Este código es un ejemplo de cómo se puede utilizar la programación lógica en Prolog para resolver problemas complejos.