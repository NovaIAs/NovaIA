```
% Base de conocimiento

% Reglas para la relación "manchado"
manchado(perro(manchas)).
manchado(gato(miau)).
manchado(vaca(mu)).

% Reglas para la relación "peludo"
peludo(perro(manchas)).
peludo(gato(miau)).
peludo(conejo(pelusa)).

% Reglas para la relación "cuadrúpedo"
cuadrúpedo(perro(manchas)).
cuadrúpedo(gato(miau)).
cuadrúpedo(vaca(mu)).

% Reglas para la relación "domestico"
domestico(perro(manchas)).
domestico(gato(miau)).
domestico(conejo(pelusa)).

% Reglas para la relación "tipoAnimal"
tipoAnimal(perro(manchas), perro).
tipoAnimal(gato(miau), gato).
tipoAnimal(vaca(mu), vaca).
tipoAnimal(conejo(pelusa), conejo).

% Reglas para la relación "nombreAnimal"
nombreAnimal(perro(manchas), manchas).
nombreAnimal(gato(miau), miau).
nombreAnimal(vaca(mu), mu).
nombreAnimal(conejo(pelusa), pelusa).

% Reglas para la relación "característicaAnimal"
característicaAnimal(perro(manchas), manchas).
característicaAnimal(gato(miau), peludo).
característicaAnimal(vaca(mu), cuadrúpedo).
característicaAnimal(conejo(pelusa), domestico).

% Definiciones de consultas
:- consult('baseDeConocimiento.pl').

% Consulta 1: ¿Cuáles son los animales manchados y peludos?
consulta1(X) :-
    manchado(X),
    peludo(X).

% Consulta 2: ¿Cuáles son los animales cuadrúpedos y domesticados?
consulta2(X) :-
    cuadrúpedo(X),
    domestico(X).

% Consulta 3: ¿Cuál es el tipo de animal cuyo nombre es "manchas"?
consulta3(X) :-
    nombreAnimal(Y, manchas),
    tipoAnimal(Y, X).

% Consulta 4: ¿Cuáles son las características de los animales cuyo tipo es "conejo"?
consulta4(X) :-
    tipoAnimal(Y, conejo),
    característicaAnimal(Y, X).

```

Explicación del código:

* La base de conocimiento define las relaciones "manchado", "peludo", "cuadrúpedo", "domestico", "tipoAnimal", "nombreAnimal" y "característicaAnimal", que se utilizan para representar información sobre los animales.
* Las consultas definen las preguntas que se quieren hacer sobre la base de conocimiento.
* La consulta 1 pregunta cuáles son los animales manchados y peludos.
* La consulta 2 pregunta cuáles son los animales cuadrúpedos y domesticados.
* La consulta 3 pregunta cuál es el tipo de animal cuyo nombre es "manchas".
* La consulta 4 pregunta cuáles son las características de los animales cuyo tipo es "conejo".

El código utiliza varias técnicas de programación lógica, como la unificación y el backtracking, para resolver las consultas.