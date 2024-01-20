```
% Definición de los hechos iniciales.
perro(fido).
gato(minino).
pájaro(piopío).

% Definición de la relación "amigo".
amigo(X, Y) :- perro(X), gato(Y).
amigo(X, Y) :- gato(X), pájaro(Y).

% Definición de la relación "enemigo".
enemigo(X, Y) :- perro(X), pájaro(Y).
enemigo(X, Y) :- gato(X), perro(Y).

% Definición de la relación "es amigo de".
es_amigo_de(X, Y) :- amigo(X, Y).
es_amigo_de(X, Y) :- amigo(Y, X).

% Definición de la relación "es enemigo de".
es_enemigo_de(X, Y) :- enemigo(X, Y).
es_enemigo_de(X, Y) :- enemigo(Y, X).

% Definición de la relación "tiene como mascota".
tiene_como_mascota(X, Y) :- perro(Y), amigo(X, Y).
tiene_como_mascota(X, Y) :- gato(Y), amigo(X, Y).
tiene_como_mascota(X, Y) :- pájaro(Y), amigo(X, Y).

% Definición de la relación "es mascota de".
es_mascota_de(X, Y) :- tiene_como_mascota(Y, X).

% Definición de la relación "es amigo de la mascota de".
es_amigo_de_la_mascota_de(X, Y) :- tiene_como_mascota(Z, X), amigo(Y, Z).

% Definición de la relación "es enemigo de la mascota de".
es_enemigo_de_la_mascota_de(X, Y) :- tiene_como_mascota(Z, X), enemigo(Y, Z).

```

Este código en PROLOG define una serie de relaciones entre animales y personas. Las relaciones definidas son:

* amigo(X, Y): X es amigo de Y.
* enemigo(X, Y): X es enemigo de Y.
* es_amigo_de(X, Y): X es amigo de Y.
* es_enemigo_de(X, Y): X es enemigo de Y.
* tiene_como_mascota(X, Y): X tiene como mascota a Y.
* es_mascota_de(X, Y): X es mascota de Y.
* es_amigo_de_la_mascota_de(X, Y): X es amigo de la mascota de Y.
* es_enemigo_de_la_mascota_de(X, Y): X es enemigo de la mascota de Y.

El código también define algunos hechos iniciales, que son declaraciones que se asumen como verdaderas. Los hechos iniciales definidos son:

* perro(fido): Fido es un perro.
* gato(minino): Minino es un gato.
* pájaro(piopío): Piopío es un pájaro.

El código se puede utilizar para responder a preguntas sobre las relaciones entre animales y personas. Por ejemplo, se podría utilizar para responder a preguntas como:

* ¿Quién es amigo de Fido?
* ¿Quién es enemigo de Minino?
* ¿Quién tiene como mascota a Piopío?
* ¿Es Fido amigo de la mascota de Minino?
* ¿Es Piopío enemigo de la mascota de Fido?

El código también se puede utilizar para generar nuevas relaciones a partir de las relaciones existentes. Por ejemplo, se podría utilizar para generar la relación "es amigo del amigo de" a partir de la relación "es amigo de". La relación "es amigo del amigo de" se define como sigue:

```
amigo_del_amigo_de(X, Y) :- amigo(X, Z), amigo(Z, Y).
```

Esta relación se puede utilizar para responder a preguntas como:

* ¿Quién es amigo del amigo de Fido?
* ¿Quién es amigo del amigo del amigo de Minino?

El código es un ejemplo de cómo se puede utilizar PROLOG para representar y razonar sobre el conocimiento. PROLOG es un lenguaje de programación lógico que se utiliza a menudo para desarrollar sistemas de inteligencia artificial.