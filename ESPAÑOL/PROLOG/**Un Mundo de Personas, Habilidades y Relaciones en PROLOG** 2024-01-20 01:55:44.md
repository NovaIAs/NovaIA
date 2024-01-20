```
:- dynamic persona/2, amigo/2, enemigo/2, sabe/2.

/* Uniones, que son características de las personas. */
:- dynamic inteligente/1, guapo/1, rico/1, poderoso/1.

/* Reglas que definen las uniones. */
inteligente(X) :- persona(X), sabe(X, "Matemáticas").
guapo(X) :- persona(X), sabe(X, "Arte").
rico(X) :- persona(X), sabe(X, "Economía").
poderoso(X) :- persona(X), sabe(X, "Política").

/* Reglas que definen las relaciones entre personas. */
amigo(X, Y) :- persona(X), persona(Y), sabe(X, Y), sabe(Y, X).
enemigo(X, Y) :- amigo(X, Z), amigo(Y, Z), X \= Y.

/* Hechos que definen personas, habilidades y relaciones. */
persona("Juan").
persona("María").
persona("Pedro").

sabe("Juan", "Matemáticas").
sabe("Juan", "Arte").
sabe("Juan", "Economía").
sabe("María", "Matemáticas").
sabe("María", "Arte").
sabe("María", "Psicología").
sabe("Pedro", "Matemáticas").
sabe("Pedro", "Economía").
sabe("Pedro", "Política").

amigo("Juan", "María").
amigo("María", "Pedro").
enemigo("Juan", "Pedro").

/* Búsquedas que se pueden realizar. */
/* ¿Quién es el más inteligente? */
mas_inteligente(Persona) :-
    findall(X, inteligente(X), Personas),
    sort(Personas, [Persona | _]).

/* ¿Quién es el más guapo? */
mas_guapo(Persona) :-
    findall(X, guapo(X), Personas),
    sort(Personas, [Persona | _]).

/* ¿Quién es el más rico? */
mas_rico(Persona) :-
    findall(X, rico(X), Personas),
    sort(Personas, [Persona | _]).

/* ¿Quién es el más poderoso? */
mas_poderoso(Persona) :-
    findall(X, poderoso(X), Personas),
    sort(Personas, [Persona | _]).

/* ¿Quiénes son los mejores amigos? */
mejores_amigos(X, Y) :-
    amigo(X, Y),
    amigo(Y, X),
    findall(Z, amigo(X, Z), Amigos),
    sort(Amigos, [X, Y | _]).

/* ¿Quiénes son los peores enemigos? */
peores_enemigos(X, Y) :-
    enemigo(X, Y),
    enemigo(Y, X),
    findall(Z, enemigo(Y, Z), EnemigosX),
    findall(Z, enemigo(X, Z), EnemigosY),
    sort(EnemigosX, _),
    sort(EnemigosY, _).

/* ¿Quién sabe más? */
sabe_mas(Persona) :-
    findall(X, sabe(Persona, X), Habilidades),
    length(Habilidades, NumHabilidades),
    findall(Y, persona(Y), Personas),
    forall(member(Y, Personas),
           findall(Z, sabe(Y, Z), HabilidadesY),
           length(HabilidadesY, NumHabilidadesY),
           NumHabilidades > NumHabilidadesY).

/* Explicación del código: */

Este código PROLOG define un conjunto de reglas y hechos para representar personas, habilidades y relaciones entre personas. Las uniones se definen utilizando reglas que relacionan las personas con las habilidades que conocen. Las relaciones entre personas se definen utilizando reglas que relacionan a las personas entre sí. Los hechos definen las personas, las habilidades que conocen y las relaciones entre ellas.

El código también define una serie de búsquedas que se pueden realizar sobre el conjunto de datos. Estas búsquedas incluyen encontrar a la persona más inteligente, la persona más guapa, la persona más rica, la persona más poderosa, los mejores amigos y los peores enemigos. La búsqueda sabe_mas encuentra a la persona que sabe más.

Este código es un ejemplo de cómo PROLOG se puede utilizar para representar y razonar sobre información compleja.