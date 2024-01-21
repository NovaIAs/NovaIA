```prolog
:- dynamic hechos/2.

% Definición de la regla "perro".
perro(perro1).

% Definición de la regla "gato".
gato(gato1).

% Definición de la regla "pajaro".
pajaro(pajaro1).

% Definición de la regla "animal".
animal(X) :- perro(X).
animal(X) :- gato(X).
animal(X) :- pajaro(X).

% Definición de la regla "mamifero".
mamifero(X) :- perro(X).

% Definición de la regla "ave".
ave(X) :- pajaro(X).

% Definición de la regla "tiene_patas".
tiene_patas(X) :- perro(X).
tiene_patas(X) :- gato(X).

% Definición de la regla "tiene_alas".
tiene_alas(X) :- pajaro(X).

% Definición de la regla "hace_guau".
hace_guau(X) :- perro(X).

% Definición de la regla "hace_miau".
hace_miau(X) :- gato(X).

% Definición de la regla "hace_pio".
hace_pio(X) :- pajaro(X).

% Definición de la regla "mostrar_animal".
mostrar_animal(X) :-
    write('El animal es: '),
    write(X),
    nl.

% Definición de la regla "mostrar_caracteristicas".
mostrar_caracteristicas(X) :-
    write('Características del animal:'),
    nl,
    (animal(X) -> write('Es un animal.'), nl; true),
    (mamifero(X) -> write('Es un mamífero.'), nl; true),
    (ave(X) -> write('Es un ave.'), nl; true),
    (tiene_patas(X) -> write('Tiene patas.'), nl; true),
    (tiene_alas(X) -> write('Tiene alas.'), nl; true),
    (hace_guau(X) -> write('Hace guau.'), nl; true),
    (hace_miau(X) -> write('Hace miau.'), nl; true),
    (hace_pio(X) -> write('Hace pio.'), nl; true).

% Definición de la regla "iniciar".
iniciar :-
    assert(hechos(perro1, 'Firulais')),
    assert(hechos(gato1, 'Michi')),
    assert(hechos(pajaro1, 'Piolín')),
    write('Base de hechos cargada.'),
    nl,
    write('¿Qué animal quieres consultar? '),
    read(Animal),
    (hechos(Animal, Nombre) ->
        mostrar_animal(Nombre),
        mostrar_caracteristicas(Nombre)
    ;
        write('El animal no existe en la base de hechos.')
    ).

% Punto de entrada al programa.
iniciar.
```

**Explicación del código:**

* Las primeras líneas definen reglas que asignan propiedades a diferentes animales. Por ejemplo, la regla "perro" establece que "perro1" es un perro.
* La siguiente sección define reglas que combinan propiedades para crear categorías más generales. Por ejemplo, la regla "mamífero" establece que cualquier cosa que sea un perro es también un mamífero.
* La siguiente sección define reglas que describen los comportamientos de los animales. Por ejemplo, la regla "hace_guau" establece que cualquier cosa que sea un perro hace guau.
* La siguiente sección define reglas que muestran información sobre los animales. Por ejemplo, la regla "mostrar_animal" muestra el nombre de un animal.
* La siguiente sección define reglas que combinan las reglas anteriores para crear consultas más complejas. Por ejemplo, la regla "iniciar" carga una base de hechos con información sobre animales y luego permite al usuario consultar la base de hechos.
* El punto de entrada al programa es la regla "iniciar".

Este código es un ejemplo de un sistema experto en Prolog. Un sistema experto es un programa de computadora que puede resolver problemas en un dominio específico utilizando conocimiento e inferencia. En este caso, el sistema experto puede responder preguntas sobre los animales y sus características.