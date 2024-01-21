```
:- dynamic reino/2. % Hechos dinámicos para representar el reino

% Reglas para agregar nuevos reinos al reino

agregar_reino(Nombre, Continente) :-
    \+ reino(Nombre, _), % Comprobar si el reino ya existe
    assert(reino(Nombre, Continente)). % Agregar el nuevo reino

% Reglas para listar los reinos del reino

listar_reinos :-
    findall(Nombre, reino(Nombre, _), Nombres), % Obtener los nombres de los reinos
    sort(Nombres, NombresOrdenados), % Ordenar los nombres alfabéticamente
    foreach(Nombre in NombresOrdenados do
        writeln(Nombre)
    ).

listar_reinos_continente(Continente) :-
    findall(Nombre, reino(Nombre, Continente), Nombres),
    sort(Nombres, NombresOrdenados),
    foreach(Nombre in NombresOrdenados do
        writeln(Nombre)
    ).

% Reglas para eliminar reinos del reino

eliminar_reino(Nombre) :-
    reino(Nombre, _), % Comprobar si el reino existe
    retract(reino(Nombre, _)). % Eliminar el reino

% Reglas para obtener información sobre los reinos

reino_continente(Nombre, Continente) :-
    reino(Nombre, Continente).

% Regla que inicializa el reino con algunos datos iniciales

inicializar_reino :-
    agregar_reino("España", "Europa"),
    agregar_reino("Francia", "Europa"),
    agregar_reino("Alemania", "Europa"),
    agregar_reino("Reino Unido", "Europa"),
    agregar_reino("Italia", "Europa"),
    agregar_reino("Estados Unidos", "América"),
    agregar_reino("Canadá", "América"),
    agregar_reino("México", "América"),
    agregar_reino("Brasil", "América"),
    agregar_reino("Argentina", "América").

% Consulta para inicializar el reino
inicializar_reino.

% Consulta para listar los reinos del reino
listar_reinos.

% Consulta para listar los reinos de un continente específico
listar_reinos_continente("América").

% Consulta para eliminar un reino del reino
eliminar_reino("España").

% Consulta para obtener el continente de un reino específico
reino_continente("Francia", Continente).
```

Este código en PROLOG define un reino y sus características, como el nombre y el continente al que pertenece. Adicionalmente, proporciona una serie de reglas para agregar, listar, eliminar y obtener información sobre los reinos.

El código incluye una regla inicial que inicializa el reino con algunos datos iniciales, como los nombres y continentes de algunos países.

El código también incluye una serie de consultas que permiten interactuar con el reino, como listar los reinos, listar los reinos de un continente específico, eliminar un reino y obtener el continente de un reino específico.