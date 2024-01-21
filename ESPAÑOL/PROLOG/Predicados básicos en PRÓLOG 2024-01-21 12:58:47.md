```prolog
% Definimos un predicado "saludar/1" que toma un argumento "Persona" y devuelve
% un saludo personalizado.
saludar(Persona) :-
    % Comprobamos si la persona es "Juan".
    (Persona = "Juan") ->
        % Si es Juan, decimos "Hola, Juan!"
        write('Hola, Juan!'), nl;

    % Comprobamos si la persona es "María".
    (Persona = "María") ->
        % Si es María, decimos "Hola, María!"
        write('Hola, María!'), nl;

    % Si no es ni Juan ni María, decimos "Hola, [Nombre de la persona]!"
    write('Hola, '), write(Persona), write('!'), nl.

% Definimos un predicado "calcular_mcd/3" que toma tres argumentos: "A", "B" y
% "MCD". Este predicado calcula el máximo común divisor de "A" y "B" y lo
% devuelve en "MCD".
calcular_mcd(A, B, MCD) :-
    % Si B es 0, entonces el MCD es A.
    (B = 0) ->
        MCD is A;

    % Si B no es 0, entonces calculamos el resto de la división de A entre B y
    % llamamos recursivamente al predicado "calcular_mcd/3" con los argumentos B y
    % el resto de la división.
    calcular_mcd(B, A mod B, MCD).

% Definimos un predicado "ordenar_lista/2" que toma dos argumentos: "Lista" y
% "ListaOrdenada". Este predicado ordena la lista "Lista" y devuelve la lista
% ordenada en "ListaOrdenada".
ordenar_lista([], []).

ordenar_lista([Cabeza | Resto], ListaOrdenada) :-
    % Insertamos la cabeza de la lista en la lista ordenada en la posición
    % correcta.
    insertar(Cabeza, ListaOrdenada, NuevaListaOrdenada),

    % Llamamos recursivamente al predicado "ordenar_lista/2" con el argumento
    % "Resto" para ordenar el resto de la lista.
    ordenar_lista(Resto, NuevaListaOrdenada).

% Definimos un predicado "insertar/3" que toma tres argumentos: "Elemento",
% "Lista" y "NuevaLista". Este predicado inserta el elemento "Elemento" en la
% lista "Lista" en la posición correcta y devuelve la nueva lista en
% "NuevaLista".
insertar(Elemento, [], [Elemento]).

insertar(Elemento, [Cabeza | Resto], [Cabeza | NuevaLista]) :-
    % Si el elemento a insertar es menor que la cabeza de la lista, lo
    % insertamos antes de la cabeza.
    (Elemento < Cabeza) ->
        NuevaLista = [Elemento | [Cabeza | Resto]];

    % Si el elemento a insertar es mayor que la cabeza de la lista, lo
    % insertamos después de la cabeza.
    (Elemento >= Cabeza) ->
        insertar(Elemento, Resto, NuevaLista).

% Definimos un predicado "factorial/2" que toma dos argumentos: "Numero" y
% "Factorial". Este predicado calcula el factorial de "Numero" y lo devuelve en
% "Factorial".
factorial(0, 1).

factorial(Numero, Factorial) :-
    % Calculamos el factorial de Numero-1.
    factorial(Numero-1, FactorialAnterior),

    % Multiplicamos el factorial de Numero-1 por Numero para obtener el
    % factorial de Numero.
    Factorial is Numero * FactorialAnterior.
```

Este código incluye los siguientes predicados:

* "saludar/1": Este predicado saluda a una persona por su nombre.
* "calcular_mcd/3": Este predicado calcula el máximo común divisor de dos números.
* "ordenar_lista/2": Este predicado ordena una lista de números en orden ascendente.
* "insertar/3": Este predicado inserta un elemento en una lista en la posición correcta.
* "factorial/2": Este predicado calcula el factorial de un número.

Cada uno de estos predicados es útil por sí mismo y se puede utilizar en una variedad de programas. Por ejemplo, el predicado "saludar/1" se puede utilizar para crear un programa que salude a los usuarios por su nombre. El predicado "calcular_mcd/3" se puede utilizar para crear un programa que calcule el máximo común divisor de dos números. El predicado "ordenar_lista/2" se puede utilizar para crear un programa que ordene una lista de números. El predicado "insertar/3" se puede utilizar para crear un programa que inserte un elemento en una lista. El predicado "factorial/2" se puede utilizar para crear un programa que calcule el factorial de un número.

Este código es un ejemplo de cómo se puede utilizar PROLOG para crear programas complejos y útiles.