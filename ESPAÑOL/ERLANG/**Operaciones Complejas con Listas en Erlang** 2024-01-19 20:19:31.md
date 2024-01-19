```erlang
-module(ejemplo_complejo).
-export([main/0]).

% Función principal
main() ->
    % Definición de una lista con números del 1 al 10
    Numeros = lists:seq(1, 10),

    % Impresión de la lista original
    io:fwrite("Lista original: ~p~n", [Numeros]),

    % Filtrar la lista para obtener sólo los números pares
    Pares = [N || N <- Numeros, N rem 2 =:= 0],

    % Impresión de la lista de números pares
    io:fwrite("Lista de números pares: ~p~n", [Pares]),

    % Sumar todos los números de la lista original
    Suma = lists:sum(Numeros),

    % Impresión del resultado de la suma
    io:fwrite("Suma de los números: ~p~n", [Suma]),

    % Ordenar la lista original de forma descendente
    OrdenadosDesc = lists:reverse(lists:sort(Numeros)),

    % Impresión de la lista ordenada de forma descendente
    io:fwrite("Lista ordenada de forma descendente: ~p~n", [OrdenadosDesc]).
```

Explicación del código:

1. Se define un módulo llamado `ejemplo_complejo` y una función de exportación llamada `main/0`.

2. La función `main/0` es la función principal del programa.

3. Dentro de la función `main/0`, se definen variables y se realizan operaciones sobre ellas.

4. Se define una lista llamada `Numeros` con los valores del 1 al 10.

5. Se imprime la lista original utilizando la función `io:fwrite/2`.

6. Se filtra la lista `Numeros` para obtener sólo los números pares utilizando una comprensión de listas (`[N || N <- Numeros, N rem 2 =:= 0]`).

7. Se imprime la lista de números pares utilizando la función `io:fwrite/2`.

8. Se suma todos los números de la lista `Numeros` utilizando la función `lists:sum/1`.

9. Se imprime el resultado de la suma utilizando la función `io:fwrite/2`.

10. Se ordena la lista `Numeros` de forma descendente utilizando las funciones `lists:sort/1` y `lists:reverse/1`.

11. Se imprime la lista ordenada de forma descendente utilizando la función `io:fwrite/2`.