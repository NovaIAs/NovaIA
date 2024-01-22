```erlang
-module(mi_modulo).
-export([funcion_principal/0]).

%-*- coding: utf-8 -*-
% Primer comentario de prueba.

% Función principal del módulo.
funcion_principal() ->
    % Variable que almacena el nombre del usuario.
    Nombre = io:get_line("Ingrese su nombre: "),

    % Imprime un mensaje de bienvenida al usuario.
    io:fwrite("Hola, ~s", [Nombre]),

    % Llama a la función `solicitar_numero` para obtener el número del usuario.
    Numero = solicitar_numero(),

    % Imprime el número introducido por el usuario.
    io:fwrite("El número introducido es: ~w", [Numero]),

    % Llama a la función `calcular_factorial` para obtener el factorial del número.
    Factorial = calcular_factorial(Numero),

    % Imprime el factorial del número.
    io:fwrite("El factorial de ~w es: ~w", [Numero, Factorial]).

% Función para solicitar el número al usuario.
solicitar_numero() ->
    % Variable que almacena el número introducido por el usuario.
    Numero = io:get_line("Ingrese un número: "),

    % Comprueba si el número introducido es un entero.
    case erlang:is_integer(Numero) of
        % Si el número es un entero, lo devuelve.
        true -> Numero;

        % Si el número no es un entero, imprime un mensaje de error y vuelve a solicitar el número.
        false -> io:fwrite("El número introducido no es válido"),
                                   solicitar_numero()
    end.

% Función para calcular el factorial de un número.
calcular_factorial(Numero) ->
    % Caso base: si el número es 0, el factorial es 1.
    case Numero of
        0 -> 1;

        % Caso recursivo: para cualquier otro número, el factorial se calcula multiplicando el número por el factorial del número anterior.
        _ -> Numero * calcular_factorial(Numero - 1)
    end.
```

Explicación:

* La primera línea del código define el módulo `mi_modulo` y la función `funcion_principal/0`, que es la función principal del módulo.
* La segunda línea define una variable llamada `Nombre` que almacena el nombre del usuario.
* La tercera línea imprime un mensaje de bienvenida al usuario.
* La cuarta línea llama a la función `solicitar_numero` para obtener el número del usuario.
* La quinta línea imprime el número introducido por el usuario.
* La sexta línea llama a la función `calcular_factorial` para obtener el factorial del número.
* La séptima línea imprime el factorial del número.

La función `solicitar_numero` solicita el número al usuario y comprueba si el número introducido es un entero. Si el número es un entero, lo devuelve. Si el número no es un entero, imprime un mensaje de error y vuelve a solicitar el número.

La función `calcular_factorial` calcula el factorial de un número. El factorial de un número es el producto de todos los números enteros positivos desde 1 hasta el número. Por ejemplo, el factorial de 5 es 120, que se calcula como 5 * 4 * 3 * 2 * 1.

El caso base de la función `calcular_factorial` es cuando el número es 0. En este caso, el factorial es 1. El caso recursivo de la función `calcular_factorial` es cuando el número es mayor que 0. En este caso, el factorial se calcula multiplicando el número por el factorial del número anterior.