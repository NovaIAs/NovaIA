```erlang
-module(mi_modulo).
-export([hello/0, factorial/1]).

% Función que imprime un mensaje de saludo en la consola.
hello() ->
    io:fwrite("¡Hola, mundo!").

% Función que calcula el factorial de un número.
factorial(0) ->
    1;
factorial(N) when N > 0 ->
    N * factorial(N-1).
```

Explicación del código:

* El módulo `mi_modulo` es un contenedor de funciones en Erlang.
* La función `hello()` imprime el mensaje "¡Hola, mundo!" en la consola. La función `io:fwrite()` se utiliza para imprimir datos en la consola.
* La función `factorial()` calcula el factorial de un número. La cláusula `factorial(0) -> 1` es un caso base que devuelve 1 si el número es 0. La cláusula `factorial(N) when N > 0 -> N * factorial(N-1)` es un caso recursivo que multiplica el número por el factorial del número anterior.
* Las cláusulas `when` se utilizan para comprobar si se cumplen ciertas condiciones. En este caso, la cláusula `when N > 0` comprueba si el número es mayor que 0.
* El operador `*` se utiliza para multiplicar dos números.
* El operador `-` se utiliza para restar dos números.
* El operador `->` se utiliza para separar la condición de la expresión.

Este código es complejo porque utiliza recursividad y cláusulas `when`. La recursividad es una técnica en la que una función se llama a sí misma. Las cláusulas `when` se utilizan para comprobar si se cumplen ciertas condiciones. Este código también es complejo porque utiliza varios operadores y expresiones.