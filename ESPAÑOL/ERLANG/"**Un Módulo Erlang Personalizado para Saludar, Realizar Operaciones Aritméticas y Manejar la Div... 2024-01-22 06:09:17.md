```erlang
-module(mi_super_modulo).

-export([saludar/0, sumar/2, restar/2, multiplicar/2, dividir/2]).

% Función para saludar
saludar() ->
    io:fwrite("¡Hola, mundo!").

% Función para sumar dos números
sumar(A, B) ->
    A + B.

% Función para restar dos números
restar(A, B) ->
    A - B.

% Función para multiplicar dos números
multiplicar(A, B) ->
    A * B.

% Función para dividir dos números
dividir(A, B) cuando B =/= 0 -> % Comprobar que el divisor no sea cero
    A / B;
dividir(_, _) ->
    io:fwrite("No se puede dividir por cero").

-endif.
```

Este código define un módulo Erlang llamado `mi_super_modulo` que contiene cinco funciones: `saludar/0`, `sumar/2`, `restar/2`, `multiplicar/2` y `dividir/2`.

La función `saludar/0` simplemente imprime el mensaje "¡Hola, mundo!" en la consola.

Las funciones `sumar/2`, `restar/2` y `multiplicar/2` toman dos argumentos y devuelven el resultado de sumar, restar o multiplicar los dos argumentos.

La función `dividir/2` también toma dos argumentos, pero comprueba que el segundo argumento no sea cero antes de realizar la división. Si el segundo argumento es cero, la función imprime el mensaje "No se puede dividir por cero" en la consola.

Para utilizar este módulo, puedes importarlo en otro módulo Erlang utilizando la siguiente línea de código:

```erlang
-import(mi_super_modulo, [saludar/0, sumar/2, restar/2, multiplicar/2, dividir/2]).
```

Una vez importado el módulo, puedes utilizar sus funciones en tu código. Por ejemplo, el siguiente código imprime el mensaje "¡Hola, mundo!" en la consola y luego suma los números 1 y 2 e imprime el resultado:

```erlang
mi_super_modulo:saludar();
io:fwrite("El resultado de sumar 1 y 2 es: ~p~n", [mi_super_modulo:sumar(1, 2)]).
```

El resultado de ejecutar este código será:

```
¡Hola, mundo!
El resultado de sumar 1 y 2 es: 3
```