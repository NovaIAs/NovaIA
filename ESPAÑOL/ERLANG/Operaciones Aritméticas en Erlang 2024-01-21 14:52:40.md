```erlang
-module(calculos_complejos).
-export([sumar/2, restar/2, multiplicar/2, dividir/2]).

% Función para sumar dos valores
sumar(A, B) ->
    A + B.

% Función para restar dos valores
restar(A, B) ->
    A - B.

% Función para multiplicar dos valores
multiplicar(A, B) ->
    A * B.

% Función para dividir dos valores
dividir(A, B) cuando A == 0 ->
    error("No se puede dividir por cero");
dividir(A, B) ->
    A / B.

% Función auxiliar para detectar casos de error
error(Mensaje) ->
    erlang:error(Mensaje).
```

Este código define un módulo Erlang llamado `calculos_complejos` que contiene cuatro funciones: `sumar/2`, `restar/2`, `multiplicar/2` y `dividir/2`. Cada función toma dos argumentos y devuelve el resultado de la operación correspondiente.

La función `sumar/2` simplemente suma los dos argumentos y devuelve el resultado.

La función `restar/2` resta el segundo argumento del primero y devuelve el resultado.

La función `multiplicar/2` multiplica los dos argumentos y devuelve el resultado.

La función `dividir/2` divide el primer argumento por el segundo y devuelve el resultado. Sin embargo, si el primer argumento es 0, la función devuelve un error.

La función auxiliar `error/1` se utiliza para generar mensajes de error. La función toma un argumento, que es el mensaje de error, y lo escribe en el registro de errores de Erlang.