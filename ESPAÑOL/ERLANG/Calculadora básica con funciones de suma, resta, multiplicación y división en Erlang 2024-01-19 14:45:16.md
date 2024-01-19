```erlang
%% Módulo para una calculadora simple

-module(calculadora).
-export([suma/2, resta/2, multiplicación/2, división/2]).

%% Función para sumar dos números
suma(A, B) ->
    A + B.

%% Función para restar dos números
resta(A, B) ->
    A - B.

%% Función para multiplicar dos números
multiplicación(A, B) ->
    A * B.

%% Función para dividir dos números
división(A, B) ->
    A / B.
```

Este código define un módulo llamado `calculadora` con cuatro funciones: `suma`, `resta`, `multiplicación` y `división`. Cada función toma dos argumentos y devuelve el resultado de la operación correspondiente.

El código utiliza la sintaxis `-module` para definir el nombre del módulo y la sintaxis `-export` para exportar las funciones del módulo para que puedan ser utilizadas por otros módulos.

Las funciones utilizan la sintaxis `A + B`, `A - B`, `A * B` y `A / B` para realizar las operaciones de suma, resta, multiplicación y división, respectivamente.

Este código es un ejemplo simple de cómo se puede utilizar Erlang para crear una calculadora básica.