```erlang
-module(mi_código_complejo).
-export([sumar/2, restar/2, multiplicar/2, dividir/2]).

% Función para sumar dos números
sumar(X, Y) ->
    X + Y.

% Función para restar dos números
restar(X, Y) ->
    X - Y.

% Función para multiplicar dos números
multiplicar(X, Y) ->
    X * Y.

% Función para dividir dos números
dividir(X, Y) ->
    X / Y.

% Función principal
-main(_) ->
    A = 10,
    B = 5,
    Suma = sumar(A, B),
    Resta = restar(A, B),
    Multiplicacion = multiplicar(A, B),
    Division = dividir(A, B),
    io:format("La suma de ~p y ~p es ~p~n", [A, B, Suma]),
    io:format("La resta de ~p y ~p es ~p~n", [A, B, Resta]),
    io:format("La multiplicación de ~p y ~p es ~p~n", [A, B, Multiplicacion]),
    io:format("La división de ~p y ~p es ~p~n", [A, B, Division]).
```

Este código es un módulo Erlang llamado `mi_código_complejo`, que contiene cuatro funciones matemáticas básicas (`sumar`, `restar`, `multiplicar` y `dividir`) y una función principal `-main(_)`.

Las funciones matemáticas básicas toman dos argumentos y devuelven el resultado de la operación correspondiente.

La función principal `-main(_)`, que se invoca con el argumento `_` (que no se utiliza), define dos variables numéricas `A` y `B`, y luego utiliza las funciones matemáticas básicas para calcular la suma, la resta, la multiplicación y la división de `A` y `B`.

Finalmente, la función principal imprime los resultados en la consola utilizando la función `io:format`.

Este código es un ejemplo de cómo utilizar Erlang para realizar cálculos matemáticos básicos.