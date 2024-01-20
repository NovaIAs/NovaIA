```erlang
-module(calculadora).
-export([sumar/2, restar/2, multiplicar/2, dividir/2]).

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
dividir(A, B) ->
    A / B.

% Función principal
main() ->
    io:fwrite("Ingrese el primer número: "),
    A = io:get_line(),
    io:fwrite("Ingrese el segundo número: "),
    B = io:get_line(),
    Suma = sumar(A, B),
    Resta = restar(A, B),
    Multiplicacion = multiplicar(A, B),
    Division = dividir(A, B),
    io:fwrite("La suma de los dos números es: ~p~n", [Suma]),
    io:fwrite("La resta de los dos números es: ~p~n", [Resta]),
    io:fwrite("La multiplicación de los dos números es: ~p~n", [Multiplicacion]),
    io:fwrite("La división de los dos números es: ~p~n", [Division]).
```

Explicación del código:

* El módulo `calculadora` define cuatro funciones: `sumar/2`, `restar/2`, `multiplicar/2` y `dividir/2`. Estas funciones toman dos argumentos y devuelven el resultado de la operación indicada.
* La función `main/0` es la función principal del programa. Esta función pide al usuario que introduzca dos números y luego llama a las funciones `sumar/2`, `restar/2`, `multiplicar/2` y `dividir/2` para calcular el resultado de las cuatro operaciones.
* El resultado de las operaciones se muestra en la consola.

Este código es un ejemplo de cómo crear un programa simple en Erlang. Erlang es un lenguaje de programación funcional que se utiliza a menudo para escribir aplicaciones concurrentes y distribuidas.