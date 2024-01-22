```erlang
% Función: saludar/1
% Argumentos:
%   nombre: un nombre como cadena
% Retorno:
%   un mensaje de saludo como cadena

-module(saludar).
-export([saludar/1]).

saludar(Nombre) ->
    io:format("Hola, ~s! ¿Cómo estás hoy?", [Nombre]).

% Función: sumar/2
% Argumentos:
%   a: un número como entero
%   b: un número como entero
% Retorno:
%   la suma de a y b como entero

-module(sumar).
-export([sumar/2]).

sumar(A, B) ->
    A + B.

% Función: factorial/1
% Argumentos:
%   n: un número natural como entero
% Retorno:
%   el factorial de n como entero

-module(factorial).
-export([factorial/1]).

factorial(N) when N == 0 ->
    1;
factorial(N) when N > 0 ->
    N * factorial(N-1).

% Función: fibonacci/1
% Argumentos:
%   n: un número natural como entero
% Retorno:
%   el n-ésimo número de Fibonacci como entero

-module(fibonacci).
-export([fibonacci/1]).

fibonacci(N) when N == 0 ->
    0;
fibonacci(N) when N == 1 ->
    1;
fibonacci(N) when N > 1 ->
    fibonacci(N-1) + fibonacci(N-2).

% Función: es_primo/1
% Argumentos:
%   n: un número natural como entero
% Retorno:
%   cierto si n es primo, falso en caso contrario

-module(es_primo).
-export([es_primo/1]).

es_primo(N) when N < 2 ->
    false;
es_primo(N) ->
    es_primo_aux(N, 2).

es_primo_aux(_, N) ->
    true;
es_primo_aux(N, Divisor) when N rem Divisor == 0 ->
    false;
es_primo_aux(N, Divisor) ->
    es_primo_aux(N, Divisor+1).

% Función: máximo_común_divisor/2
% Argumentos:
%   a: un número natural como entero
%   b: un número natural como entero
% Retorno:
%   el máximo común divisor de a y b como entero

-module(máximo_común_divisor).
-export([máximo_común_divisor/2]).

máximo_común_divisor(A, B) ->
    máximo_común_divisor_aux(A, B, A).

máximo_común_divisor_aux(_, 0, MCD) ->
    MCD;
máximo_común_divisor_aux(A, B, MCD) ->
    máximo_común_divisor_aux(B, A rem B, MCD).

% Función: mínimo_común_múltiplo/2
% Argumentos:
%   a: un número natural como entero
%   b: un número natural como entero
% Retorno:
%   el mínimo común múltiplo de a y b como entero

-module(mínimo_común_múltiplo).
-export([mínimo_común_múltiplo/2]).

mínimo_común_múltiplo(A, B) ->
    A * B div máximo_común_divisor(A, B).
```

Este código contiene una colección de funciones matemáticas básicas, como sumar, factorial, fibonacci, es_primo, máximo_común_divisor y mínimo_común_múltiplo. Cada función está definida en su propio módulo y exportada para que pueda ser utilizada desde otros módulos.

Aquí hay una explicación más detallada de cada función:

* `saludar/1`: Esta función toma un nombre como argumento y devuelve un mensaje de saludo personalizado.
* `sumar/2`: Esta función toma dos números como argumentos y devuelve su suma.
* `factorial/1`: Esta función toma un número natural como argumento y devuelve su factorial.
* `fibonacci/1`: Esta función toma un número natural como argumento y devuelve el n-ésimo número de Fibonacci.
* `es_primo/1`: Esta función toma un número natural como argumento y devuelve cierto si el número es primo, y falso en caso contrario.
* `máximo_común_divisor/2`: Esta función toma dos números naturales como argumentos y devuelve su máximo común divisor.
* `mínimo_común_múltiplo/2`: Esta función toma dos números naturales como argumentos y devuelve su mínimo común múltiplo.