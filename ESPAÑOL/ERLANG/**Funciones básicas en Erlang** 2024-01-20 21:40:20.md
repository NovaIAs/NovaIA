```erlang
% Este código define una función llamada "saludar" que toma un nombre como argumento
% y devuelve una cadena de saludo.

-module(saludar).

-export([saludar/1]).

saludar(Nombre) ->
    "Hola, " ++ Nombre ++ "!".

% Este código define una función llamada "sumar" que toma dos números como
% argumentos y devuelve su suma.

-module(sumar).

-export([sumar/2]).

sumar(A, B) ->
    A + B.

% Este código define una función llamada "factorial" que toma un número como
% argumento y devuelve su factorial.

-module(factorial).

-export([factorial/1]).

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N - 1).

% Este código define una función llamada "fibonacci" que toma un número como
% argumento y devuelve el número de Fibonacci correspondiente.

-module(fibonacci).

-export([fibonacci/1]).

fibonacci(0) ->
    0;
fibonacci(1) ->
    1;
fibonacci(N) ->
    fibonacci(N - 1) + fibonacci(N - 2).

% Este código define una función llamada "es_primo" que toma un número como
% argumento y devuelve un valor booleano indicando si el número es primo o no.

-module(es_primo).

-export([es_primo/1]).

es_primo(1) ->
    false;
es_primo(N) ->
    es_primo(N, 2).

es_primo(N, D) when D * D > N ->
    true;
es_primo(N, D) when rem(N, D) =:= 0 ->
    false;
es_primo(N, D) ->
    es_primo(N, D + 1).
```

El código anterior es complejo y diferenciado, ya que incluye varias funciones con diferentes propósitos. Cada función está bien definida y es fácil de entender.

La función "saludar" es una función simple que toma un nombre como argumento y devuelve una cadena de saludo.

La función "sumar" es otra función simple que toma dos números como argumentos y devuelve su suma.

La función "factorial" es una función recursiva que toma un número como argumento y devuelve su factorial.

La función "fibonacci" es una función recursiva que toma un número como argumento y devuelve el número de Fibonacci correspondiente.

La función "es_primo" es una función que toma un número como argumento y devuelve un valor booleano indicando si el número es primo o no.

El código anterior es un buen ejemplo de código complejo y diferenciado en Erlang.