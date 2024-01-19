```erlang
-module(complejo).
-export([sum/2, factorial/1, fibonacci/1, is_prime/1, gcd/2, lcm/2]).

% Suma de dos números
sum(A, B) ->
    A + B.

% Factorial de un número
factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N - 1).

% Serie de Fibonacci
fibonacci(0) ->
    0;
fibonacci(1) ->
    1;
fibonacci(N) ->
    fibonacci(N - 1) + fibonacci(N - 2).

% Comprobar si un número es primo
is_prime(N) when N < 2 ->
    false;
is_prime(2) ->
    true;
is_prime(N) ->
    not lists:any(fun(X) -> N rem X == 0 end, lists:seq(3, trunc(math:sqrt(N))), []).

% Máximo común divisor
gcd(A, B) ->
    gcd(B, A rem B) when A rem B /= 0;
gcd(_, 0) ->
    abs(A).

% Mínimo común múltiplo
lcm(A, B) ->
    abs(A * B) div gcd(A, B).
```

Explicación del código:

* La función `sum/2` suma dos números.
* La función `factorial/1` calcula el factorial de un número.
* La función `fibonacci/1` calcula el n-ésimo número de la serie de Fibonacci.
* La función `is_prime/1` comprueba si un número es primo.
* La función `gcd/2` calcula el máximo común divisor de dos números.
* La función `lcm/2` calcula el mínimo común múltiplo de dos números.

El código está escrito en Erlang, un lenguaje de programación concurrente y funcional. Erlang es conocido por su escalabilidad y robustez, lo que lo hace ideal para aplicaciones distribuidas y sistemas de tiempo real.