```erlang
-module(factorizador).

-export([factorizar/1]).

% Factorización de enteros
factorizar(N) ->
    Res = primes(),
    factorizar(N, Res, []).

% Factorización recursiva
factorizar(_, [], Acc) ->
    Acc;
factorizar(N, [P | Rest], Acc) ->
    if
        N rem P == 0 ->
            factorizar(N div P, Rest, [P | Acc]);
        true ->
            factorizar(N, Rest, Acc)
    end.

% Generación de números primos
primes() ->
    primes(2, []).

% Generación de números primos recursiva
primes(N, Primes) ->
    if
        is_prime(N) ->
            primes(N+1, [N | Primes]);
        true ->
            primes(N+1, Primes)
    end.

% Comprobación de primalidad
is_prime(N) ->
    is_prime(N, 2).

% Comprobación de primalidad recursiva
is_prime(_, N) ->
    true;
is_prime(N, D) when N rem D == 0 ->
    false;
is_prime(N, D) ->
    is_prime(N, D+1).
```

Este código implementa un factorizador de números enteros en Erlang. El factorizador utiliza el método de criba de Eratóstenes para generar una lista de números primos, y luego utiliza esta lista para factorizar el número entero dado.

La función `factorizar/1` toma un número entero como argumento y devuelve una lista de sus factores primos. La función utiliza la función `primes()` para generar una lista de números primos, y luego utiliza la función `factorizar/3` para factorizar el número entero dado utilizando la lista de números primos.

La función `factorizar/3` toma tres argumentos: el número entero a factorizar, la lista de números primos y la lista de factores primos encontrados hasta el momento. La función utiliza la función `is_prime/1` para comprobar si un número es primo, y utiliza la función `factorizar/3` de forma recursiva para factorizar el número entero dado utilizando la lista de números primos.

La función `primes()` genera una lista de números primos utilizando el método de criba de Eratóstenes. La función toma dos argumentos: el número máximo de números primos a generar y la lista de números primos generados hasta el momento. La función utiliza la función `is_prime/1` para comprobar si un número es primo, y utiliza la función `primes/2` de forma recursiva para generar la lista de números primos.

La función `is_prime/1` comprueba si un número es primo. La función toma un número como argumento y devuelve `true` si el número es primo, o `false` si el número no es primo. La función utiliza la función `is_prime/2` de forma recursiva para comprobar si un número es primo.

La función `is_prime/2` comprueba si un número es primo dividiéndolo por todos los números primos menores o iguales a su raíz cuadrada. Si el número es divisible por alguno de estos números primos, entonces el número no es primo. De lo contrario, el número es primo.