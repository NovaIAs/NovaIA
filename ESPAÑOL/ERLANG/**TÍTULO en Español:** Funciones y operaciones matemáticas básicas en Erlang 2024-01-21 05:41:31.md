```erlang
-module(helloworld).
-export([main/0]).

main() ->
    io:fwrite("Hola, mundo!\n"),
    1.

% Esta es una función que calcula el factorial de un número.
factorial(N) ->
    factorial(N, 1).

factorial(0, Acc) ->
    Acc;
factorial(N, Acc) ->
    factorial(N-1, Acc*N).

% Esta es una función que convierte un número decimal a binario.
decimal_to_binary(N) ->
    decimal_to_binary(N, []).

decimal_to_binary(0, Acc) ->
    Acc;
decimal_to_binary(N, Acc) ->
    decimal_to_binary(N div 2, [N rem 2 | Acc]).

% Esta es una función que convierte un número binario a decimal.
binary_to_decimal(B) ->
    binary_to_decimal(B, 0).

binary_to_decimal([], Acc) ->
    Acc;
binary_to_decimal([H|T], Acc) ->
    binary_to_decimal(T, Acc*2 + H).

% Esta es una función que calcula el mayor común divisor de dos números.
gcd(A, B) ->
    gcd(A, B, A).

gcd(A, 0, _) ->
    A;
gcd(0, B, _) ->
    B;
gcd(A, B, GCD) ->
    gcd(B rem GCD, GCD, GCD).

% Esta es una función que calcula el mínimo común múltiplo de dos números.
lcm(A, B) ->
    lcm(A, B, A*B).

lcm(A, 0, _) ->
    0;
lcm(0, B, _) ->
    0;
lcm(A, B, LCM) ->
    gcd(A, B, GCD),
    lcm(LCM/GCD, GCD, LCM*GCD).

% Esta es una función que devuelve una lista de los primeros N números primos.
prime_numbers(N) ->
    prime_numbers(N, 2, []).

prime_numbers(0, _, Primes) ->
    Primes;
prime_numbers(N, P, Primes) ->
    case is_prime(P) of
        true -> prime_numbers(N-1, P+1, [P|Primes]);
        false -> prime_numbers(N, P+1, Primes)
    end.

is_prime(N) ->
    is_prime(N, 2, math:sqrt(N)).

is_prime(N, _, 1) ->
    true;
is_prime(N, D, S) ->
    case N rem D of
        0 -> false;
        _ -> is_prime(N, D+1, S)
    end.

```