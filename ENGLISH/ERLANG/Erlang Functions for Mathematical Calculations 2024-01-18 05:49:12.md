```erlang
% Define a function to calculate the factorial of a number.
factorial(N) when N >= 0 ->
    if
        N == 0 -> 1;
        true -> N * factorial(N-1)
    end;
factorial(_) ->
    throw({error, "Invalid input: Factorial of a negative number is not defined"}).

% Define a function to check if a number is prime.
is_prime(N) when N > 1 ->
    is_prime(N, 2);
is_prime(_) ->
    false.

is_prime(N, D) when D * D > N ->
    true;
is_prime(N, D) when rem(N, D) == 0 ->
    false;
is_prime(N, D) ->
    is_prime(N, D+1).

% Define a function to generate a list of prime numbers up to a given limit.
prime_list(N) when N > 1 ->
    [2 | prime_list(N, 3)];
prime_list(_, _) ->
    [].

prime_list(N, D) when D * D > N ->
    [D | prime_list(N, D+2)];
prime_list(N, D) ->
    prime_list(N, D+2).

% Define a function to find the largest prime factor of a given number.
largest_prime_factor(N) when N > 1 ->
    largest_prime_factor(N, 2).

largest_prime_factor(N, D) when D * D > N ->
    N;
largest_prime_factor(N, D) when rem(N, D) == 0 ->
    largest_prime_factor(N / D, D);
largest_prime_factor(N, D) ->
    largest_prime_factor(N, D+1).

% Define a function to calculate the greatest common divisor of two numbers.
gcd(A, B) ->
    if
        B == 0 -> A;
        true -> gcd(B, rem(A, B))
    end.

% Define a function to calculate the least common multiple of two numbers.
lcm(A, B) ->
    A * B div gcd(A, B).

% Define a function to calculate the Fibonacci sequence up to a given limit.
fibonacci(N) when N > 0 ->
    fibonacci(1, 1, N-1).

fibonacci(A, B, 0) ->
    [A];
fibonacci(A, B, N) ->
    [A | fibonacci(B, A+B, N-1)].

% Define a function to check if a string is a palindrome.
is_palindrome(String) ->
    is_palindrome(String, []).

is_palindrome([], Reversed) ->
    Reversed == [];
is_palindrome([H|T], Reversed) ->
    is_palindrome(T, [H|Reversed]).

% Define a function to convert a decimal number to a binary number.
to_binary(N) ->
    to_binary(N, []).

to_binary(0, Acc) ->
    Acc;
to_binary(N, Acc) ->
    to_binary(N div 2, [rem(N,