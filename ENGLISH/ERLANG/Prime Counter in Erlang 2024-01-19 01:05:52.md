```erlang

-module( prime_counter ).
-export( [ prime_counter/2, test/0 ] ).

prime_counter( From, To ) ->
  Primes = prime_counter([], From, To),
  length( Primes ).

prime_counter( Primes, From, To ) when From =< 2 ->
  prime_counter( [ From | Primes ], 3, To );
prime_counter( Primes, From, To ) when From > 2 ->
  case is_prime( From ) of
    true -> prime_counter( [ From | Primes ], From + 2, To );
    false -> prime_counter( Primes, From + 2, To )
  end;
prime_counter( Primes, _, _ ) ->
  lists:reverse( Primes ).

is_prime( N ) ->
  is_prime( N, 2 ).

is_prime( N, D ) when D * D > N ->
  true;
is_prime( N, D ) when N rem D =:= 0 ->
  false;
is_prime( N, D ) ->
  is_prime( N, D + 1 ).

test() ->
  io:fwrite( "~w~n", [ prime_counter( 1, 100 ) ] ),
  io:fwrite( "~w~n", [ prime_counter( 100, 1000 ) ] ),
  io:fwrite( "~w~n", [ prime_counter( 1000, 10000 ) ] ),
  io:fwrite( "~w~n", [ prime_counter( 10000, 100000 ) ] ).

```

Explanation:

1. `prime_counter/2`: This function takes two arguments, `From` and `To`, and returns the number of prime numbers between `From` and `To`.

2. `prime_counter([], From, To)`: This is the base case of the recursive function. It checks if `From` is less than or equal to 2, in which case it starts counting primes from 3. Otherwise, it continues the recursion.

3. `prime_counter([From | Primes], From + 2, To)`: This is the recursive case of the function. It checks if `From` is a prime number using the `is_prime/2` function. If it is a prime, it adds it to the list of primes and continues the recursion with `From + 2`. If it is not a prime, it continues the recursion without adding it to the list of primes.

4. `prime_counter(Primes, _, _)`: This is the termination case of the recursive function. It returns the list of primes found so far, reversed.

5. `is_prime/2`: This function checks if a given number `N` is a prime number. It uses the trial division method to check for divisibility by numbers up to the square root of `N`. If a divisor is found, it returns `false`. Otherwise, it returns `true`.

6. `test/0`: This function is used to test the `prime_counter/2` function. It calculates and prints the number of primes in various ranges.

This code is an example of a recursive function in Erlang. It uses a recursive approach to count the number of prime numbers between two given numbers efficiently. The `is_prime/2` function uses the trial division method to check if a number is prime. The `test/0` function is used to demonstrate the functionality of the `prime_counter/2` function.