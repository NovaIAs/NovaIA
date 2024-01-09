```erlang

-module(complex_erlang_code).
-export([main/0]).

main() ->
    % Define a function to calculate the factorial of a number
    factorial(N) ->
        if
            N == 0 ->
                1;
            N > 0 ->
                N * factorial(N-1);
            true ->
                throw(error)
        end.

    % Define a function to generate a list of prime numbers up to a given number
    prime_list(N) ->
        prime_list(N, 2, []).

prime_list(N, Current, Acc) ->
    if
        Current > N ->
            Acc;
        is_prime(Current) ->
            prime_list(N, Current+1, [Current|Acc]);
        true ->
            prime_list(N, Current+1, Acc)
    end.

is_prime(N) ->
    is_prime(N, 2).

is_prime(N, Current) ->
    if
        Current == N ->
            true;
        N rem Current == 0 ->
            false;
        true ->
            is_prime(N, Current+1)
    end.

    % Define a function to find the longest common subsequence of two strings
    lcs(String1, String2) ->
        lcs(String1, String2, []).

lcs([], [], Acc) ->
    Acc;
lcs(String1, String2, Acc) ->
    case lists:member(hd(String1), String2) of
        true ->
            lcs(tl(String1), tl(String2), [hd(String1)|Acc]);
        false ->
            lcs(tl(String1), String2, Acc)
    end.

    % Define a function to calculate the Fibonacci sequence up to a given number
    fibonacci(N) ->
        fibonacci(N, 0, 1).

fibonacci(N, A, B) ->
    if
        N == 0 ->
            A;
        N == 1 ->
            B;
        true ->
            fibonacci(N-1, B, A+B)
    end.

    % Print the results of the functions
    io:format("Factorial of 5: ~p~n", [factorial(5)]),
    io:format("Prime list up to 100: ~p~n", [prime_list(100)]),
    io:format("Longest common subsequence of \"Hello\" and \"World\": ~p~n", [lcs("Hello", "World")]),
    io:format("Fibonacci sequence up to 10: ~p~n", [fibonacci(10)]).

```

Here's a more detailed explanation of the code:

1. **Factorial Function**:
   - `factorial/1` calculates the factorial of a given number using recursion.
   - If the number is 0, it returns 1.
   - If the number is greater than 0, it multiplies the number by the factorial of the previous number.

2. **Prime List Function**:
   - `prime_list/1` generates a list of prime numbers up to a given number.
   - It starts with the number 2 and recursively checks each number up to the given number to see if it is prime.
   - If a number is prime, it is added to the list.

3. **Is Prime Function**:
   - `is_prime/1` checks if a given number is prime.
   - It starts with the number 2 and checks if the given number is divisible by any number up to itself.
   - If the number is divisible by any number other than 1 and itself, it is not prime.

4. **Longest Common Subsequence Function**:
   - `lcs/3` finds the longest common subsequence of two strings.
   - It recursively compares the two strings character by character and adds the common characters to a list.
   - The final list is the longest common subsequence of the two strings.

5. **Fibonacci Function**:
   - `fibonacci/3` calculates the Fibonacci sequence up to a given number.
   - It starts with the first two numbers of the sequence (0 and 1) and recursively calculates the subsequent numbers by adding the previous two numbers.

6. **Main Function**:
   - The main function calls the above functions and prints the results.
   - It calculates the factorial of 5, generates a list of prime numbers up to 100, finds the longest common subsequence of "Hello" and "World", and calculates the Fibonacci sequence up to 10.

This code demonstrates various concepts in Erlang, including recursion, pattern matching, and list manipulation. It also shows how to write complex and differentiated code in Erlang.