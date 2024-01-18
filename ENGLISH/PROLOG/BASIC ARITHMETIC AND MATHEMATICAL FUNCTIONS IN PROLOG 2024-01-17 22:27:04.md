```
:- dynamic rules/2.

% Define a rule that adds two numbers together.
assert(rules(add(X, Y, Z),
    [
        % Check if X and Y are numbers.
        number(X),
        number(Y),
        % Calculate the sum of X and Y.
        Z is X + Y
    ])).

% Define a rule that subtracts two numbers.
assert(rules(subtract(X, Y, Z),
    [
        % Check if X and Y are numbers.
        number(X),
        number(Y),
        % Calculate the difference of X and Y.
        Z is X - Y
    ])).

% Define a rule that multiplies two numbers.
assert(rules(multiply(X, Y, Z),
    [
        % Check if X and Y are numbers.
        number(X),
        number(Y),
        % Calculate the product of X and Y.
        Z is X * Y
    ])).

% Define a rule that divides two numbers.
assert(rules(divide(X, Y, Z),
    [
        % Check if X and Y are numbers.
        number(X),
        number(Y),
        % Check if Y is not equal to 0.
        Y =\= 0,
        % Calculate the quotient of X and Y.
        Z is X / Y
    ])).

% Define a rule that calculates the factorial of a number.
assert(rules(factorial(N, F),
    [
        % Check if N is a non-negative integer.
        integer(N),
        N >= 0,
        % Calculate the factorial of N.
        factorial_helper(N, 1, F)
    ])).

factorial_helper(0, F, F).
factorial_helper(N, Acc, F) :-
    N > 0,
    NewN is N - 1,
    NewAcc is Acc * N,
    factorial_helper(NewN, NewAcc, F).

% Define a rule that calculates the greatest common divisor of two numbers.
assert(rules(gcd(X, Y, GCD),
    [
        % Check if X and Y are numbers.
        number(X),
        number(Y),
        % Calculate the greatest common divisor of X and Y.
        gcd_helper(X, Y, GCD)
    ])).

gcd_helper(X, 0, X).
gcd_helper(X, Y, GCD) :-
    Y > 0,
    NewY is X mod Y,
    gcd_helper(Y, NewY, GCD).

% Define a rule that checks if a number is prime.
assert(rules(prime(N),
    [
        % Check if N is a positive integer.
        integer(N),
        N > 1,
        % Check if N has any divisors other than 1 and itself.
        \+ (between(2, N-1, Divisor), N mod Divisor =:= 0)
    ])).

% Define a rule that finds all the prime numbers up to a given number.
assert(rules(primes_up_to(N, Primes),
    [
        % Check if N is a positive integer.
        integer(N),
        N > 1,
        % Find all the prime numbers up to N.
        primes_helper(2, N, Primes)
    ])).

primes_helper(N, N, [N]).
primes_helper(N, Max, [N | Primes]) :-
    N < Max,
    NewN is N + 1,
    prime(NewN),
    primes_helper(NewN, Max, Primes).

% Define a rule that calculates the Fibonacci sequence up to a given number.
assert(rules(fibonacci_sequence(N, Sequence),
    [
        % Check if N is a non-negative integer.
        integer(N),
        N >= 0,
        % Calculate the Fibonacci sequence up to N.
        fibonacci_helper(0, 1, N, Sequence)
    ])).

fibonacci_helper(A, B, N, [A | Sequence]) :-
    N > 0,
    NewN is N - 1,
    NewA is A + B,
    fibonacci_helper(B, NewA, NewN, Sequence).

fibonacci_helper(_, _, 0, []).

% Define a rule that sorts a list of numbers in ascending order.
assert(rules(sort(List, SortedList),
    [
        % Check if List is a list.
        list(List),
        % Sort the list in ascending order.
        sort_helper(List, SortedList)
    ])).

sort_helper([], []).
sort_helper([Pivot | Tail], SortedList) :-
    partition(Pivot, Tail, Left, Right),
    sort_helper(Left, LeftSortedList),
    sort_helper(Right, RightSortedList),
    append(LeftSortedList, [Pivot | RightSortedList], SortedList).

partition(Pivot, List, Left, Right) :-
    partition_helper(Pivot, List, [], [], Left, Right).

partition_helper(_, [], Left, Right, Left, Right).
partition_helper(Pivot, [Head | Tail], Left, Right, NewLeft, NewRight) :-
    compare(Head, Pivot, =),
    !,
    partition_helper(Pivot, Tail, Left, [Head | Right], NewLeft, NewRight).
partition_helper(Pivot, [Head | Tail], Left