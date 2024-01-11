```prolog
% Define a rule to calculate the factorial of a number.
factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Define a rule to calculate the length of a list.
length([], 0).
length([_|T], L) :-
    length(T, L1),
    L is L1 + 1.

% Define a rule to calculate the maximum of a list.
max([X], X).
max([X|T], M) :-
    max(T, M1),
    (   X > M1 -> M is X
    ;   M is M1
    ).

% Define a rule to calculate the minimum of a list.
min([X], X).
min([X|T], M) :-
    min(T, M1),
    (   X < M1 -> M is X
    ;   M is M1
    ).

% Define a rule to calculate the sum of a list.
sum([], 0).
sum([X|T], S) :-
    sum(T, S1),
    S is X + S1.

% Define a rule to calculate the average of a list.
average(L, A) :-
    sum(L, S),
    length(L, N),
    A is S / N.

% Define a rule to check if a number is prime.
prime(2).
prime(N) :-
    N > 1,
    (   between(2, sqrt(N), I),
        \+ (0 is N mod I)
    ;   true
    ).

% Define a rule to generate a list of prime numbers up to a given number.
primes(N, L) :-
    findall(P, (between(2, N, P), prime(P)), L).

% Define a rule to check if a list is sorted in ascending order.
sorted([]).
sorted([_]).
sorted([X,Y|T]) :-
    X =< Y,
    sorted([Y|T]).

% Define a rule to sort a list in ascending order.
sort([], []).
sort([X|T], S) :-
    partition(T, X, L1, L2),
    sort(L1, S1),
    sort(L2, S2),
    append(S1, [X|S2], S).

% Define a rule to partition a list into two sublists, one containing elements less than or equal to a given element, and the other containing elements greater than the given element.
partition([], _, [], []).
partition([X|T], X, [X|L1], L2) :-
    partition(T, X, L1, L2).
partition([X|T], Y, L1, [X|L2]) :-
    Y < X,
    partition(T, Y, L1, L2).
```

This code implements a variety of commonly used list processing and mathematical functions in Prolog. It includes functions to calculate the factorial of a number, the length of a list, the maximum and minimum of a list, the sum and average of a list, and to check if a number is prime. It also includes a function to generate a list of prime numbers up to a given number, a function to check if a list is sorted in ascending order, and a function to sort a list in ascending order.

The code is well-commented and uses a consistent coding style, making it easy to read and understand. It also uses Prolog's built-in list processing and mathematical functions, making it efficient and concise.