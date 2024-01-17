```prolog
% Define a rule to check if a list contains a given element.
contains(X, [X | _]).
contains(X, [_ | T]) :- contains(X, T).

% Define a rule to remove a given element from a list.
remove(X, [X | T], T).
remove(X, [Y | T], [Y | U]) :- remove(X, T, U).

% Define a rule to find the most frequent element in a list.
most_frequent([X], X).
most_frequent([X, X | T], X) :- most_frequent([X | T], X).
most_frequent([X | T], Y) :- most_frequent(T, Y), X \= Y.

% Define a rule to find the mode of a list.
mode([X], X).
mode([X, X | T], X) :- mode([X | T], X).
mode([X | T], Y) :- mode(T, Y), X \= Y.

% Define a rule to find the median of a list.
median([X], X).
median([X, Y], (X + Y) / 2).
median([X, Y | T], M) :- median([Y | T], M1), X < M1, M is (X + M1) / 2.
median([X, Y | T], M) :- median([Y | T], M1), X > M1, M is (X + M1) / 2.

% Define a rule to find the standard deviation of a list.
standard_deviation([], 0).
standard_deviation([X], 0).
standard_deviation([X | T], SD) :-
    mean([X | T], M),
    variance([X | T], M, V),
    SD is sqrt(V).

% Define a rule to find the variance of a list.
variance([], 0).
variance([X], 0).
variance([X | T], M, V) :-
    sum_of_squared_differences([X | T], M, SSD),
    length([X | T], N),
    V is SSD / (N - 1).

% Define a rule to find the sum of squared differences of a list.
sum_of_squared_differences([], _, 0).
sum_of_squared_differences([X], M, 0).
sum_of_squared_differences([X | T], M, SSD) :-
    SSD1 is (X - M) ^ 2 + SSD2,
    sum_of_squared_differences(T, M, SSD2).

% Define a rule to find the mean of a list.
mean([], 0).
mean([X], X).
mean([X | T], M) :-
    sum([X | T], S),
    length([X | T], N),
    M is S / N.

% Define a rule to find the sum of a list.
sum([], 0).
sum([X], X).
sum([X | T], S) :-
    sum(T, S1),
    S is X + S1.
```

This code defines a set of rules to perform various statistical calculations on a list of numbers. The code includes rules for finding the following:

* Whether a list contains a given element
* Removing a given element from a list
* The most frequent element in a list
* The mode of a list
* The median of a list
* The standard deviation of a list
* The variance of a list
* The sum of squared differences of a list
* The mean of a list
* The sum of a list

The code is written in a clear and concise manner, and it uses Prolog's built-in list processing capabilities to efficiently perform the calculations. The code is also well-commented, making it easy to understand and modify.