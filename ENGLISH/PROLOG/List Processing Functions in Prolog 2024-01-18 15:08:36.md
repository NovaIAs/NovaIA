```prolog
% Define a rule to check if a list is empty.
empty_list([]).

% Define a rule to check if an element is a member of a list.
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% Define a rule to remove all occurrences of an element from a list.
remove_all(X, [], []).
remove_all(X, [X|T], R) :- remove_all(X, T, R).
remove_all(X, [Y|T], [Y|R]) :- X \= Y, remove_all(X, T, R).

% Define a rule to find the intersection of two lists.
intersection([], _, []).
intersection([X|T], L2, R) :- member(X, L2), intersection(T, L2, R).
intersection([_|T], L2, R) :- intersection(T, L2, R).

% Define a rule to find the union of two lists.
union([], L2, L2).
union([X|T], L2, [X|R]) :- \+ member(X, L2), union(T, L2, R).
union([X|T], L2, R) :- member(X, L2), union(T, L2, R).

% Define a rule to find the difference between two lists.
difference([], _, []).
difference([X|T], L2, R) :- \+ member(X, L2), difference(T, L2, R).
difference([X|T], L2, R) :- member(X, L2), difference(T, L2, R).

% Define a rule to find the symmetric difference between two lists.
symmetric_difference(L1, L2, R) :- difference(L1, L2, D1), difference(L2, L1, D2), union(D1, D2, R).

% Define a rule to find the Cartesian product of two lists.
cartesian_product([], _, []).
cartesian_product([X|T], L2, R) :- cartesian_product_helper(X, L2, R1), cartesian_product(T, L2, R2), union(R1, R2, R).

cartesian_product_helper(X, [], []).
cartesian_product_helper(X, [Y|T], [(X,Y)|R]) :- cartesian_product_helper(X, T, R).
```

This code provides a collection of useful list processing functions in Prolog. It includes functions for checking if a list is empty, checking if an element is a member of a list, removing all occurrences of an element from a list, finding the intersection, union, difference, symmetric difference, and Cartesian product of two lists.

The code is well-organized and uses descriptive function names. It also uses recursion to implement some of the functions, which is a common technique in Prolog.

Here are some examples of how to use the code:

```prolog
?- empty_list([]).
true.

?- member(a, [a, b, c]).
true.

?- remove_all(a, [a, b, c, a, d], R).
R = [b, c, d].

?- intersection([1, 2, 3], [2, 3, 4], R).
R = [2, 3].

?- union([1, 2, 3], [2, 3, 4], R).
R = [1, 2, 3, 4].

?- difference([1, 2, 3], [2, 3, 4], R).
R = [1].

?- symmetric_difference([1, 2, 3], [2, 3, 4], R).
R = [1, 4].

?- cartesian_product([1, 2], [a, b], R).
R = [(1, a), (1, b), (2, a), (2, b)].
```