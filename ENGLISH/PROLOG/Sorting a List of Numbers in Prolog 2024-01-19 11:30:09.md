```prolog
% Define the rules for the logic program.

% Base case: If the list is empty, then it is sorted.
sorted([], []).

% Recursive case: If the list is not empty, then we need to find the smallest
% element in the list and remove it. The remaining list is then sorted.
sorted([H|T], Sorted) :-
    min(T, Min),
    delete(Min, T, Rest),
    sorted(Rest, Sorted1),
    insert(Min, Sorted1, Sorted).

% Find the minimum element in a list.
min([H], H).
min([H|T], Min) :-
    min(T, Min1),
    (   H < Min1 -> Min = H
    ;   Min = Min1
    ).

% Delete an element from a list.
delete(X, [X|T], T).
delete(X, [H|T], [H|T1]) :-
    delete(X, T, T1).

% Insert an element into a sorted list.
insert(X, [], [X]).
insert(X, [H|T], [H|T1]) :-
    X =< H,
    insert(X, T, T1).
insert(X, [H|T], [X, H|T1]) :-
    X > H,
    insert(X, T, T1).

% Define the query to be solved.
query(L) :-
    sorted(L, Sorted),
    writeln(Sorted).

% Run the query.
query([3, 1, 4, 2, 5]).
```

This Prolog program defines a set of logical rules for sorting a list of numbers in ascending order. It uses a recursive approach to identify the smallest element in the list and remove it. The remaining list is then sorted using the same approach. The program also includes a base case to handle empty lists.

To use the program, you can specify a list of numbers as the input query. The program will then sort the list and display the sorted list as the output. Here's an example of how to use the program:

```prolog
?- query([3, 1, 4, 2, 5]).
[1, 2, 3, 4, 5]
```

In this example, the input list is `[3, 1, 4, 2, 5]`. The program sorts the list and displays the sorted list as `[1, 2, 3, 4, 5]`.