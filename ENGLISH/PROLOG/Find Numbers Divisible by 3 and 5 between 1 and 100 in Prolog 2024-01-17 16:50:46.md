```prolog

% Define the query to be solved.

?- findall(X, (between(1, 100, X), X mod 3 = 0, X mod 5 = 0), Result).

% Define a helper predicate to check if a number is divisible by 3 and 5.

divisible_by_3_and_5(X) :-
    X mod 3 = 0,
    X mod 5 = 0.

% Define the main predicate to generate a list of numbers divisible by 3 and 5.

findall(X, Goal, Result) :-
    findall(X, Goal, Temp),
    sort(Temp, Result).

% Define the built-in predicate between/3 to generate a range of numbers.

between(Start, End, X) :-
    Start =< X,
    X =< End.
```

Explanation:

1. Query: The query ?- findall(X, (between(1, 100, X), X mod 3 = 0, X mod 5 = 0), Result). defines the task to find all integer values of X between 1 and 100 that are divisible by both 3 and 5. The result of the query is assigned to the variable Result.

2. Helper Predicate: The helper predicate divisible_by_3_and_5(X) is defined to check if a given integer X is divisible by both 3 and 5. It uses the modulo operator mod to check these conditions. If both conditions are true, the predicate succeeds.

3. Main Predicate: The main predicate findall(X, Goal, Result) is used to generate a list of values X that satisfy the Goal condition. The Goal in this case is the conjunctive goal between(1, 100, X), X mod 3 = 0, X mod 5 = 0, which specifies that X should be between 1 and 100 and divisible by both 3 and 5.

4. findall Implementation: The findall predicate uses a temporary list Temp to collect the values of X that satisfy the Goal condition. It first calls the Goal with X as the variable to be instantiated. For each successful instantiation of X, it adds X to the temporary list Temp. After all solutions have been found, the predicate sorts the temporary list Temp in ascending order using the built-in predicate sort/2, and assigns the sorted result to the Result variable.

5. Built-in Predicates: The built-in predicate between/3 is used to generate a range of numbers between Start and End, inclusive. The built-in predicate sort/2 is used to sort a list in ascending order.

When this program is run, it will generate a list of all the numbers between 1 and 100 that are divisible by both 3 and 5. The result will be assigned to the variable Result, which can then be printed or further processed as needed.