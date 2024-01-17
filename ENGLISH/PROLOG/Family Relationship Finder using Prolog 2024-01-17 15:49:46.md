```prolog
% Define the family tree
parent(john, mary).
parent(john, bob).
parent(mary, alice).
parent(mary, tom).
parent(bob, charlie).
parent(bob, david).
parent(alice, eve).
parent(alice, frank).
parent(tom, george).
parent(tom, harry).

% Define the gender of each person
male(john).
male(bob).
male(charlie).
male(david).
male(tom).
male(george).
male(harry).
female(mary).
female(alice).
female(eve).
female(frank).

% Define the relationship between two people
sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    X \== Y.

aunt(X, Y) :-
    parent(Z, X),
    sibling(Z, W),
    parent(W, Y),
    female(X).

uncle(X, Y) :-
    parent(Z, X),
    sibling(Z, W),
    parent(W, Y),
    male(X).

cousin(X, Y) :-
    parent(Z, X),
    sibling(Z, W),
    parent(W, Y),
    X \== Y.

% Define the query to find all the cousins of a person
all_cousins(X, Cousins) :-
    findall(Y, cousin(X, Y), Cousins).

% Define the query to find all the aunts of a person
all_aunts(X, Aunts) :-
    findall(Y, aunt(X, Y), Aunts).

% Define the query to find all the uncles of a person
all_uncles(X, Uncles) :-
    findall(Y, uncle(X, Y), Uncles).

% Define the query to find all the siblings of a person
all_siblings(X, Siblings) :-
    findall(Y, sibling(X, Y), Siblings).
```

This Prolog code defines a family tree and allows you to query it to find relationships between people. The code defines the parent-child relationship, the gender of each person, and the sibling relationship. Additionally, it defines the aunt/uncle, cousin, and sibling relationships based on the parent-child and sibling relationships.

The code includes three queries:

1. `all_cousins(X, Cousins)`: Finds all the cousins of the person `X`.
2. `all_aunts(X, Aunts)`: Finds all the aunts of the person `X`.
3. `all_uncles(X, Uncles)`: Finds all the uncles of the person `X`.

To use the code, you can consult the Prolog file that contains the code and then use the queries to find the desired relationships. For example, to find all the cousins of `alice`, you would use the query:

```prolog
?- all_cousins(alice, Cousins).
```

This would return a list of all the cousins of `alice`.

This code can be used to model complex family relationships and to answer queries about those relationships. It is a powerful tool that can be used for a variety of purposes, such as genealogy or social network analysis.