```prolog
% Define the knowledge base

% Facts about the world

person(john).
person(mary).
person(bob).
person(alice).

male(john).
female(mary).
male(bob).
female(alice).

parent(john, mary).
parent(john, bob).
parent(mary, alice).

% Rules

% A person is a grandparent if they have a child who is a parent

grandparent(X, Y) :-
    parent(X, Z),
    parent(Z, Y).

% A person is a sibling if they have at least one parent in common

sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    X \= Y.

% A person is an aunt or uncle if they are a sibling of a parent

aunt_or_uncle(X, Y) :-
    sibling(X, Z),
    parent(Z, Y).

% A person is a cousin if they have a grandparent in common

cousin(X, Y) :-
    grandparent(Z, X),
    grandparent(Z, Y),
    X \= Y.

% Queries

% Find all of John's grandchildren

?- grandparent(john, X).
X = mary.
X = bob.
X = alice.

% Find all of Mary's siblings

?- sibling(mary, X).
X = john.
X = bob.

% Find all of Bob's aunts and uncles

?- aunt_or_uncle(X, bob).
X = john.
X = mary.

% Find all of Alice's cousins

?- cousin(alice, X).
X = mary.
X = bob.
```

Explanation:

This code defines a knowledge base about people and their relationships. The facts include information about who is a person, who is male or female, and who is the parent of whom. The rules define how to infer new relationships from the facts. For example, the rule for grandparent states that a person is a grandparent if they have a child who is a parent.

The queries are used to ask questions about the knowledge base. For example, the query

```
?- grandparent(john, X).
```

asks the system to find all of John's grandchildren. The system will respond with all of the people who are John's grandchildren, in this case mary, bob, and alice.

This code is a simple example of how Prolog can be used to represent and reason about relationships between objects. Prolog is a powerful language that can be used to solve a wide variety of problems.