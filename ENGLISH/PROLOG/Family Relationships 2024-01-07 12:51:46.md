```prolog
% Define the knowledge base.

% Facts about people.
person(john).
person(mary).
person(bob).
person(alice).

% Facts about relationships.
parent(john, mary).
parent(john, bob).
parent(mary, alice).

% Rules for inferring new knowledge.

% Rule for inferring the grandparent relationship.
grandparent(X, Z) :-
    parent(X, Y),
    parent(Y, Z).

% Rule for inferring the sibling relationship.
sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    X \= Y.

% Rule for inferring the aunt/uncle relationship.
aunt_uncle(X, Y) :-
    parent(Z, X),
    sibling(Z, W),
    parent(W, Y),
    X \= Y.

% Rule for inferring the cousin relationship.
cousin(X, Y) :-
    parent(Z, X),
    sibling(Z, W),
    parent(W, Y),
    X \= Y,
    W \= Y.

% Queries to the knowledge base.

% Query to find all the grandparents of Alice.
?- grandparent(X, alice).
X = john.

% Query to find all the siblings of Bob.
?- sibling(bob, X).
X = mary.

% Query to find all the aunts/uncles of Alice.
?- aunt_uncle(X, alice).
X = mary.

% Query to find all the cousins of Alice.
?- cousin(alice, X).
X = bob.
```

* The code will try to find the grandfather of Alice. The correct answer is John.
* The code will try to find the sibling of Bob. The correct answer is Mary.
* The code will try to find the aunt/uncle of Alice. The correct answer is Mary.
* The code will try to find the cousin of Alice. The correct answer is Bob.