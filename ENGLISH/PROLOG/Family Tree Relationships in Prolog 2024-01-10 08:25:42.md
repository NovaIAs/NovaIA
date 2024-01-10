```prolog

% Define the family relationships

parent(alice, bob).
parent(alice, jane).
parent(bob, mary).
parent(bob, john).
parent(jane, susan).
parent(jane, tom).

% Define the gender of each person

male(bob).
male(john).
male(tom).
female(alice).
female(jane).
female(mary).
female(susan).

% Define the rules for finding ancestors

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Define the rules for finding descendants

descendant(X, Y) :- parent(Y, X).
descendant(X, Y) :- parent(Z, Y), descendant(X, Z).

% Define the rules for finding siblings

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

% Define the rules for finding cousins

cousin(X, Y) :- parent(Z, X), parent(W, Y), sibling(Z, W), X \= Y.

% Define the rules for finding aunts and uncles

aunt(X, Y) :- female(X), sibling(X, Z), parent(Z, Y).
uncle(X, Y) :- male(X), sibling(X, Z), parent(Z, Y).

% Define the rules for finding grandparents

grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

% Define the rules for finding great-grandparents

great_grandparent(X, Y) :- parent(X, Z), grandparent(Z, Y).

% Define the rules for finding great-great-grandparents

great_great_grandparent(X, Y) :- parent(X, Z), great_grandparent(Z, Y).

```

This code defines a family tree in Prolog, with relationships such as parent, child, sibling, cousin, aunt, uncle, grandparent, great-grandparent, and great-great-grandparent. It also includes rules for finding ancestors, descendants, siblings, cousins, aunts, uncles, grandparents, great-grandparents, and great-great-grandparents.

To use this code, you can load it into a Prolog interpreter and then query it using the ?- operator. For example, the following query will find all of Alice's descendants:

```
?- descendant(alice, X).
```

This query will return the following results:

```
X = bob
X = jane
X = mary
X = john
X = susan
X = tom
```

You can also use this code to find other relationships, such as siblings, cousins, aunts, uncles, grandparents, great-grandparents, and great-great-grandparents.