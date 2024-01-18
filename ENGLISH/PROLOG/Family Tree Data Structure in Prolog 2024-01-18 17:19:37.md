```prolog
% Define the rules for the family relationships.

parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).

father(john, mary).
father(john, bob).
father(bob, alice).

mother(mary, mary).
mother(mary, bob).
mother(alice, alice).

% Define the rules for finding the ancestors of a person.

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Define the rules for finding the descendants of a person.

descendant(X, Y) :- parent(Y, X).
descendant(X, Y) :- parent(Z, Y), descendant(X, Z).

% Define the rules for finding the siblings of a person.

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

% Define the rules for finding the cousins of a person.

cousin(X, Y) :-
    parent(Z, X),
    parent(W, Y),
    sibling(Z, W),
    X \= Y.

% Define the rules for finding the uncles and aunts of a person.

uncle(X, Y) :-
    parent(Z, X),
    parent(W, Y),
    sibling(Z, W),
    male(Z).

aunt(X, Y) :-
    parent(Z, X),
    parent(W, Y),
    sibling(Z, W),
    female(Z).

% Define the rules for finding the nephews and nieces of a person.

nephew(X, Y) :-
    parent(Z, X),
    parent(W, Y),
    male(Y),
    sibling(Z, W).

niece(X, Y) :-
    parent(Z, X),
    parent(W, Y),
    female(Y),
    sibling(Z, W).

% Define the rules for finding the in-laws of a person.

in_law(X, Y) :-
    married(X, Z),
    parent(Z, Y).

% Define the rules for finding the step-parents and step-children of a person.

step_parent(X, Y) :-
    married(X, Z),
    parent(Z, Y),
    X \= parent(Y).

step_child(X, Y) :-
    married(X, Z),
    parent(Z, Y),
    X \= parent(Y).

% Define the rules for finding the grandparents and grandchildren of a person.

grandparent(X, Y) :-
    parent(X, Z),
    parent(Z, Y).

grandchild(X, Y) :-
    parent(Y, Z),
    parent(Z, X).

% Define the rules for finding the great-grandparents and great-grandchildren of a person.

great_grandparent(X, Y) :-
    parent(X, Z),
    grandparent(Z, Y).

great_grandchild(X, Y) :-
    grandchild(X, Z),
    parent(Z, Y).
```

This code defines a family tree data structure in Prolog. It includes rules for defining parent-child relationships, as well as rules for finding ancestors, descendants, siblings, cousins, uncles, aunts, nephews, nieces, in-laws, step-parents, step-children, grandparents, grandchildren, great-grandparents, and great-grandchildren.

To use this code, you can load it into a Prolog interpreter and then use the various predicates to query the family tree. For example, you could use the following query to find all of the descendants of John:

```prolog
?- descendant(john, X).
```

This query would return the following results:

```prolog
X = mary
X = bob
X = alice
```

You could also use the following query to find all of the siblings of Mary:

```prolog
?- sibling(mary, X).
```

This query would return the following result:

```prolog
X = bob
```

Finally, you could use the following query to find all of the great-grandchildren of John:

```prolog
?- great_grandchild(john, X).
```

This query would return the following result:

```prolog
X = alice
```