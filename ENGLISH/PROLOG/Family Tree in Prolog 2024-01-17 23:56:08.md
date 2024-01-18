```prolog
% Define the facts for the family relationships.
parent(mary, john).
parent(mary, jane).
parent(john, susan).
parent(john, michael).
parent(jane, david).
parent(jane, sarah).

% Define the rules for finding ancestors.
ancestor(X, Y) :-
    parent(X, Y).
ancestor(X, Y) :-
    parent(X, Z),
    ancestor(Z, Y).

% Define the rules for finding descendants.
descendant(X, Y) :-
    parent(Y, X).
descendant(X, Y) :-
    parent(Z, Y),
    descendant(X, Z).

% Define the rules for finding siblings.
sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    X \= Y.

% Define the rules for finding cousins.
cousin(X, Y) :-
    parent(A, X),
    parent(B, Y),
    sibling(A, B),
    X \= Y.

% Define the rules for finding aunts and uncles.
aunt(X, Y) :-
    parent(Z, Y),
    sibling(X, Z),
    \+ parent(X, Y).

uncle(X, Y) :-
    parent(Z, Y),
    sibling(X, Z),
    \+ parent(X, Y).

% Define the rules for finding grandparents.
grandparent(X, Y) :-
    parent(X, Z),
    parent(Z, Y).

% Define the rules for finding great-grandparents.
great_grandparent(X, Y) :-
    parent(X, Z),
    grandparent(Z, Y).

% Define the rules for finding great-great-grandparents.
great_great_grandparent(X, Y) :-
    parent(X, Z),
    great_grandparent(Z, Y).

% Define the rules for finding great-great-great-grandparents.
great_great_great_grandparent(X, Y) :-
    parent(X, Z),
    great_great_grandparent(Z, Y).

% Define the rules for finding great-great-great-great-grandparents.
great_great_great_great_grandparent(X, Y) :-
    parent(X, Z),
    great_great_great_grandparent(Z, Y).

% Define the rules for finding great-great-great-great-great-grandparents.
great_great_great_great_great_grandparent(X, Y) :-
    parent(X, Z),
    great_great_great_great_grandparent(Z, Y).
```

This code is a family tree program in Prolog. It defines facts for the family relationships, and rules for finding ancestors, descendants, siblings, cousins, aunts, uncles, grandparents, great-grandparents, great-great-grandparents, great-great-great-grandparents, and great-great-great-great-great-grandparents.

To use the program, you would first consult the Prolog file containing the code. Then, you could use the following queries to find information about the family tree:

* To find the ancestors of a person, you would use the query `ancestor(X, Y)`. For example, the query `ancestor(john, susan)` would return true, since John is the ancestor of Susan.
* To find the descendants of a person, you would use the query `descendant(X, Y)`. For example, the query `descendant(mary, michael)` would return true, since Michael is the descendant of Mary.
* To find the siblings of a person, you would use the query `sibling(X, Y)`. For example, the query `sibling(john, jane)` would return true, since John and Jane are siblings.
* To find the cousins of a person, you would use the query `cousin(X, Y)`. For example, the query `cousin(susan, david)` would return true, since Susan and David are cousins.
* To find the aunts and uncles of a person, you would use the queries `aunt(X, Y)` and `uncle(X, Y)`. For example, the query `aunt(mary, susan)` would return true, since Mary is the aunt of Susan.
* To find the grandparents of a person, you would use the query `grandparent(X, Y)`. For example, the query `grandparent(john, susan)` would return true, since John is the grandfather of Susan.
* To find the great-grandparents of a person, you would use the query `great_grandparent(X, Y)`. For example, the query `great_grandparent(mary, susan)` would return true, since Mary is the great-grandmother of Susan.
* To find the great-great-grandparents of a person, you would use the query `great_great_grandparent(X, Y)`. For example, the query `great_great_grandparent(mary, susan)` would return true, since Mary is the great-great-grandmother of Susan.
* To find the great-great-great-grandparents of a person, you would use the query `great_great_great_grandparent(X, Y)`. For example, the query `great_great_great_grandparent(mary, susan)` would return true, since Mary is the great-great-great-grandmother of Susan.
* To find the great-great-great-great-grandparents of a person, you would use the query `great_great_great_great_grandparent(X, Y)`. For example, the query `great_great_great_great_grandparent(mary, susan)` would return true, since Mary is the great-great-great-great-grandmother of Susan.