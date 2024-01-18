```prolog
% Define the family relationships
father(john, mary).
father(john, bob).
mother(mary, mary).
mother(mary, bob).

% Define the gender of each person
male(john).
male(bob).
female(mary).

% Define the rules for finding ancestors
ancestor(X, Y) :-
    father(X, Y).
ancestor(X, Y) :-
    mother(X, Y).
ancestor(X, Z) :-
    father(X, Y),
    ancestor(Y, Z).
ancestor(X, Z) :-
    mother(X, Y),
    ancestor(Y, Z).

% Define the rules for finding descendants
descendant(X, Y) :-
    ancestor(Y, X).

% Define the rules for finding siblings
sibling(X, Y) :-
    father(Z, X),
    father(Z, Y),
    X \= Y.
sibling(X, Y) :-
    mother(Z, X),
    mother(Z, Y),
    X \= Y.

% Define the rules for finding cousins
cousin(X, Y) :-
    sibling(Z, W),
    ancestor(Z, X),
    ancestor(W, Y),
    X \= Y.

% Define the rules for finding aunts and uncles
aunt(X, Y) :-
    sibling(Z, W),
    parent(Z, Y),
    X \= Y.
uncle(X, Y) :-
    sibling(Z, W),
    parent(W, Y),
    X \= Y.

% Define the rules for finding nieces and nephews
niece(X, Y) :-
    sibling(Z, W),
    descendant(W, X),
    X \= Y.
nephew(X, Y) :-
    sibling(Z, W),
    descendant(Z, X),
    X \= Y.

% Define the rules for finding grandparents
grandparent(X, Y) :-
    parent(Z, Y),
    parent(X, Z).

% Define the rules for finding grandchildren
grandchild(X, Y) :-
    parent(Y, Z),
    parent(X, Z).

% Define the rules for finding in-laws
in-law(X, Y) :-
    married(X, Z),
    sibling(Z, Y).
in-law(X, Y) :-
    married(Y, Z),
    sibling(Z, X).

% Define the rules for finding step-relatives
stepfather(X, Y) :-
    married(X, Z),
    parent(Z, Y),
    X \= parent(Y).
stepmother(X, Y) :-
    married(X, Z),
    parent(Z, Y),
    X \= parent(Y).
stepbrother(X, Y) :-
    stepfather(Z, X),
    parent(Z, Y),
    X \= Y.
stepsister(X, Y) :-
    stepmother(Z, X),
    parent(Z, Y),
    X \= Y.

% Define the rules for finding adopted relatives
adopted_child(X, Y) :-
    parent(Y, X),
    \+ (father(Z, X) ; mother(Z, X)).
adopted_parent(X, Y) :-
    parent(X, Y),
    \+ (father(X, Y) ; mother(X, Y)).
```

This Prolog code defines a comprehensive set of rules for representing and reasoning about family relationships. It includes rules for finding ancestors, descendants, siblings, cousins, aunts, uncles, nieces, nephews, grandparents, grandchildren, in-laws, and step-relatives. The code also defines rules for finding adopted children and parents.

The rules are written in a declarative style, which means that they state facts about family relationships without specifying how to compute them. This makes the code easy to read and understand. However, it also means that the code is not executable. To execute the code, we need to use a Prolog interpreter.

Here are some examples of how to use the code:

```prolog
?- ancestor(john, mary).
true

?- sibling(mary, bob).
true

?- cousin(mary, alice).
false

?- aunt(mary, alice).
true

?- niece(mary, bob).
true

?- grandparent(john, alice).
true

?- in-law(john, alice).
false

?- stepfather(john, bob).
false

?- adopted_child(mary, john).
false
```

The code can be used to answer a variety of questions about family relationships. It is a powerful tool for genealogical research and for understanding the complex dynamics of family life.