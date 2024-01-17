```prolog
% Define a knowledge base of facts and rules.

% Facts about people and their relationships.
person(john).
person(mary).
person(bob).
person(alice).

parent(john, mary).
parent(mary, bob).
parent(bob, alice).

% Rules for determining if someone is an ancestor of another person.

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Rules for determining if someone is a descendant of another person.

descendant(X, Y) :- parent(Y, X).
descendant(X, Y) :- parent(Z, X), descendant(Z, Y).

% Rules for determining if two people are related.

related(X, Y) :- ancestor(X, Y).
related(X, Y) :- descendant(X, Y).
related(X, Y) :- ancestor(Z, X), related(Z, Y).

% Query the knowledge base to find all people who are related to John.

?- findall(X, related(john, X), Relatives).
```

**Explanation:**

This Prolog code defines a knowledge base of facts and rules about people and their relationships. The facts include information about which people are parents of other people. The rules define how to determine if someone is an ancestor or descendant of another person, and how to determine if two people are related.

The query at the end of the code asks Prolog to find all people who are related to John. Prolog uses the rules and facts in the knowledge base to find all people who are related to John, either as ancestors, descendants, or other relatives. The results of the query are stored in the variable `Relatives`.

This code demonstrates how Prolog can be used to represent and reason about complex relationships between objects. It is a good example of how Prolog can be used to solve problems that require logical reasoning and inference.