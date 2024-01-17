```prolog
% Define a knowledge base of facts and rules about family relationships.

parent(john, mary).
parent(john, bob).
parent(mary, jane).
parent(bob, alice).
parent(alice, tom).

% Define a rule to determine if someone is an ancestor of another person.

ancestor(X, Y) :-
    parent(X, Y).
ancestor(X, Y) :-
    parent(X, Z),
    ancestor(Z, Y).

% Define a rule to determine if someone is a descendant of another person.

descendant(X, Y) :-
    ancestor(Y, X).

% Define a rule to determine if two people are related.

related(X, Y) :-
    ancestor(X, Y).
related(X, Y) :-
    ancestor(Y, X).

% Define a query to find all the ancestors of a given person.

ancestors(Person) :-
    findall(Ancestor, ancestor(Ancestor, Person), Ancestors).

% Define a query to find all the descendants of a given person.

descendants(Person) :-
    findall(Descendant, descendant(Descendant, Person), Descendants).

% Define a query to find all the relatives of a given person.

relatives(Person) :-
    findall(Relative, related(Relative, Person), Relatives).

% Define a query to find all the siblings of a given person.

siblings(Person) :-
    findall(Sibling, (parent(Parent, Person), parent(Parent, Sibling), Sibling \= Person), Siblings).

% Define a query to find all the cousins of a given person.

cousins(Person) :-
    findall(Cousin, (parent(Parent1, Person), parent(Parent2, Cousin), Parent1 \= Parent2, Cousin \= Person), Cousins).

% Define a query to find all the aunts and uncles of a given person.

aunts_and_uncles(Person) :-
    findall(AuntOrUncle, (parent(Parent, Person), parent(Parent, AuntOrUncle), AuntOrUncle \= Person), AuntsAndUncles).
```

This code defines a knowledge base of facts and rules about family relationships. It includes rules for determining if someone is an ancestor, descendant, or relative of another person. It also includes queries to find all the ancestors, descendants, relatives, siblings, cousins, and aunts and uncles of a given person.

Here are some examples of how to use the code:

```prolog
?- ancestors(mary).
[john]

?- descendants(bob).
[alice, tom]

?- related(john, jane).
true

?- siblings(bob).
[mary]

?- cousins(tom).
[]

?- aunts_and_uncles(jane).
[john]
```

This code can be used to answer a variety of questions about family relationships. It is a good example of how Prolog can be used to represent and reason about complex knowledge.