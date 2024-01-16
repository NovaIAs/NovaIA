```prolog
% Define a knowledge base of facts and rules about family relationships.

parent(john, mary).
parent(john, bob).
parent(mary, alice).
parent(bob, charlie).
parent(alice, dave).

male(john).
male(bob).
male(charlie).
male(dave).

female(mary).
female(alice).

% Define a rule to determine if someone is a grandparent of another person.

grandparent(X, Y) :-
    parent(X, Z),
    parent(Z, Y).

% Define a rule to determine if someone is a sibling of another person.

sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    X \= Y.

% Define a rule to determine if someone is an ancestor of another person.

ancestor(X, Y) :-
    parent(X, Y).
ancestor(X, Y) :-
    parent(X, Z),
    ancestor(Z, Y).

% Define a rule to determine if someone is a descendant of another person.

descendant(X, Y) :-
    parent(Y, X).
descendant(X, Y) :-
    parent(Z, X),
    descendant(Z, Y).

% Define a rule to determine if someone is a cousin of another person.

cousin(X, Y) :-
    parent(A, X),
    parent(B, Y),
    sibling(A, B),
    X \= Y.

% Define a rule to determine if someone is a relative of another person.

relative(X, Y) :-
    ancestor(X, Y).
relative(X, Y) :-
    descendant(X, Y).
relative(X, Y) :-
    sibling(X, Y).
relative(X, Y) :-
    cousin(X, Y).

% Define a query to find all the grandparents of a person.

grandparents_of(Person) :-
    findall(Grandparent, grandparent(Grandparent, Person), Grandparents),
    write(Grandparents).

% Define a query to find all the siblings of a person.

siblings_of(Person) :-
    findall(Sibling, sibling(Sibling, Person), Siblings),
    write(Siblings).

% Define a query to find all the ancestors of a person.

ancestors_of(Person) :-
    findall(Ancestor, ancestor(Ancestor, Person), Ancestors),
    write(Ancestors).

% Define a query to find all the descendants of a person.

descendants_of(Person) :-
    findall(Descendant, descendant(Descendant, Person), Descendants),
    write(Descendants).

% Define a query to find all the cousins of a person.

cousins_of(Person) :-
    findall(Cousin, cousin(Cousin, Person), Cousins),
    write(Cousins).

% Define a query to find all the relatives of a person.

relatives_of(Person) :-
    findall(Relative, relative(Relative, Person), Relatives),
    write(Relatives).
```

This code defines a knowledge base of facts and rules about family relationships. It includes facts about who is the parent of whom, as well as rules to determine if someone is a grandparent, sibling, ancestor, descendant, cousin, or relative of another person. The code also includes queries to find all the grandparents, siblings, ancestors, descendants, cousins, and relatives of a person.

To use the code, you can consult the Prolog file containing the code and then use the queries to find the information you want. For example, to find all the grandparents of John, you would use the following query:

```prolog
grandparents_of(john).
```

This would return the following result:

```prolog
[mary, bob]
```

This means that John's grandparents are Mary and Bob.