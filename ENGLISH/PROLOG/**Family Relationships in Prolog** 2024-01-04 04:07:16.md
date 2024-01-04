```prolog
% Define a knowledge base of facts and rules.

% Facts about the world.
person(john).
person(mary).
person(bob).

parent(john, mary).
parent(john, bob).
parent(mary, alice).
parent(bob, carol).

male(john).
male(bob).
female(mary).
female(alice).
female(carol).

% Rules for inferring new knowledge.

% A person is a grandparent if they have a child who is a parent.
grandparent(X, Y) :-
    parent(X, Z),
    parent(Z, Y).

% A person is a sibling if they share a parent.
sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    X \= Y.

% A person is an ancestor of another person if they are their parent,
% grandparent, great-grandparent, and so on.
ancestor(X, Y) :-
    parent(X, Y).
ancestor(X, Y) :-
    parent(X, Z),
    ancestor(Z, Y).

% A person is a descendant of another person if they are their child,
% grandchild, great-grandchild, and so on.
descendant(X, Y) :-
    parent(Y, X).
descendant(X, Y) :-
    parent(Z, X),
    descendant(Z, Y).

% A person is related to another person if they are their ancestor,
% descendant, sibling, or spouse.
related(X, Y) :-
    ancestor(X, Y).
related(X, Y) :-
    descendant(X, Y).
related(X, Y) :-
    sibling(X, Y).
related(X, Y) :-
    spouse(X, Y).

% A person is a couple if they are married to each other.
couple(X, Y) :-
    spouse(X, Y),
    spouse(Y, X).

% A person is a family member if they are related to another person
% by blood or marriage.
family_member(X, Y) :-
    related(X, Y).
family_member(X, Y) :-
    spouse(X, Z),
    related(Z, Y).

% A person is a relative if they are related to another person by blood.
relative(X, Y) :-
    ancestor(X, Y).
relative(X, Y) :-
    descendant(X, Y).
relative(X, Y) :-
    sibling(X, Y).

% A person is an in-law if they are married to a relative of another person.
in_law(X, Y) :-
    spouse(X, Z),
    relative(Z, Y).

% Define a query to find all the relatives of a person.

relatives(Person, Relatives) :-
    setof(Relative, related(Person, Relative), Relatives).

% Define a query to find all the family members of a person.

family_members(Person, FamilyMembers) :-
    setof(FamilyMember, family_member(Person, FamilyMember), FamilyMembers).

% Define a query to find all the in-laws of a person.

in_laws(Person, InLaws) :-
    setof(InLaw, in_law(Person, InLaw), InLaws).
```

This code defines a knowledge base of facts and rules about people and their relationships. The facts include information about who is a person, who is a parent of whom, and who is male or female. The rules include definitions of grandparent, sibling, ancestor, descendant, related, couple, family member, relative, and in-law.

The queries at the end of the code can be used to find all the relatives, family members, or in-laws of a person. For example, the following query would find all the relatives of John:

```prolog
?- relatives(john, Relatives).
```

This query would return the following result:

```prolog
Relatives = [mary, bob, alice, carol]
```

This means that John's relatives are Mary, Bob, Alice, and Carol.