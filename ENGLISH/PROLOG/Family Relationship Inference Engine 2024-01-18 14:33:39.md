```prolog
% Define a knowledge base of facts and rules.

% Facts about people and their relationships.
person(john).
person(mary).
person(bob).
parent(john, mary).
parent(john, bob).
parent(mary, bob).

% Rules for inferring new facts from existing ones.

% A person is an ancestor of another person if they are their parent or
% the ancestor of their parent.
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% A person is a descendant of another person if they are their child or
% the descendant of their child.
descendant(X, Y) :- parent(Y, X).
descendant(X, Y) :- parent(Z, X), descendant(Z, Y).

% A person is a sibling of another person if they have the same parent.
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

% A person is a cousin of another person if they share a grandparent.
cousin(X, Y) :- parent(A, X), parent(B, Y), sibling(A, B), A \= B.

% A person is a relative of another person if they are an ancestor,
% descendant, sibling, or cousin of that person.
relative(X, Y) :- ancestor(X, Y).
relative(X, Y) :- descendant(X, Y).
relative(X, Y) :- sibling(X, Y).
relative(X, Y) :- cousin(X, Y).

% Queries to demonstrate the knowledge base.

% Find all of John's relatives.
?- relative(john, X).

% Find all of Mary's ancestors.
?- ancestor(X, mary).

% Find all of Bob's descendants.
?- descendant(X, bob).

% Find all of John's siblings.
?- sibling(john, X).

% Find all of Mary's cousins.
?- cousin(mary, X).
```

Explanation:

This Prolog code defines a knowledge base of facts and rules about people and their relationships. It includes facts about who is a person, who is the parent of whom, and rules for inferring new facts such as who is an ancestor, descendant, sibling, or cousin of whom. The code also includes queries to demonstrate the knowledge base, such as finding all of John's relatives, all of Mary's ancestors, and all of Bob's descendants.

Here is a detailed explanation of the code:

* The first section of the code defines facts about people and their relationships. The `person` predicate is used to assert that someone is a person. The `parent` predicate is used to assert that someone is the parent of someone else. For example, the fact `parent(john, mary)` means that John is the parent of Mary.
* The second section of the code defines rules for inferring new facts from existing ones. The `ancestor` predicate is used to infer that someone is an ancestor of someone else. The rule `ancestor(X, Y) :- parent(X, Y)` says that if X is the parent of Y, then X is an ancestor of Y. The rule `ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y)` says that if X is the parent of Z and Z is an ancestor of Y, then X is an ancestor of Y.
* The `descendant` predicate is used to infer that someone is a descendant of someone else. The rule `descendant(X, Y) :- parent(Y, X)` says that if Y is the parent of X, then X is a descendant of Y. The rule `descendant(X, Y) :- parent(Z, X), descendant(Z, Y)` says that if Z is the parent of X and Z is a descendant of Y, then X is a descendant of Y.
* The `sibling` predicate is used to infer that someone is a sibling of someone else. The rule `sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y` says that if Z is the parent of X and Z is the parent of Y, and X is not equal to Y, then X is a sibling of Y.
* The `cousin` predicate is used to infer that someone is a cousin of someone else. The rule `cousin(X, Y) :- parent(A, X), parent(B, Y), sibling(A, B), A \= B` says that if A is the parent of X, B is the parent of Y, A is a sibling of B, and A is not equal to B, then X is a cousin of Y.
* The `relative` predicate is used to infer that someone is a relative of someone else. The rule `relative(X, Y) :- ancestor(X, Y)` says that if X is an ancestor of Y, then X is a relative of Y. The rule `relative(X, Y) :- descendant(X, Y)` says that if X is a descendant of Y, then X is a relative of Y. The rule `relative(X, Y) :- sibling(X, Y)` says that if X is a sibling of Y, then X is a relative of Y. The rule `relative(X, Y) :- cousin(X, Y)` says that if X is a cousin of Y, then X is a relative of Y.
* The last section of the code includes queries to demonstrate the knowledge base. The query `?- relative(john, X).` asks the Prolog interpreter to find all of John's relatives. The query `?- ancestor(X, mary).` asks the Prolog interpreter to find all of Mary's ancestors. The query `?- descendant(X, bob).` asks the Prolog interpreter to find all of Bob's descendants. The query `?- sibling(john, X).` asks the Prolog interpreter to find all of John's siblings. The query `?- cousin(mary, X).` asks the Prolog interpreter to find all of Mary's cousins.

This code is a complex and differentiated Prolog program that demonstrates the power of Prolog for representing and reasoning about relationships between people.