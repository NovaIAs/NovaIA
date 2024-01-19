```prolog
% Define the knowledge base.

% Facts about people.
person(john).
person(mary).
person(bob).

% Facts about relationships.
parent(john, mary).
parent(john, bob).
sibling(mary, bob).

% Rules for inferring relationships.

% A person is a child of their parents.
child(X, Y) :- parent(Y, X).

% A person is a sibling of another person if they have the same parents.
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

% A person is a grandparent of another person if they are the parent of their parent.
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

% A person is an ancestor of another person if they are their parent, grandparent, or great-grandparent, etc.
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% A person is a descendant of another person if they are their child, grandchild, or great-grandchild, etc.
descendant(X, Y) :- child(X, Y).
descendant(X, Y) :- child(X, Z), descendant(Z, Y).

% Queries.

% Find the parents of a person.
?- parent(X, mary).
X = john.

% Find the siblings of a person.
?- sibling(X, mary).
X = bob.

% Find the children of a person.
?- child(X, john).
X = mary.
X = bob.

% Find the grandchildren of a person.
?- grandparent(X, bob).
X = john.

% Find the ancestors of a person.
?- ancestor(X, bob).
X = john.

% Find the descendants of a person.
?- descendant(X, john).
X = mary.
X = bob.
```

This Prolog code defines a knowledge base about people and their relationships. It includes facts about who is a person, who is the parent of whom, and who is the sibling of whom. It also includes rules for inferring relationships, such as who is a child of someone, who is a sibling of someone, who is a grandparent of someone, and so on.

The code can be used to answer queries about relationships between people. For example, the query ?- parent(X, mary) will find the parents of Mary, which is John. The query ?- sibling(X, mary) will find the siblings of Mary, which is Bob.

The code can also be used to infer relationships between people. For example, the query ?- ancestor(X, bob) will find the ancestors of Bob, which is John. The query ?- descendant(X, john) will find the descendants of John, which is Mary and Bob.

This code is a good example of how Prolog can be used to represent and reason about relationships between objects. It is a powerful tool for knowledge representation and reasoning.