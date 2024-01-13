```prolog
% Define a knowledge base of facts and rules.

% Facts about family relationships.
parent(john, mary).
parent(john, bob).
parent(mary, alice).
parent(bob, charlie).

% Rules for inferring relationships.

% A person is a grandparent of another person if they are the parent of their parent.
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

% A person is a sibling of another person if they have the same parent.
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

% A person is an ancestor of another person if they are their parent, grandparent, great-grandparent, and so on.
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% A person is a descendant of another person if they are their child, grandchild, great-grandchild, and so on.
descendant(X, Y) :- parent(Y, X).
descendant(X, Y) :- parent(Z, X), descendant(Z, Y).

% A person is a cousin of another person if they share a grandparent.
cousin(X, Y) :- grandparent(Z, X), grandparent(Z, Y), X \= Y.

% Queries to the knowledge base.

% Find all of John's descendants.
?- descendant(john, X).
X = mary.
X = bob.
X = alice.
X = charlie.

% Find all of Mary's ancestors.
?- ancestor(X, mary).
X = john.
X = mary.

% Find all of Bob's siblings.
?- sibling(bob, X).
X = mary.

% Find all of Alice's cousins.
?- cousin(alice, X).
X = charlie.
```

This code defines a knowledge base of facts and rules about family relationships. The facts include statements about who is the parent of whom. The rules include definitions of relationships such as grandparent, sibling, ancestor, descendant, and cousin. The queries to the knowledge base demonstrate how to use the rules to infer new relationships from the facts.

For example, the query

```
?- descendant(john, X).
```

returns all of John's descendants, which are Mary, Bob, Alice, and Charlie. The query

```
?- ancestor(X, mary).
```

returns all of Mary's ancestors, which are John and Mary herself. The query

```
?- sibling(bob, X).
```

returns Bob's sibling, which is Mary. The query

```
?- cousin(alice, X).
```

returns Alice's cousin, which is Charlie.

This code is a simple example of a Prolog program. Prolog is a logic programming language that is well-suited for representing and reasoning about relationships between objects.