```prolog

% Define the knowledge base
parent(john, mary).
parent(mary, bob).
parent(bob, alice).

% Define the query
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Explain the code
The first line of the code defines the parent/2 predicate, which represents the parent-child relationship between two individuals. The second line of the code defines the ancestor/2 predicate, which represents the ancestor-descendant relationship between two individuals. The third line of the code defines the query, which asks for all the ancestors of a given individual.

The ancestor/2 predicate is defined using recursion. The first clause of the predicate states that if X is the parent of Y, then X is also an ancestor of Y. The second clause of the predicate states that if X is the parent of Z, and Z is an ancestor of Y, then X is also an ancestor of Y.

To use the code, you can enter the following query:

```
?- ancestor(john, alice).
```

This query will return the following answer:

```
true
```

This means that John is an ancestor of Alice, which is correct because John is the father of Mary, who is the mother of Bob, who is the father of Alice.

```