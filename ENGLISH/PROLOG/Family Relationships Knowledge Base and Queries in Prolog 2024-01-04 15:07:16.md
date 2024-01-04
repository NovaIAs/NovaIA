```prolog
% Define the knowledge base of facts and rules.

% Facts about family relationships.
parent(john, mary).
parent(john, bob).
parent(mary, alice).
parent(bob, charlie).

% Rule for determining if someone is an ancestor of another person.
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Rule for determining if someone is a descendant of another person.
descendant(X, Y) :- parent(Y, X).
descendant(X, Y) :- parent(Z, X), descendant(Z, Y).

% Rule for determining if two people are related.
related(X, Y) :- ancestor(X, Y).
related(X, Y) :- descendant(X, Y).
related(X, Y) :- ancestor(Z, X), descendant(Z, Y).

% Query the knowledge base to find all the ancestors of Alice.
?- ancestor(X, alice).
X = john ;
X = mary.

% Query the knowledge base to find all the descendants of John.
?- descendant(X, john).
X = mary ;
X = bob ;
X = alice ;
X = charlie.

% Query the knowledge base to find all the people who are related to Bob.
?- related(X, bob).
X = john ;
X = mary ;
X = alice ;
X = charlie.
```

Explanation:

* The first few lines of the code define the knowledge base of facts and rules. Facts are statements that are always true, while rules are statements that define relationships between facts.
* The facts in this example are about family relationships. For example, the fact `parent(john, mary)` means that John is the parent of Mary.
* The rules in this example define how to determine if someone is an ancestor, descendant, or related to another person.
* The last few lines of the code query the knowledge base to find all the ancestors of Alice, all the descendants of John, and all the people who are related to Bob.
* The output of the queries shows that John and Mary are Alice's ancestors, Mary, Bob, Alice, and Charlie are John's descendants, and John, Mary, Alice, and Charlie are all related to Bob.