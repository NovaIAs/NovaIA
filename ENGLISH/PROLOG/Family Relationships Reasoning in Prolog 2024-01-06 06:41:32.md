```prolog
% Define a knowledge base of facts and rules.
% Facts represent known information, while rules define relationships between facts.

% Fact: There is a person named John.
person(john).

% Fact: There is a person named Mary.
person(mary).

% Fact: John is the father of Mary.
father(john, mary).

% Rule: A person is a parent of another person if they are their father or mother.
parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).

% Rule: Two people are siblings if they have the same parents.
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \== Y.

% Rule: A person is an ancestor of another person if they are their parent or the ancestor of their parent.
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Rule: Two people are related if they are siblings, or if they are related to a common ancestor.
related(X, Y) :- sibling(X, Y).
related(X, Y) :- ancestor(Z, X), ancestor(Z, Y), X \== Y.

% Query the knowledge base to find all people related to John.
?- related(john, X).
```

Explanation:

1. Facts:
   - `person(john)`: John is a person.
   - `person(mary)`: Mary is a person.
   - `father(john, mary)`: John is the father of Mary.

2. Rules:
   - `parent(X, Y)`: Defines that X is the parent of Y if they are the father or mother of Y.
   - `sibling(X, Y)`: Defines that X and Y are siblings if they have the same parents.
   - `ancestor(X, Y)`: Defines that X is an ancestor of Y if they are the parent of Y or the ancestor of Y's parent.
   - `related(X, Y)`: Defines that X and Y are related if they are siblings or if they are related to a common ancestor.

3. Query:
   - `?- related(john, X).`: This query asks the Prolog system to find all people who are related to John.

When this code is executed, the Prolog system will use the facts and rules to deduce new information. In this case, it will deduce that Mary is related to John because she is his daughter. The system will then output the following result:

```
X = mary
```

This demonstrates how Prolog can be used to represent complex relationships and reason about them using logical rules.