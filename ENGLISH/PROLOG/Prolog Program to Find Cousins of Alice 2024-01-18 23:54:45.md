```
% Define the knowledge base of facts and rules.

% Facts:
mother(mary, alice).
mother(mary, bob).
father(john, alice).
father(john, bob).

% Rules:
parent(X, Y) :- mother(X, Y).
parent(X, Y) :- father(X, Y).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

cousin(X, Y) :- parent(A, X), parent(B, Y), sibling(A, B).

% Define the query to find all cousins of Alice.

cousins_of_alice(Cousins) :-
    findall(Cousin, cousin(Cousin, alice), Cousins).

% Execute the query and print the results.

cousins_of_alice(Cousins),
forall(member(Cousin, Cousins), writeln(Cousin)).

```

Explanation:

The code written in Prolog is designed to find all the cousins of Alice. It utilizes a set of facts and rules to establish relationships between individuals.

1. Facts:
   - The facts provided in the code specify the parent-child relationships. For instance, "mother(mary, alice)" indicates that Mary is the mother of Alice.

2. Rules:
   - Parent Rule: Defines that if X is the mother or father of Y, then X is a parent of Y.

   - Ancestor Rule: Defines that if X is a parent of Y, or X is a parent of Z and Z is an ancestor of Y, then X is an ancestor of Y. This establishes the concept of grandparent, great-grandparent, and so on.

   - Sibling Rule: Defines that if Z is a parent of both X and Y, and X and Y are distinct individuals, then X and Y are siblings.

   - Cousin Rule: Defines that if X and Y have parents A and B respectively, and A and B are siblings, then X and Y are cousins.

3. Query:
   - The main query, "cousins_of_alice(Cousins)," searches for all individuals who are cousins of Alice and assigns the results to the variable Cousins.

4. Execution:
   - The query is executed, and if successful, it returns a list of all cousins of Alice.

   - The code uses the "forall" construct to iterate over the list and print each cousin's name.

By using facts and rules to represent familial relationships and applying logical reasoning through queries, this Prolog code provides a sophisticated way to explore relationships within a family tree, in this case finding all the cousins of Alice.