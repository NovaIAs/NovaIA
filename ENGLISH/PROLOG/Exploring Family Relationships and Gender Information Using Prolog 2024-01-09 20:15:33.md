```prolog
% Define the knowledge base
father(john, mary).
father(john, bob).
mother(mary, mary).
mother(mary, bob).
male(john).
male(bob).
female(mary).

% Define the rules
parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).

% Define the query
?- parent(X, bob).

% Explanation:
% The first line defines the relationship between John and Mary as father and daughter.
% The second line defines the relationship between John and Bob as father and son.
% The third line defines the relationship between Mary and Mary as mother and daughter.
% The fourth line defines the relationship between Mary and Bob as mother and son.
% The fifth line defines John as a male.
% The sixth line defines Bob as a male.
% The seventh line defines Mary as a female.
% The eighth line defines the rule that a parent is either a father or a mother.
% The ninth line defines the query to find all parents of Bob.
% The tenth line is the query result, which shows that John and Mary are both parents of Bob.
```

This code demonstrates a more complex knowledge base and rule-based query in Prolog. It defines relationships between individuals, such as parent-child and gender, and uses these relationships to answer queries about family members. The query result shows that both John and Mary are parents of Bob, demonstrating the power of Prolog's logical reasoning capabilities.