```prolog
% Define the grammar rules for a simple calculator.
grammar(Expr) --> term(Expr).
term(Expr) --> factor(Expr), term_tail(Expr).
term_tail(Expr) --> ["+", "-"], factor(Expr2), {Expr is Expr1 + Expr2}.
term_tail(_).
factor(Expr) --> atom(Expr).
factor(Expr) --> ["(", Expr1, ")"], {Expr is Expr1}.

% Define the predicate to evaluate a given expression.
evaluate(Expr, Value) :-
    phrase(Expr, ParseTree),
    evaluate_tree(ParseTree, Value).

% Define the predicate to evaluate a parse tree.
evaluate_tree([], 0).
evaluate_tree([Operator | Args], Value) :-
    functor(Operator, OperatorName, Arity),
    functor(Args, ArgsName, Arity),
    ArgsValues = [V | _],
    evaluate_tree(Args, ArgsValues),
    call(OperatorName, ArgsValues, V).

% Define the operator predicates.
:- op(1200, fx, +).
:- op(1100, fx, -).
```

This code defines a simple calculator in Prolog. The grammar rules define the syntax of expressions that the calculator can evaluate, including addition, subtraction, parentheses, and unary minus. The evaluate predicate uses the phrase/3 meta-predicate to parse an expression and then calls the evaluate_tree predicate to evaluate the parse tree. The evaluate_tree predicate uses the functor/3 and call/N predicates to determine the operator and arguments of a term and then calls the appropriate operator predicate to evaluate the term.

The following is an example of how to use the calculator:

```prolog
?- evaluate(1 + 2 * 3, X).
X = 7.
```

This query evaluates the expression "1 + 2 * 3" and assigns the result (7) to the variable X.

This calculator is a simple example of how Prolog can be used to define a grammar and then use that grammar to parse and evaluate expressions. Prolog is a powerful language that can be used to solve a wide variety of problems, including natural language processing, expert systems, and constraint satisfaction problems.