```prolog
:- dynamic fact/1.
:- dynamic rule/2.

% Add a fact to the knowledge base
add_fact(Fact) :-
  assertz(fact(Fact)).

% Add a rule to the knowledge base
add_rule(Head, Body) :-
  assertz(rule(Head, Body)).

% Query the knowledge base
query(Goal) :-
  call(Goal).

% Prove a goal using forward chaining
prove(Goal) :-
  fact(Goal).
prove(Goal) :-
  rule(Head, Body),
  prove_body(Body),
  Goal = Head.

% Prove a body of a rule
prove_body([]).
prove_body([Goal | Goals]) :-
  prove(Goal),
  prove_body(Goals).

% Print the knowledge base
print_knowledge_base :-
  forall(fact(Fact), print(Fact)),
  forall(rule(Head, Body), print_rule(Head, Body)).

% Print a rule
print_rule(Head, Body) :-
  print(Head),
  print(' :- '),
  print(Body),
  nl.

% Example usage

add_fact(sky_is_blue).
add_rule(daytime, [sun_up, no_clouds]).
add_rule(nighttime, [sun_down]).
add_rule(moon_visible, [nighttime, clear_sky]).

query(sky_is_blue).
query(daytime).
query(nighttime).
query(moon_visible).

print_knowledge_base.
```

This Prolog program demonstrates a more complex and detailed knowledge base with facts, rules, and a query mechanism. Here's an explanation:

1. **Dynamic Knowledge Base:**
   - The program uses dynamic predicates `fact/1` and `rule/2` to store facts and rules, respectively. This allows the knowledge base to be updated and modified dynamically.

2. **Adding Facts and Rules:**
   - `add_fact/1` and `add_rule/2` are used to add new facts and rules to the knowledge base.

3. **Querying the Knowledge Base:**
   - `query/1` is used to query the knowledge base with a goal. If the goal can be proven using the facts and rules in the knowledge base, the program will return `true`.

4. **Forward Chaining:**
   - The program uses forward chaining to prove goals. It starts with the goal and tries to find facts or rules that can be used to derive the goal.

5. **Proving Goals and Bodies:**
   - `prove/1` is used to prove a goal. It checks if the goal is a fact or can be derived from a rule.
   - `prove_body/1` is used to prove the body of a rule. It recursively proves each subgoal in the body.

6. **Printing the Knowledge Base:**
   - `print_knowledge_base/0` is used to print the facts and rules in the knowledge base.

7. **Example Knowledge Base and Queries:**
   - The program includes an example knowledge base with facts about the sky, day and night, and the moon.
   - It also includes queries to demonstrate how the program can answer questions based on the knowledge base.

This program showcases a more comprehensive and intricate use of Prolog for knowledge representation and reasoning tasks.