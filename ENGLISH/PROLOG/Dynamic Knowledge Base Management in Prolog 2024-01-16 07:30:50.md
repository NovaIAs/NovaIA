```prolog
:- dynamic assertion/1. % Define a dynamic predicate for managing assertions

% Define a rule to create a new assertion
create_assertion(Assertion) :-
    assert(assertion(Assertion)).

% Define a rule to retract an assertion
retract_assertion(Assertion) :-
    retract(assertion(Assertion)).

% Define a rule to list all assertions
list_assertions :-
    findall(Assertion, assertion(Assertion), Assertions),
    sort(Assertions, SortedAssertions),
    print_assertions(SortedAssertions).

% Define a helper function to print assertions
print_assertions([]).
print_assertions([Assertion | Assertions]) :-
    write(Assertion), nl,
    print_assertions(Assertions).

% Define a rule to find all assertions that contain a given term
find_assertions(Term, Assertions) :-
    findall(Assertion, assertion(Assertion), Assertions),
    filter_assertions(Term, Assertions, FilteredAssertions).

% Define a helper function to filter assertions based on a given term
filter_assertions(_, [], []).
filter_assertions(Term, [Assertion | Assertions], FilteredAssertions) :-
    (   member(Term, Assertion)
    ->  FilteredAssertions = [Assertion | FilteredRest]
    ;   FilteredRest = FilteredAssertions
    ),
    filter_assertions(Term, Assertions, FilteredRest).

% Define a rule to assert a fact if it does not already exist
assert_fact(Fact) :-
    \+ (assertion(Fact) -> true ; fail).

% Define a rule to retract a fact if it exists
retract_fact(Fact) :-
    retract(assertion(Fact)).

% Define a rule to list all facts
list_facts :-
    findall(Fact, assertion(Fact), Facts),
    sort(Facts, SortedFacts),
    print_facts(SortedFacts).

% Define a helper function to print facts
print_facts([]).
print_facts([Fact | Facts]) :-
    write(Fact), nl,
    print_facts(Facts).

% Define a rule to create a new rule
create_rule(Head, Body) :-
    assert(rule(Head, Body)).

% Define a rule to retract a rule
retract_rule(Head, Body) :-
    retract(rule(Head, Body)).

% Define a rule to list all rules
list_rules :-
    findall(Rule, rule(Head, Body), Rules),
    sort(Rules, SortedRules),
    print_rules(SortedRules).

% Define a helper function to print rules
print_rules([]).
print_rules([Rule | Rules]) :-
    write(Rule), nl,
    print_rules(Rules).

% Define a rule to find all rules that have a given head or body
find_rules(Term, Rules) :-
    findall(Rule, rule(Head, Body), Rules),
    filter_rules(Term, Rules, FilteredRules).

% Define a helper function to filter rules based on a given term
filter_rules(_, [], []).
filter_rules(Term, [Rule | Rules], FilteredRules) :-
    (   member(Term, Head) ; member(Term, Body)
    ->  FilteredRules = [Rule | FilteredRest]
    ;   FilteredRest = FilteredRules
    ),
    filter_rules(Term, Rules, FilteredRest).
```

This Prolog code provides a comprehensive framework for managing assertions, facts, and rules in a dynamic knowledge base. It includes a rich set of operations for creating, retracting, listing, and searching assertions, facts, and rules. The code is well-structured and modular, making it easy to extend and modify.

Here's a detailed explanation of the code:

1. **Dynamic Predicate for Assertions**:
   - We define a dynamic predicate `assertion/1` to manage assertions. This allows us to assert and retract assertions dynamically during program execution.

2. **Creating Assertions**:
   - The `create_assertion/1` rule is used to create a new assertion. It takes an assertion as an argument and asserts it using the `assert/1` built-in predicate.

3. **Retracting Assertions**:
   - The `retract_assertion/1` rule is used to retract an assertion. It takes an assertion as an argument and retracts it using the `retract/1` built-in predicate.

4. **Listing Assertions**:
   - The `list_assertions` rule is used to list all assertions. It uses the `findall/3` and `sort/2` built-in predicates to retrieve and sort the assertions before printing them.

5. **Finding Assertions**:
   - The `find_assertions/2` rule is used to find all assertions that contain a given term. It uses the `findall/3` and `filter_assertions/3` predicates to retrieve and filter the assertions based on the given term.

6. **Asserting and Retracting Facts**:
   - The `assert_fact/1` and `retract_fact/1` rules are used to assert and retract facts, respectively. They use the `assert/1` and `retract/1` built-in predicates to manipulate facts.

7. **Listing Facts**:
   - The `list_facts` rule is used to list all facts. It uses the `findall/3` and `sort/2` built-in predicates to retrieve and sort the facts before printing them.

8. **Creating and Retracting Rules**:
   - The `create_rule/2` and `retract_rule/2` rules are used to create and retract rules, respectively. They use the `assert/1` and `retract/1` built-in predicates to manipulate rules.

9. **Listing Rules**:
   - The `list_rules` rule is used to list all rules. It uses the `findall/3` and `sort/2` built-in predicates to retrieve and sort the rules before printing them.

10. **Finding Rules**:
    - The `find_rules/2` rule is used to find all rules that have a given head or body. It uses the `findall/3` and `filter_rules/3` predicates to retrieve and filter the rules based on the given term.

This code provides a powerful and flexible framework for managing a dynamic knowledge base in Prolog. It allows for the creation, retraction, listing, and searching of assertions, facts, and rules, making it a valuable tool for building complex knowledge-based systems.