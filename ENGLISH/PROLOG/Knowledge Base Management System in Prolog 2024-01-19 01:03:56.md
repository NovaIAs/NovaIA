```prolog
:- dynamic fact_added/1.
:- dynamic fact_removed/1.

% Define a rule to add a fact to the knowledge base.
add_fact(Fact) :-
    assert(Fact),
    assert(fact_added(Fact)).

% Define a rule to remove a fact from the knowledge base.
remove_fact(Fact) :-
    retract(Fact),
    assert(fact_removed(Fact)).

% Define a rule to check if a fact is in the knowledge base.
has_fact(Fact) :-
    (
        fact_added(Fact)
    ;
        fact_removed(Fact)
    ).

% Define a rule to get all facts in the knowledge base.
get_facts(Facts) :-
    findall(Fact, (has_fact(Fact)), Facts).

% Define a rule to clear the knowledge base.
clear_knowledge_base :-
    retractall(fact_added(_)),
    retractall(fact_removed(_)),
    retractall(_).

% Define a rule to print a fact.
print_fact(Fact) :-
    write(Fact),
    nl.

% Define a rule to print all facts in the knowledge base.
print_all_facts :-
    get_facts(Facts),
    forall(member(Fact, Facts), print_fact(Fact)).

% Define a rule to run a query.
run_query(Query) :-
    call(Query),
    write('true'),
    nl;
    write('false'),
    nl.

% Define a rule to run a loop.
loop :-
    write('Enter a command (add, remove, has, get, clear, print, query, or exit): '),
    read_line(Command),
    handle_command(Command).

% Define a rule to handle a command.
handle_command(Command) :-
    (
        Command = 'add' ->
            write('Enter a fact to add: '),
            read_line(Fact),
            add_fact(Fact)
    ;
        Command = 'remove' ->
            write('Enter a fact to remove: '),
            read_line(Fact),
            remove_fact(Fact)
    ;
        Command = 'has' ->
            write('Enter a fact to check: '),
            read_line(Fact),
            (
                has_fact(Fact) ->
                    write('true'),
                    nl
            ;
                write('false'),
                nl
            )
    ;
        Command = 'get' ->
            get_facts(Facts),
            (
                Facts = [] ->
                    write('No facts in the knowledge base'),
                    nl
            ;
                forall(member(Fact, Facts), print_fact(Fact))
            )
    ;
        Command = 'clear' ->
            clear_knowledge_base,
            write('Knowledge base cleared'),
            nl
    ;
        Command = 'print' ->
            print_all_facts
    ;
        Command = 'query' ->
            write('Enter a query: '),
            read_line(Query),
            run_query(Query)
    ;
        Command = 'exit' ->
            write('Exiting...'),
            nl
    ),
    loop.

% Start the loop.
loop.
```

This code provides a simple interactive command-line interface to manage a knowledge base represented in Prolog. Here's a brief explanation:

- **Dynamic Facts**: We use dynamic facts (`fact_added/1` and `fact_removed/1`) to keep track of facts that have been added or removed from the knowledge base. This allows us to check if a fact is in the knowledge base, even if it was added or removed during the session.

- **Fact Manipulation**: The code defines rules to add facts (`add_fact/1`), remove facts (`remove_fact/1`), check if a fact exists (`has_fact/1`), and get all facts (`get_facts/1`).

- **Command Handling**: The `handle_command/1` rule handles different commands:
  - `add`: Prompts the user to enter a fact and adds it to the knowledge base.
  - `remove`: Prompts the user to enter a fact and removes it from the knowledge base.
  - `has`: Prompts the user to enter a fact and checks if it exists in the knowledge base.
  - `get`: Retrieves and prints all facts in the knowledge base.
  - `clear`: Clears the knowledge base, removing all facts.
  - `print`: Prints all facts in the knowledge base.
  - `query`: Prompts the user to enter a query and evaluates it.
  - `exit`: Exits the program.

- **Query Evaluation**: The `run_query/1` rule evaluates a query entered by the user. It prints `true` if the query succeeds and `false` if it fails.

- **Interactive Loop**: The `loop/0` rule is the main entry point of the program. It prompts the user to enter a command, handles the command, and then loops back to prompt for the next command.

This code demonstrates various features of Prolog, including dynamic facts, fact manipulation, command handling, and query evaluation. It allows users to interact with a knowledge base, adding, removing, and checking facts, and running queries.