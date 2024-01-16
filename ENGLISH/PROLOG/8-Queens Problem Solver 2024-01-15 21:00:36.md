```prolog
/*
** A Prolog program to solve the 8-queens problem.
**
** The 8-queens problem is to place 8 queens on a chessboard so that no two queens
** attack each other. This can be expressed as a constraint satisfaction problem,
** where the variables are the positions of the queens on the board, and the
** constraints are that no two queens can be in the same row, column, or diagonal.
**
** This program uses a backtracking search to solve the problem. It starts by
** placing the first queen in the first column of the board. It then tries to place
** the next queen in the next column, and so on. If it finds a solution, it prints
** it and terminates. If it reaches a point where it cannot place any more queens,
** it backtracks and tries a different solution.
**
** The program uses the following predicates:
**
** * `place_queen(Column, Row)`: This predicate places a queen in the given column
**   and row. It checks to make sure that the queen does not attack any other
**   queens on the board.
**
** * `is_safe(Column, Row)`: This predicate checks to make sure that the given
**   column and row is safe for a queen. It checks to make sure that there are no
**   other queens in the same row, column, or diagonal.
**
** * `solve_queens(Column)`: This predicate solves the 8-queens problem starting
**   from the given column. It tries to place a queen in each row of the column,
**   and backtracks if it finds a solution.
**
** * `print_solution()`: This predicate prints a solution to the 8-queens problem.
**
*/

/*
** Place a queen at the given column and row.
*/
place_queen(Column, Row) :-
    is_safe(Column, Row),
    assert(queen(Column, Row)).

/*
** Check if the given column and row is safe for a queen.
*/
is_safe(Column, Row) :-
    \+ queen(Column, _),
    \+ queen(_, Row),
    \+ diagonal_attack(Column, Row).

/*
** Check if there is a queen in the given diagonal.
*/
diagonal_attack(Column, Row) :-
    findall(QueenCol, (queen(QueenCol, QueenRow),
                      abs(Column - QueenCol) =:= abs(Row - QueenRow)),
             Queens),
    length(Queens, N),
    N > 0.

/*
** Solve the 8-queens problem starting from the given column.
*/
solve_queens(Column) :-
    (   Column > 8
    ->  true
    ;   between(1, 8, Row),
        place_queen(Column, Row),
        solve_queens(Column + 1),
        retract(queen(Column, Row))
    ).

/*
** Print a solution to the 8-queens problem.
*/
print_solution :-
    findall(Queen, queen(Column, Queen), Queens),
    maplist(writeln, Queens).

/*
** Start the program.
*/
main :-
    solve_queens(1),
    print_solution.
```

**Explanation:**

The program starts by defining the `place_queen/2` predicate, which places a queen at the given column and row. It checks to make sure that the queen does not attack any other queens on the board.

The `is_safe/2` predicate checks to make sure that the given column and row is safe for a queen. It checks to make sure that there are no other queens in the same row, column, or diagonal.

The `solve_queens/1` predicate solves the 8-queens problem starting from the given column. It tries to place a queen in each row of the column, and backtracks if it finds a solution.

The `print_solution/0` predicate prints a solution to the 8-queens problem.

The `main/0` predicate starts the program. It calls the `solve_queens/1` predicate to find a solution to the 8-queens problem, and then calls the `print_solution/0` predicate to print the solution.