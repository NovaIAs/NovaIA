```d
import std.stdio, std.traits, std.algorithm, std.typecons, std.variant,
        std.functional, std.traits, std.array, std.integer_sequence;

struct Update {
    type If (TCond, TTrue, TFalse, TVar) pure nothrow @nogc @safe {
        static if (cond, true, false, var) if (cond) var: true else var: false;
    }
}

template UpdatePred(T)(T pred(T value)) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = pred;
    }
    return This;
}

template Not(T)(T pred(T value)) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T.Pred.Not;
    }
    return This;
}

template And(T1, T2)(T1 pred1(T1 value), T2 pred2(T2 value)) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T1.Pred.And(T2.Pred);
    }
    return This;
}

template Or(T1, T2)(T1 pred1(T1 value), T2 pred2(T2 value)) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T1.Pred.Or(T2.Pred);
    }
    return This;
}

template Cmp(T)(T pred(T value, T other)) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T.Pred.Cmp;
    }
    return This;
}

template Eq(T) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T.Pred.Eq;
    }
    return This;
}

template Less(T) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T.Pred.Less;
    }
    return This;
}

template Greater(T) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T.Pred.Greater;
    }
    return This;
}

template LessEq(T) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T.Pred.LessEq;
    }
    return This;
}

template GreaterEq(T) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T.Pred.GreaterEq;
    }
    return This;
}

template Contains(T)(T pred(T value, T other)) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T.Pred.Contains;
    }
    return This;
}

template ContainsI(T)(T pred(T value, T other)) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T.Pred.ContainsI;
    }
    return This;
}

template ContainsN(T)(T pred(T value, T other)) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T.Pred.ContainsN;
    }
    return This;
}

template Contained(T)(T pred(T value, T other)) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T.Pred.Contained;
    }
    return This;
}

template ContainedI(T)(T pred(T value, T other)) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T.Pred.ContainedI;
    }
    return This;
}

template ContainedN(T)(T pred(T value, T other)) pure nothrow @nogc @safe {
    struct This : Update {
        override type Pred = T.Pred.ContainedN;
    }
    return This;
}

void main() {
    import std.stdio, std.string, std.range;

    enum Output {
        False, True, Nil, Maybe,
    }

    alias Print = writefln;
    enum Table_ {
        PLAYER,
        COMPUTER,
    }

    enum Cell_ {
        EMPTY,
        CROSS,
        CIRCLE,
    }

    Cell_.Cross.swap = Cell_.CIRCLE;
    Cell_.CIRCLE.swap = Cell_.CROSS;

    struct Board {
        Cell_[Table_][Table_] cells;
    }

    static size_t countWins(Board board, Table_ player, Table_ opponent) pure nothrow @nogc @safe {
        auto pred = Eq(Cell_(player));
        return (board.cells == [
            pred, pred, pred,
            pred, pred, pred,
            pred, pred, pred,
        ]) + (board.cells.transpose == [
            pred, pred, pred,
            pred, pred, pred,
            pred, pred, pred,
        ]) + (board.cells.diagonal == [
            pred, pred, pred,
        ]) + (board.cells.ndiagonal == [
            pred, pred, pred,
        ]);
    }

    auto findBestMove(Board board, Table_ player, Table_ opponent) pure nothrow @nogc @safe {
        enum Moveopt {
            OK,
            FAIL,
            LOSE,
        }

        immutable Win = size_t._max;
        immutable Lose = size_t._min;

        auto move = Cell_[Table_][Table_](Win);
        auto score = size_t._max;
        auto pred = Eq(Cell_(player));
        auto oppPred = Eq(Cell_(opponent));

        immutable Diagonals = [
            [0, 0, 2, 2],
            [2, 0, 0, 2],
        ];
        for (auto row; 0 .. board.cells.length) {
            for (auto col; 0 .. board.cells[row].length) {
                if (board.cells[row][col] != Cell_.EMPTY) continue;
                auto copy = board;
                copy.cells[row][col] = player;
                auto winCount = countWins(copy, player, opponent);
                if (winCount >= 3) return [row, col];

                auto loseCount = countWins(copy, opponent, player);
                if (loseCount >= 3) continue;

                immutable Default = [
                    [0, 1],
                    [0, 2],
                    [1, 0],
                    [1, 2],
                    [2, 0],
                    [2, 1],
                ];
                 immutable CenterBorder = [
                    [0, 0],
                    [0, 2],
                    [2, 0],
                    [2, 2],
                ];

                immutable Diagonals = [
                    [0, 0, 2, 2],
                    [2, 0, 0, 2],
                ];
                immutable StartingCenterOffset = Cell_[Table_][Table_](Empty);
                immutable DiagonalMirror = Cell_[Table_][Table_](Empty);

                immutable DiagonalOffset = [
                    Cell_[Table_][Table_](Empty),
                    StartingCenterOffset,
                    DiagonalMirror,
                    Cell_[Table_][Table_](Empty),
                ];

                auto moves = Default;

                if (!pred(copy.cells[1][1])) {
                    moves += CenterBorder;
                }
                immutable Directions = [
                    [0, 1],
                    [0, -1],
                    [1, 0],
                    [-1, 0],
                ];
                for (auto diag; Diagonals) {
                    auto [fromRow, fromCol, toRow, toCol] = diag;
                    auto center = pred(copy.cells[1][1]);
                    auto crossed = pred(copy.cells[fromRow][fromCol]) && pred(copy.cells[toRow][toCol]);
                    auto empty = Empty == copy.cells[fromRow][fromCol] && Empty == copy.cells[toRow][toCol];
                    if ((fromRow == 0 || fromRow == 2) && (fromCol == 0 || fromCol == 2)) {
                        auto offset = DiagonalOffset[fromRow][fromCol];
                        moves += [
                            [fromRow + offset[0], fromCol + offset[1]],
                            [toRow + offset[0], toCol + offset[1]],
                        ];
                    } else if (center && !crossed && !empty) {
                        auto offset = DiagonalOffset[fromRow][fromCol];
                        moves += [
                            [fromRow + offset[0], fromCol + offset[1]],
                            [toRow + offset[0], toCol + offset[1]],
                        ];
                    }
                    for (auto dir; Directions) {
                        auto [dx, dy] = dir;
                        auto [dRow, dCol] = fromRow + dx, fromCol + dy;
                        auto off = 0;
                        auto offset = Cell_[Table_][Table_](empty);
                        auto border = (dRow == 0) || (dRow == 2) || (dCol == 0) || (dCol == 2);
                        auto centerAdjacent = center && !pred(copy.cells[dRow][dCol]) && border;
                        auto diagonalAdjacent = (fromRow == dRow) && (fromCol == dCol) && !crossed && empty && !center;
                        if (centerAdjacent || diagonalAdjacent) {
                            auto next = dRow + dx, dCol + dy;
                            if (!pred(copy.cells[dRow][dCol])) {
                                offset = DiagonalOffset[next][dCol];
                            }
                        }
                        if ((next != dRow) || (next != dCol) || !pred(copy.cells[next][dCol])) {
                            moves += [dRow + offset[0], dCol + offset[1]];
                        }
                        else {
                            off = 1;
                        }
                        dRow = fromRow + dx * off;
                        dCol = fromCol + dy * off;
                        if ((dRow != fromRow) || (dCol != fromCol) || !pred(copy.cells[dRow][dCol])) {
                            moves += [dRow + offset[0], dCol + offset[1]];
                        }
                    }
                }
                for (auto mv; moves) {
                    auto [r, c] = mv;
                    if (board.cells[r][c] == Empty) {
                        copy.cells[r][c] = player;
                        auto prevScore = score;
                        score = min(score, findBestMove(copy, opponent, player));
                        if (prevScore == Win || score == Lose) {
                            return [r, c];
                        }
                        copy.cells[r][c] = Empty;
                    }
                }
            }
        }
        return move;
    }

    enum MoveResult {
        OK,
        FAIL,
        WIN,
        LOSE,
        DRAW,
    }

    Board buildBoard(immutable Cell_[Table_][Table_] moveHistory) pure nothrow @nogc @safe {
        Board board;
        for (auto r; 0 .. 3) {
            for (auto c; 0 .. 3) {
                board.cells[r][c] = moveHistory[r][c];
            }
        }
        return board;
    }

    immutable AllMoves = [
        [0, 0],
        [0, 1],
        [0, 2],
        [1, 0],
        [1, 1],
        [1, 2],
        [2, 0],
        [2, 1],
        [2, 2],
    ];

    MoveResult playMove(const Board board, Cell_[Table_][Table_] moveHistory, Table_ player, Table_ opponent, Cell_ move) pure nothrow @nogc @safe {
        auto r = move[0];
        auto c = move[1];
        if (board.cells[r][c] != Cell_.EMPTY) return FAIL;
        auto copy = moveHistory;
        copy[r][c] = player;

        auto bothCells = Eq(Cell_(player)) & Eq(Cell_(opponent));
        auto oppPred = Eq(Cell_(opponent));
        auto winPredEqPlayer = countWins(buildBoard(copy), player, opponent) >= 3;
        auto winPredEqOpponent = countWins(buildBoard(copy), opponent, player) >= 3;

        auto drawPred = (board.cells == copy) && !winPredEqPlayer && !winPredEqOpponent;
        auto losePred = !winPredEqPlayer && winPredEqOpponent;
        auto winPred = winPredEqPlayer && !winPredEqOpponent;

        auto result = OK;
        if (drawPred) result = DRAW;
        if (losePred) result = LOSE;
        if (winPred) result = WIN;
        if (result == OK) {
            auto [emptyRows, emptyCols] = AllMoves
                .map!([r, c] {
                    return emptyCell(board, r, c);
                })
                ._partition!([it] (auto v) { return v != Null; });

            if (!emptyCols.length && !emptyRows.length) result = DRAW;
            else {
                auto opponentMove = findBestMove(buildBoard(copy), opponent, player);
                result = playMove(buildBoard(copy), copy, opponent, player, opponentMove);
            }
        }
        return result;
    }

    auto emptyCell(const Board board, immutable size_t row, immutable size_t col) pure nothrow @nogc @safe {
        if (board.cells[row][col] == Cell_.EMPTY) return [row, col];
        else return null;
    }

    immutable gameHistory = [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]];
    immutable playerMoveHistory = [];
    immutable computerMoveHistory = [];
    auto playerTurn = true;
    auto moves = 0;
    auto result = OK;

    while (result == OK) {
        if (playerTurn) {
            writefln("Choose a position (row, col): ");
            auto move = readLine().split(",")
                .map!(it -> it.toInt());
            auto r = move[0];
            auto c = move[1];
            if (gameHistory[r][c] == Empty) {
                gameHistory[r][c] = Cell_.PLAYER;
                moves += 1;
                playerMoveHistory += [r, c];
                playerTurn = false;
            }
        } else {
            auto move = findBestMove(buildBoard(gameHistory), Cell_.COMPUTER, Cell_.PLAYER);
            gameHistory[move[0]][move[1]] = Cell_.COMPUTER;
            moves += 1;
            computerMoveHistory += move;
            playerTurn = true;
        }
        auto mr = playMove(buildBoard(gameHistory), gameHistory, Cell_.PLAYER, Cell_.COMPUTER, gameHistory[move[0]][move[1]]);
        result = mr;
    }

    writeln("Game Over!");
    writefln("Moves: %d", moves);
    if (result == WIN) writeln("You Win!");
    if (result == LOSE) writeln("You Lose!");
    if (result == DRAW) writeln("Draw!");
}