```cool
class Reversi {
  -- Attribute --
  board : Board;  -- The board
  whose_turn : String;  -- The current player
  -- Method --
  initializer(rows, columns, player_1, player_2)
      is
        board := Board(rows, columns);
        board.place_initial_pieces;
        whose_turn := player_1
  end;

  end.

class Board {
  -- Attribute --
  rows : Int;  -- The number of rows
  columns : Int;  -- The number of columns
  board : Array[Array[String]];  -- The board itself
  -- Method --
  initializer(rows, columns)
      is
        rows := rows;
        columns := columns;
        board := Array.new(rows, Array.new(columns, "Empty"));
  end;

  place_initial_pieces
      is
        board[4][4] := "White";
        board[4][5] := "Black";
        board[5][4] := "Black";
        board[5][5] := "White"
  end;

  flip_pieces(player, row, column)
      is
        if board[row][column] = "Empty" then
            error("Invalid move")
        end;
        // Find the opponent's pieces
        capture_directions := {"North", "South", "East", "West", "NorthEast", "NorthWest", "SouthEast", "SouthWest"};
        for direction in capture_directions
            do
                found := false;
                step := 1;
                while not found and step < rows and step < columns
                    do
                        if board[row + step * direction.row][column + step * direction.column] = player then
                            found := true
                        elsif board[row + step * direction.row][column + step * direction.column] = "Empty" then
                            step := rows + 1
                        end
                    od
                step := step - 1;
                if found and step > 0 then
                    // Flip the opponent's pieces
                    for i in [1..step]
                        do
                            board[row + i * direction.row][column + i * direction.column] := player
                        od
                end
            od
  end;

  score(player)
      is
        white_count := 0;
        black_count := 0;
        for row in [1..rows]
            do
                for column in [1..columns]
                    do
                        if board[row][column] = "White" then
                            white_count := white_count + 1
                        elsif board[row][column] = "Black" then
                            black_count := black_count + 1
                        end
                    od
            od
        if player = "White" then
            return white_count
        else
            return black_count
        end
  end;

  is_game_over()
      is
        return board.all_pieces_placed() or board.no_more_moves()
  end;

  all_pieces_placed()
      is
        for row in [1..rows]
            do
                for column in [1..columns]
                    do
                        if board[row][column] = "Empty" then
                            return false
                        end
                    od
            od
        return true
  end;

  no_more_moves()
      is
        for row in [1..rows]
            do
                for column in [1..columns]
                    do
                        if board[row][column] = "Empty" then
                            if board.can_place_piece(whose_turn, row, column) then
                                return false
                            end
                        end
                    od
            od
        return true
  end;

  can_place_piece(player, row, column)
      is
        if board[row][column] <> "Empty" then
            return false
        end;
        // Find the opponent's pieces
        capture_directions := {"North", "South", "East", "West", "NorthEast", "NorthWest", "SouthEast", "SouthWest"};
        for direction in capture_directions
            do
                found := false;
                step := 1;
                while not found and step < rows and step < columns
                    do
                        if board[row + step * direction.row][column + step * direction.column] = player then
                            found := true
                        elsif board[row + step * direction.row][column + step * direction.column] = "Empty" then
                            step := rows + 1
                        end
                    od
                step := step - 1;
                if found and step > 0 then
                    return true
                end
            od
        return false
  end;

  print()
      is
        for row in [1..rows]
            do
                for column in [1..columns]
                    do
                        print(board[row][column], " ")
                    od;
                print("\n")
            od
  end;

  end.
```

This code implements the classic board game Reversi in COOL. The game is played on a square board, typically 8x8, with two players, Black and White. The players take turns placing pieces, known as discs, on the board. The goal is to capture your opponent's pieces by surrounding them with your own.

The code is divided into two classes:
- `Reversi` : Represents the game itself, including the game board, the current player, and methods to play the game.
- `Board` : Represents the game board and provides methods to manipulate it.

Details about the code:

- `Reversi.initializer`: Initializes a new game of Reversi with the specified board size and player names. It creates an empty board, places the initial four pieces in the center, and sets the current player.

- `Board.initializer`: Initializes a new game board with the specified size. It creates a 2D array to represent the board, with each cell initially set to "Empty".

- `Board.place_initial_pieces`: Places the initial four pieces in the center of the board.

- `Board.flip_pieces`: Flips pieces on the board to capture the opponent's pieces. It takes three parameters:

  1. `player`: The player who is making the move.
  2. `row`: The row of the piece being placed.
  3. `column`: The column of the piece being placed.

  If the move is valid, it flips the opponent's pieces in the specified direction.

- `Board.score`: Calculates the score for a player. It takes one parameter:

  1. `player`: The player whose score is being calculated.

  It counts the number of pieces the player has on the board and returns the score.

- `Board.is_game_over`: Checks if the game is over. It returns true if all pieces have been placed or if there are no more valid moves.

- `Board.all_pieces_placed`: Checks if all pieces have been placed on the board. It iterates through the board and returns false if any cell is still empty.

- `Board.no_more_moves`: Checks if there are no more valid moves for either player. It iterates through the board and returns true if there are no empty cells that can be filled with a piece.

- `Board.can_place_piece`: Checks if a piece can be placed at a specific location on the board. It takes three parameters:

  1. `player`: The player who is making the move.
  2. `row`: The row of the location being checked.
  3. `column`: The column of the location being checked.

  It checks if the location is empty and if any of the opponent's pieces can be captured by placing a piece at that location.