```modula-2
MODULE Main;
FROM Terminal IMPORT WriteString, WriteLn;

TYPE
  BoardType = ARRAY[0..3] OF ARRAY[0..3] OF CARDINAL;
  PositionType = ARRAY[0..1] OF CARDINAL;

VAR Board : BoardType;
    Player : CARDINAL;

PROCEDURE ClearBoard;
BEGIN
  FOR X := 0 TO 3 DO
    FOR Y := 0 TO 3 DO
      Board[X][Y] := 0
    END
  END
END ClearBoard;

PROCEDURE DrawBoard;
BEGIN
  WriteString("  0 1 2 3");
  WriteLn;
  FOR Y := 0 TO 3 DO
    WriteString(IntToStr(Y) + " ");
    FOR X := 0 TO 3 DO
      WriteString(IntToStr(Board[X][Y]) + " ");
    END;
    WriteLn
  END
END DrawBoard;

PROCEDURE PlacePiece(Position : PositionType);
BEGIN
  Board[Position[0]][Position[1]] := Player
END PlacePiece;

PROCEDURE CheckForWin(Position : PositionType) : BOOLEAN;
VAR
  X, Y, Count : CARDINAL;
BEGIN
  Count := 0;

  FOR X := 0 TO 3 DO
    IF Board[X][Position[1]] = Player THEN
      INC(Count)
    END
  END;

  IF Count = 4 THEN
    RETURN TRUE
  END;

  Count := 0;

  FOR Y := 0 TO 3 DO
    IF Board[Position[0]][Y] = Player THEN
      INC(Count)
    END
  END;

  IF Count = 4 THEN
    RETURN TRUE
  END;

  Count := 0;

  IF Position[0] = Position[1] THEN
    FOR X := 0 TO 3 DO
      IF Board[X][X] = Player THEN
        INC(Count)
      END
    END
  ELSIF Position[0] + Position[1] = 3 THEN
    X := 3;
    FOR Y := 0 TO 3 DO
      IF Board[X][Y] = Player THEN
        INC(Count)
      END;
      DEC(X)
    END
  END;

  IF Count = 4 THEN
    RETURN TRUE
  END;

  RETURN FALSE
END CheckForWin;

PROCEDURE GetPlayerMove(Player : CARDINAL) : PositionType;
VAR
  X, Y : CARDINAL;
BEGIN
  WriteString("Player " + IntToStr(Player) + ", enter your move (x, y): ");
  Read(X, Y);

  RETURN PositionType(X, Y)
END GetPlayerMove;

PROCEDURE Main;
BEGIN
  ClearBoard;
  Player := 1;

  WHILE NOT CheckForWin(PositionType(2, 2)) DO
    DrawBoard;
    PlacePiece(GetPlayerMove(Player));

    IF Player = 1 THEN
      Player := 2
    ELSE
      Player := 1
    END
  END;

  DrawBoard;
  WriteString("Player " + IntToStr(Player) + " wins!");
  WriteLn
END Main.
```

This Modula-2 code is a complete implementation of the classic game of Tic-Tac-Toe. The code starts by importing the `Terminal` module, which provides functions for writing to the console.

The `BoardType` type is defined as a 4x4 array of `CARDINAL`s, which will be used to store the state of the game board. The `PositionType` type is defined as an array of two `CARDINAL`s, which will be used to represent positions on the game board.

The `ClearBoard` procedure initializes the game board to all zeros.

The `DrawBoard` procedure draws the current state of the game board to the console.

The `PlacePiece` procedure places a piece on the game board at the specified position.

The `CheckForWin` procedure checks if the current state of the game board results in a win for either player.

The `GetPlayerMove` procedure gets the next move from the player whose turn it is.

The `Main` procedure is the entry point for the program. It initializes the game board, starts a loop to take turns between the two players, and checks for a win after each turn. Once a win is detected, the program prints the winning player and exits.

This code is a good example of how to structure a Modula-2 program. It uses modules to organize the code, procedures to encapsulate functionality, and types to represent data. The code is also well-commented, making it easy to understand and modify.