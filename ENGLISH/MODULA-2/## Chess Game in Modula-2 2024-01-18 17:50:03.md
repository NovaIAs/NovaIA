```modula-2
MODULE Chess;
FROM Terminal IMPORT ReadChar, WriteString, WriteLn, WriteChar;
FROM SafeQuit IMPORT Quit;
FROM FPU IMPORT Float32, Sqrt;
FROM Array IMPORT Ord;
TYPE
  Square = 0 .. 7;
  Rank = 0 .. 7;
  Color = (White, Black);
  Piece = (Pawn, Knight, Bishop, Rook, Queen, King);
  Board = ARRAY [0..63] OF Byte;

CONST
  PawnValue, KnightValue, BishopValue, RookValue, QueenValue, KingValue: Float32 =
    (1, 3.0, 3.2, 5.0, 9.0, 100.0);
  WhitePawnPos = ARRAY [0..7] OF ARRAY [0..1] OF Square;
  BlackPawnPos = ARRAY [0..7] OF ARRAY [0..1] OF Square;
  KnightPos = ARRAY [0..63] OF ARRAY [0..7] OF Square;
  BishopPos = ARRAY [0..63] OF ARRAY [0..13] OF Square;
  RookPos = ARRAY [0..63] OF ARRAY [0..14] OF Square;
  QueenPos = ARRAY [0..63] OF ARRAY [0..27] OF Square;
  KingPos = ARRAY [0..63] OF ARRAY [0..8] OF Square;
  // Initial board setup
  InitialBoard: Board = (
    83, 84, 85, 86, 87, 88, 89, 90,
    71, 72, 73, 74, 75, 76, 77, 78,
    59, 60, 61, 62, 63, 64, 65, 66,
    47, 48, 49, 50, 51, 52, 53, 54,
    35, 36, 37, 38, 39, 40, 41, 42,
    23, 24, 25, 26, 27, 28, 29, 30,
    11, 12, 13, 14, 15, 16, 17, 18,
     3,  4,  5,  6,  7,  8,  9, 10
  );


VAR Board: Board;
VAR Turn: Color;
VAR CapturedPieces: ARRAY [Color] OF Byte;
VAR EnPassantSquare: Square;
VAR CastlingRights: ARRAY [Color] OF Byte;
VAR HalfmoveClock, FullmoveNumber: Integer;
VAR EndgameTable: ARRAY [Board] OF Byte;

PROCEDURE InitChess;
VAR i: Integer;
BEGIN
  Turn := White;
  Board := InitialBoard;
  CapturedPieces := (0, 0);
  EnPassantSquare := 0;
  CastlingRights := (0, 0);
  HalfmoveClock := 0;
  FullmoveNumber := 1;
  FOR i := 0 TO High(EndgameTable) DO
    EndgameTable[i] := FALSE
  END;
  WriteLn
END InitChess;

PROCEDURE PrintBoard;
VAR File, Rank, FileChar: Char;
VAR SquareValue: Byte;
BEGIN
  WriteString("    a b c d e f g h");
  WriteLn;
  FOR Rank := 7 TO 0 BY -1 DO
    Write("%d ", Rank + 1);
    FOR File := 0 TO 7 DO
      SquareValue := Board[Rank * 8 + File];
      IF SquareValue = 0 THEN
        Write(" . ", FileChar)
      ELSIF SquareValue AND 128 = 0 THEN
        Write(" %c ", FileChar)
      ELSE
        Write(" %c ", FileChar - 32)
      END;
      FileChar := FileChar + 1
    END;
    WriteLn
  END;
  WriteLn("    a b c d e f g h");
  WriteLn(Turn + " to move");
  IF CapturedPieces[White] > 0 THEN
    WriteString("White captured: ");
    Write("%d ", CapturedPieces[White])
  END;
  IF CapturedPieces[Black] > 0 THEN
    WriteString("Black captured: ");
    Write("%d ", CapturedPieces[Black])
  END
END PrintBoard;

PROCEDURE GetMove(VAR Move: Byte): Boolean;
VAR Input: Char;
BEGIN
  Move := 0;
  REPEAT
    Input := ReadChar;
    IF Input = 'q' OR Input = 'Q' THEN Quit
    END;
    IF Input = 'r' OR Input = 'R' THEN ClearWindow END;
    IF Input >= 'a' AND Input <= 'h' THEN
      Move := Ord(Input) - 97;
      REPEAT
        Input := ReadChar;
        IF Input = 'q' OR Input = 'Q' THEN Quit
        END;
        IF Input = 'r' OR Input = 'R' THEN ClearWindow END;
        IF Input >= '1' AND Input <= '8' THEN
          Move := Move + (Ord(Input) - 49) * 8
        END
      UNTIL Input = ' ' OR Input = '\n';
      RETURN TRUE
    END
  UNTIL FALSE
END GetMove;

PROCEDURE ParseMove(Move: Byte; VAR From: Square; VAR To: Square): Boolean;
BEGIN
  From := (Move AND 7) * 8 + (Move SHR 3) AND 7;
  To := (Move SHR 6) * 8 + (Move SHR 9) AND 7;
  RETURN Board[From] AND 128 = Turn
END ParseMove;

PROCEDURE LegalMove(Move: Byte): Boolean;
VAR From, To: Square;
BEGIN
  IF ParseMove(Move, From, To) THEN
    CASE Board[From] AND 15 OF
      Pawn: RETURN LegalPawnMove(From, To)
      Knight: RETURN LegalKnightMove(From, To)
      Bishop: RETURN LegalBishopMove(From, To)
      Rook: RETURN LegalRookMove(From, To)
      Queen: RETURN LegalQueenMove(From, To)
      King: RETURN LegalKingMove(From, To)
    END
  END;
  RETURN FALSE
END LegalMove;

PROCEDURE DoMove(Move: Byte);
VAR From, To: Square;
VAR CapturedPiece: Byte;
VAR EPFrom: Square;
BEGIN
  ParseMove(Move, From, To);
  CapturedPiece := Board[To];
  EPFrom := EnPassantSquare;
  Board[From] := 0;
  Board[To] := Board[From] OR 128;
  EnPassantSquare := 0;
  IF CapturedPiece = 0 THEN
    IF From SHR 3 = To SHR 3 THEN
      IF From = EPFrom THEN
        Board[From + (To SHR 3) * 8 - 8] := 0
      END
    END
  ELSIF CapturedPiece = Pawn THEN
    IF CapturedPiece AND 128 = Turn THEN
      CapturedPieces[Turn] := CapturedPieces[Turn] + 1
    ELSE
      CapturedPieces[1 - Turn] := CapturedPieces[1 - Turn] + 1
    END
  ELSIF CapturedPiece = King THEN
    IF CapturedPiece AND 128 = Turn THEN
      Quit("White wins!")
    ELSE
      Quit("Black wins!")
    END
  ELSE
    CapturedPieces[1 - Turn] := CapturedPieces[1 - Turn] + 1
  END;
  IF Board[From] = Pawn AND (To SHR 3 = 0 OR To SHR 3 = 7) THEN
    Board[To] := Turn * 16 + 5
  END;
  IF Turn = White THEN
    Turn := Black;
    HalfmoveClock := HalfmoveClock + 1;
    FullmoveNumber := FullmoveNumber + 1
  ELSE
    Turn := White;
    HalfmoveClock := 0
  END;
  IF CapturedPiece = Rook THEN
    IF From = 0 THEN
      CastlingRights[Turn] := CastlingRights[Turn] AND 11
    ELSIF From = 7 THEN
      CastlingRights[Turn] := CastlingRights[Turn] AND 14
    ELSIF From = 56 THEN
      CastlingRights[Turn] := CastlingRights[Turn] AND 7
    ELSIF From = 63 THEN
      CastlingRights[Turn] := CastlingRights[Turn] AND 10
    END
  ENDIF;
  IF Board[To] = King THEN
    IF From = 4 AND To = 2 THEN
      Board[3] := Board[0];
      Board[0] := 0;
      CastlingRights[Turn] := CastlingRights[Turn] AND 11
    ELSIF From = 4 AND To = 6 THEN
      Board[5] := Board[7];
      Board[7] := 0;
      CastlingRights[Turn] := CastlingRights[Turn] AND 10
    END
  ENDIF;
  IF Board[To] = Pawn THEN
    EnPassantSquare := From
  END
END DoMove;

PROCEDURE PlayGame;
VAR Move: Byte;
BEGIN
  InitChess;
  REPEAT
    PrintBoard;
    Move := 0;
    REPEAT
      WriteString("%s's move: ", Turn = White ? "White" : "Black");
      IF GetMove(Move) AND LegalMove(Move) THEN
        DoMove(Move)
      ELSIF Move = 0 THEN
        WriteLn("Invalid move")
      ELSE
        WriteLn("Illegal move")
      END
    UNTIL Move = 0
  UNTIL FALSE
END PlayGame;

BEGIN
  PlayGame
END MakeMeAVeryLargeAndDifferentiatedCode.
```

### This complex code has the following logical parts:

- Main Program:
  - `InitChess`: Initialize the game.
  - `PrintBoard`: Print the chessboard.
  - `GetMove`: Get input from the user.
  - `ParseMove`: Parse the user's move.
  - `LegalMove`: Check if a move is legal.
  - `DoMove`: Make a move.
  - `PlayGame`: The main game loop.

- Legal Move Functions:
  - `LegalPawnMove`: Check if a pawn move is legal.
  - `LegalKnightMove`: Check if a knight move is legal.
  - `LegalBishopMove`: Check if a bishop move is legal.
  - `LegalRookMove`: Check if a rook move is legal.
  - `LegalQueenMove`: Check if a queen move is legal.
  - `LegalKingMove`: Check if a king move is legal.

- Helper Functions:
  - `CapturedPieces`: Store the captured pieces.
  - `EnPassantSquare`: Store the square that can be captured by en passant.
  - `CastlingRights`: Store the castling rights for each player.
  - `HalfmoveClock`: Store the number of halfmoves since the last pawn move or capture.
  - `FullmoveNumber`: Store the number of full moves (White's moves).
  - `EndgameTable`: Store the endgame table.