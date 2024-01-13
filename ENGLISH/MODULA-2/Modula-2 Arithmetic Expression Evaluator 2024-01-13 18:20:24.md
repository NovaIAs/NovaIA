```modula-2
MODULE Complex;
FROM Terminal IMPORT WriteString, WriteCard, WriteLn, WriteF;
FROM Strings IMPORT Length, Copy, Clear, CompStr;
FROM InOut IMPORT ReadCard, ReadLine, ReadChar;
TYPE
  CharSet = ARRAY [0..255] OF BOOLEAN;
  Char = CHAR;
  String = ARRAY OF Char;
  Position = CARDINAL;
  Symbol = (ID, NUM, OP, DELIM);
  Token = RECORD
    symbol: Symbol;
    position: Position;
    value: CASE Symbol OF
      ID: String;
      NUM: CARDINAL;
      OP: Char;
      DELIM: Char
    END
  END;
  Expression = RECORD
    symbol: Symbol;
    position: Position;
    left, right: Expression;
    value: CARDINAL
  END;
  SyntaxError = RECORD
    position: Position;
    message: String
  END;

PROCEDURE CheckSyntax(Expression: Expression): SyntaxError;
BEGIN
  IF Expression.symbol = OP THEN
    IF Expression.left.symbol = NUM AND Expression.right.symbol = NUM THEN
      RETURN NIL
    ELSE
      RETURN SyntaxError{position: Expression.position, message: "Invalid operands for operator"}
    END
  ELSIF Expression.symbol = ID THEN
    IF Expression.left = NIL AND Expression.right = NIL THEN
      RETURN NIL
    ELSE
      RETURN SyntaxError{position: Expression.position, message: "Invalid syntax for identifier"}
    END
  ELSE
    RETURN SyntaxError{position: Expression.position, message: "Invalid expression"}
  END
END CheckSyntax;

PROCEDURE ParseExpression(Source: String; Position: Position): (Expression, Position);
VAR
  CurrentChar: Char;
  CharSet: CharSet;
  TokenStream: ARRAY OF Token;
  TokenStreamLength: CARDINAL;
  CurrentToken: CARDINAL;
BEGIN
  Clear(CharSet);
  CharSet[','] := TRUE;
  CharSet['+'] := TRUE;
  CharSet['-'] := TRUE;
  CharSet['*'] := TRUE;
  CharSet['/'] := TRUE;
  CharSet['('] := TRUE;
  CharSet[')'] := TRUE;
  TokenStreamLength := 0;
  CurrentToken := 0;
  WHILE Position < Length(Source) DO
    CurrentChar := Source[Position];
    IF CharSet[CurrentChar] THEN
      TokenStream[TokenStreamLength] := Token{symbol: DELIM, position: Position, value: CurrentChar};
      INC(TokenStreamLength);
      INC(Position)
    ELSIF CurrentChar = ' ' OR CurrentChar = '\t' OR CurrentChar = '\n' THEN
      INC(Position)
    ELSIF CurrentChar >= '0' AND CurrentChar <= '9' THEN
      VAR
        Number: CARDINAL;
      BEGIN
        Number := 0;
        WHILE Position < Length(Source) AND Source[Position] >= '0' AND Source[Position] <= '9' DO
          Number := Number * 10 + ORD(Source[Position]) - ORD('0');
          INC(Position)
        END;
        TokenStream[TokenStreamLength] := Token{symbol: NUM, position: Position - 1, value: Number};
        INC(TokenStreamLength)
      END
    ELSIF CurrentChar >= 'a' AND CurrentChar <= 'z' OR CurrentChar >= 'A' AND CurrentChar <= 'Z' THEN
      VAR
        Identifier: String;
      BEGIN
        Identifier := "";
        WHILE Position < Length(Source) AND (Source[Position] >= 'a' AND Source[Position] <= 'z' OR Source[Position] >= 'A' AND Source[Position] <= 'Z') DO
          Identifier := Identifier + Source[Position];
          INC(Position)
        END;
        TokenStream[TokenStreamLength] := Token{symbol: ID, position: Position - 1, value: Identifier};
        INC(TokenStreamLength)
      END
    ELSE
      TokenStream[TokenStreamLength] := Token{symbol: OP, position: Position, value: CurrentChar};
      INC(TokenStreamLength);
      INC(Position)
    END
  END;
  WHILE CurrentToken < TokenStreamLength DO
    IF TokenStream[CurrentToken].symbol IN [ID, OP] THEN
      IF TokenStream[CurrentToken].symbol = ID THEN
        VAR
          Left: Expression;
        BEGIN
          Left := Expression{symbol: TokenStream[CurrentToken].symbol, position: TokenStream[CurrentToken].position, left: NIL, right: NIL, value: 0};
          INC(CurrentToken);
          IF TokenStream[CurrentToken].symbol = OP AND TokenStream[CurrentToken].value = '(' THEN
            INC(CurrentToken);
            Left.right, Position := ParseExpression(Source, Position);
            IF TokenStream[CurrentToken].symbol = OP AND TokenStream[CurrentToken].value = ')' THEN
              INC(CurrentToken)
            ELSE
              RETURN SyntaxError{position: TokenStream[CurrentToken].position, message: "Missing closing parenthesis"}
            END
          END
        END
      ELSE
        VAR
          Left, Right: Expression;
        BEGIN
          Left := Expression{symbol: TokenStream[CurrentToken].symbol, position: TokenStream[CurrentToken].position, left: NIL, right: NIL, value: 0};
          INC(CurrentToken);
          Right, Position := ParseExpression(Source, Position);
          Left.right := Right
        END
      END;
      IF TokenStream[CurrentToken].symbol = OP AND TokenStream[CurrentToken].value = ',' THEN
        INC(CurrentToken);
        Left.left, Position := ParseExpression(Source, Position)
      END;
      IF CheckSyntax(Left) = NIL THEN
        RETURN Left, Position
      ELSE
        RETURN CheckSyntax(Left)
      END
    ELSE
      INC(CurrentToken)
    END
  END;
  RETURN SyntaxError{position: Position, message: "Unexpected end of expression"}
END ParseExpression;

PROCEDURE PrintExpression(Expression: Expression);
BEGIN
  CASE Expression.symbol OF
    ID: WriteString(Expression.value);
    NUM: WriteCard(Expression.value);
    OP: WriteChar(Expression.value);
    DELIM: WriteChar(Expression.value)
  END;
  IF Expression.left <> NIL THEN
    PrintExpression(Expression.left);
    IF Expression.symbol = OP THEN
      WriteChar(' ');
      PrintExpression(Expression.right)
    END
  END
END PrintExpression;

PROCEDURE EvaluateExpression(Expression: Expression): CARDINAL;
BEGIN
  CASE Expression.symbol OF
    ID: RETURN 0;
    NUM: RETURN Expression.value;
    OP:
      CASE Expression.value OF
        '+': RETURN EvaluateExpression(Expression.left) + EvaluateExpression(Expression.right);
        '-': RETURN EvaluateExpression(Expression.left) - EvaluateExpression(Expression.right);
        '*': RETURN EvaluateExpression(Expression.left) * EvaluateExpression(Expression.right);
        '/': RETURN EvaluateExpression(Expression.left) DIV EvaluateExpression(Expression.right)
      END
    DELIM: RETURN 0
  END
END EvaluateExpression;

PROCEDURE Main;
VAR
  Source: String;
  Expression: Expression;
  Result: CARDINAL;
  SyntaxError: SyntaxError;
BEGIN
  Source := "1 + 2 * 3, 4 + 5 * 6";
  Expression, _ := ParseExpression(Source, 0);
  IF Expression.symbol = ID OR Expression.symbol = NUM THEN
    PrintExpression(Expression);
    WriteLn;
    Result := EvaluateExpression(Expression);
    WriteCard(Result);
    WriteLn
  ELSE
    SyntaxError := CheckSyntax(Expression);
    WriteString("Syntax error at position ");
    WriteCard(SyntaxError.position);
    WriteString(": ");
    WriteString(SyntaxError.message);
    WriteLn
  END
END Main.
```

This code is a simple calculator that can evaluate arithmetic expressions. It first parses the expression into a tree of Expression records, then checks the syntax of the tree, and finally evaluates the tree to produce a result.

The ParseExpression procedure is a recursive descent parser that takes an expression as a string and a starting position and returns an Expression record representing the parsed expression and the position of the next character after the expression. The parser uses a CharSet to identify delimiter characters, and a TokenStream to hold the tokens that are parsed from the expression.

The CheckSyntax procedure checks the syntax of an Expression record and returns a SyntaxError record if there is an error. The procedure checks for invalid operands for operators, invalid syntax for identifiers, and invalid expressions.

The PrintExpression procedure prints an Expression record to the console. The procedure uses recursion to print the left and right subtrees of an Expression record, and it prints the operator or delimiter symbol of the Expression record.

The EvaluateExpression procedure evaluates an Expression record and returns the result. The procedure uses recursion to evaluate the left and right sub