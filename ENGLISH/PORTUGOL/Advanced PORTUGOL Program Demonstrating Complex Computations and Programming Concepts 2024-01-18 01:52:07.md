```PORTUGOL
PROGRAM TESTE;

PROCEDURE SOMATOTAL(VAR NUM1, NUM2: REAL); % PASSAGEM DE PARÂMETROS POR VALOR %
BEGIN
  NUM1 := NUM1 + NUM2;
END;

% FUNÇÃO SOMA COM PARÂMETROS PASSADOS POR REFERÊNCIA %
FUNCTION SOMA(NUM1, NUM2: REAL): REAL;
BEGIN
  SOMA := NUM1 + NUM2;
END;

PROCEDURE TROCANUMEROS(VAR P, Q: REAL);
BEGIN
  VAR R: REAL;
  R := P;
  P := Q;
  Q := R;
END;

% PROCEDIMENTO RECURSIVO QUE CALCULA O FATORIAL DE UM NÚMERO %
PROCEDURE FATORIAL(VAR N: INTEGER);
BEGIN
  IF N = 1 THEN FATORIAL := 1
  ELSE FATORIAL := N * FATORIAL(N - 1);
END;

% MATRIZ 3X3 %
MATRIZ M[1..3, 1..3]: INTEGER;

% VÁRIAVEL LITERAL %
CHAR LITERAL[1..100]: "ESTE É UM TEXTO LITERAL";

% VARIÁVEL ABSTRATA %
TYPE FILA = ARRAY[1..10] OF INTEGER;
VAR FILA1, FILA2: FILA;

% PROCEDIMENTO PRINCIPAL %
BEGIN
  VAR A, B, C: REAL;
  A := 20.0;
  B := 30.0;
  SOMATOTAL(A, B);
  C := SOMA(A, B);
  TROCANUMEROS(A, B);
  FATORIAL(10);

  % INICIALIZAÇÃO DA MATRIZ M %
  FOR I := 1 TO 3 DO
    FOR J := 1 TO 3 DO
      M[I, J] := I + J;
    END;
  END;

  % IMPRESSÃO DA MATRIZ M %
  FOR I := 1 TO 3 DO
    FOR J := 1 TO 3 DO
      PRINT(M[I, J]);
    END;
  END;

  % INICIALIZAÇÃO DAS FILAS %
  FOR I := 1 TO 10 DO
    FILA1[I] := I;
    FILA2[I] := 10 - I;
  END;

  % IMPRESSÃO DAS FILAS %
  FOR I := 1 TO 10 DO
    PRINT(FILA1[I]);
  END;
  PRINTLN;
  FOR I := 1 TO 10 DO
    PRINT(FILA2[I]);
  END;
END.
```

This code is a PORTUGOL program that performs various complex computations and demonstrates different programming concepts.

- **Procedure SOMATOTAL:** Accepts two real numbers as parameters by value and adds them together.

- **Function SOMA:** Calculates the sum of two real numbers passed by reference.

- **Procedure TROCANUMEROS:** Swaps the values of two real numbers passed by reference.

- **Procedure FATORIAL:** Calculates the factorial of an integer number using recursion.

- **Matrix M:** Declares a 3x3 integer matrix and initializes it with values.

- **Literal Variable LITERAL:** Defines a literal variable containing a text string.

- **Abstract Variable Type FILA:** Defines a custom data type FILA as an array of 10 integers.

- **Procedure Principal:** The main procedure of the program that performs the following tasks:
  - Calls the SOMATOTAL procedure to add two numbers.
  - Calls the SOMA function to calculate the sum of two numbers.
  - Calls the TROCANUMEROS procedure to swap the values of two numbers.
  - Calls the FATORIAL procedure to calculate the factorial of a number.
  - Initializes the matrix M with values.
  - Prints the values of the matrix M.
  - Initializes the two FILA variables with values.
  - Prints the values of the two FILA variables.