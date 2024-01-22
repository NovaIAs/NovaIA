```modula-2
MODULE Sudoku;
FROM Terminal IMPORT WriteString, WriteLn, ReadKey;

VAR
  Tablero: ARRAY[1..9, 1..9] OF Byte;
  Fila, Columna, Valor: Byte;
  JuegoTerminado: BOOLEAN;

PROCEDURE PrintTablero;
VAR
  Fila, Columna: Byte;
BEGIN
  FOR Fila := 1 TO 9 DO
    FOR Columna := 1 TO 9 DO
      WriteString(Format("%d ", [Tablero[Fila, Columna]]));
    END;
    WriteLn;
  END;
END PrintTablero;

PROCEDURE InicializarTablero;
BEGIN
  FOR Fila := 1 TO 9 DO
    FOR Columna := 1 TO 9 DO
      Tablero[Fila, Columna] := 0;
    END;
END InicializarTablero;

PROCEDURE LeerTablero;
VAR
  Fila, Columna: Byte;
BEGIN
  WriteString("Ingrese el tablero de Sudoku (filas y columnas separadas por espacios): ");
  FOR Fila := 1 TO 9 DO
    FOR Columna := 1 TO 9 DO
      Tablero[Fila, Columna] := ReadByte - 48;
    END;
  END;
END LeerTablero;

PROCEDURE ComprobarFila(Fila: Byte): BOOLEAN;
VAR
  Usados: ARRAY[1..9] OF BOOLEAN;
  Columna: Byte;
BEGIN
  FOR Columna := 1 TO 9 DO
    Usados[Tablero[Fila, Columna]] := TRUE;
  END;
  FOR Valor := 1 TO 9 DO
    IF NOT Usados[Valor] THEN
      RETURN FALSE;
    END;
  END;
  RETURN TRUE;
END ComprobarFila;

PROCEDURE ComprobarColumna(Columna: Byte): BOOLEAN;
VAR
  Usados: ARRAY[1..9] OF BOOLEAN;
  Fila: Byte;
BEGIN
  FOR Fila := 1 TO 9 DO
    Usados[Tablero[Fila, Columna]] := TRUE;
  END;
  FOR Valor := 1 TO 9 DO
    IF NOT Usados[Valor] THEN
      RETURN FALSE;
    END;
  END;
  RETURN TRUE;
END ComprobarColumna;

PROCEDURE ComprobarCaja(Fila: Byte; Columna: Byte): BOOLEAN;
VAR
  Usados: ARRAY[1..9] OF BOOLEAN;
  FilaCaja, ColumnaCaja, FilaInicial, ColumnaInicial, Valor: Byte;
BEGIN
  FilaCaja := (Fila - 1) DIV 3 + 1;
  ColumnaCaja := (Columna - 1) DIV 3 + 1;
  FilaInicial := (FilaCaja - 1) * 3 + 1;
  ColumnaInicial := (ColumnaCaja - 1) * 3 + 1;
  FOR Fila := FilaInicial TO FilaInicial + 2 DO
    FOR Columna := ColumnaInicial TO ColumnaInicial + 2 DO
      Usados[Tablero[Fila, Columna]] := TRUE;
    END;
  END;
  FOR Valor := 1 TO 9 DO
    IF NOT Usados[Valor] THEN
      RETURN FALSE;
    END;
  END;
  RETURN TRUE;
END ComprobarCaja;

PROCEDURE ComprobarTablero: BOOLEAN;
VAR
  Fila, Columna: Byte;
BEGIN
  FOR Fila := 1 TO 9 DO
    IF NOT ComprobarFila(Fila) THEN
      RETURN FALSE;
    END;
  END;
  FOR Columna := 1 TO 9 DO
    IF NOT ComprobarColumna(Columna) THEN
      RETURN FALSE;
    END;
  END;
  FOR Fila := 1 TO 3 DO
    FOR Columna := 1 TO 3 DO
      IF NOT ComprobarCaja(Fila, Columna) THEN
        RETURN FALSE;
      END;
    END;
  END;
  RETURN TRUE;
END ComprobarTablero;

PROCEDURE ResolverSudoku;
VAR
  Fila, Columna, Valor: Byte;
BEGIN
  JuegoTerminado := FALSE;
  WHILE NOT JuegoTerminado DO
    FOR Fila := 1 TO 9 DO
      FOR Columna := 1 TO 9 DO
        IF Tablero[Fila, Columna] = 0 THEN
          FOR Valor := 1 TO 9 DO
            Tablero[Fila, Columna] := Valor;
            IF ComprobarFila(Fila) AND ComprobarColumna(Columna) AND ComprobarCaja(Fila, Columna) THEN
              JuegoTerminado := TRUE;
              BREAK;
            END;
          END;
          IF NOT JuegoTerminado THEN
            Tablero[Fila, Columna] := 0;
          END;
        END;
      END;
    END;
  END;
END ResolverSudoku;

BEGIN
  InicializarTablero;
  LeerTablero;
  IF ComprobarTablero THEN
    ResolverSudoku;
    PrintTablero;
  ELSE
    WriteString("El tablero de Sudoku no es válido.");
  END;
  ReadKey;
END Sudoku.
```

Explicación del código:

* El módulo `Sudoku` define el programa principal y varias funciones y procedimientos.
* La variable `Tablero` es una matriz bidimensional de bytes que representa el tablero de Sudoku.
* El procedimiento `PrintTablero` imprime el tablero en la consola.
* El procedimiento `InicializarTablero` inicializa el tablero con ceros.
* El procedimiento `LeerTablero` lee el tablero de la entrada estándar.
* Los procedimientos `ComprobarFila`, `ComprobarColumna` y `ComprobarCaja` comprueban si una fila, columna o caja del tablero contiene todos los dígitos del 1 al 9.
* El procedimiento `ComprobarTablero` comprueba si el tablero es válido, es decir, si no contiene ninguna fila, columna o caja que no cumpla con las reglas del Sudoku.
* El procedimiento `ResolverSudoku` resuelve el tablero de Sudoku utilizando un algoritmo de backtracking.
* El programa principal crea una instancia del módulo `Sudoku` y llama al procedimiento `ResolverSudoku`. Si el tablero es válido, el programa imprime la solución. De lo contrario, imprime un mensaje de error.