```modula-2
MODULE SumaMatrices;
FROM FormatString IMPORT Fprintf;
FROM Terminal IMPORT WriteString, WriteCard, WriteLn;

TYPE Matriz = ARRAY[1..10, 1..10] OF INTEGER;

VAR A, B, C: Matrix;

PROCEDURE LeerMatriz(VAR M: Matriz);
VAR I, J: INTEGER;
BEGIN
  WriteString("Introduce los valores de la matriz:"); WriteLn;
  FOR I := 1 TO 10 DO
    FOR J := 1 TO 10 DO
      WriteString("M["); WriteCard(I); WriteString(", "); WriteCard(J); WriteString("]: ");
      ReadCard(M[I, J]);
    END
  END
END LeerMatriz;

PROCEDURE SumarMatrices(VAR A, B, C: Matriz);
VAR I, J: INTEGER;
BEGIN
  FOR I := 1 TO 10 DO
    FOR J := 1 TO 10 DO
      C[I, J] := A[I, J] + B[I, J];
    END
  END
END SumarMatrices;

PROCEDURE MostrarMatriz(M: Matriz);
VAR I, J: INTEGER;
BEGIN
  WriteString("La matriz es:"); WriteLn;
  FOR I := 1 TO 10 DO
    FOR J := 1 TO 10 DO
      Fprintf(STRING, "%3d ", M[I, J]);
    END;
    WriteLn
  END
END MostrarMatriz;

BEGIN
  LeerMatriz(A);
  LeerMatriz(B);
  SumarMatrices(A, B, C);
  MostrarMatriz(C);
END SumaMatrices.
```

Este código en MODULA-2 realiza la suma de dos matrices de 10x10.

El programa comienza definiendo el tipo `Matriz` como una matriz de 10x10 de enteros.

Luego, declara tres variables de tipo `Matriz` llamadas `A`, `B` y `C`.

El procedimiento `LeerMatriz` se utiliza para leer los valores de una matriz. Se le pasa una matriz como parámetro y la llena con los valores introducidos por el usuario.

El procedimiento `SumarMatrices` se utiliza para sumar dos matrices. Se le pasan tres matrices como parámetros: `A`, `B` y `C`. La matriz `C` es la matriz resultante de la suma de `A` y `B`.

El procedimiento `MostrarMatriz` se utiliza para mostrar una matriz. Se le pasa una matriz como parámetro y la muestra en la pantalla.

El programa principal comienza leyendo los valores de las matrices `A` y `B` utilizando el procedimiento `LeerMatriz`.

Luego, llama al procedimiento `SumarMatrices` para sumar las matrices `A` y `B`, y almacena el resultado en la matriz `C`.

Finalmente, llama al procedimiento `MostrarMatriz` para mostrar la matriz `C` en la pantalla.