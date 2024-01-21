```modula-2

MODULE FuncionesAvanzadas;
FROM Terminal IMPORT WriteString, ReadChar, ReadLine;

TYPE
  Palabras = ARRAY[0..255] OF CHAR;

PROCEDURE Quicksort(Palabras: ARRAY OF Palabras);
VAR
  Izquierda, Derecha, Pivote, i, j: CARDINAL;
BEGIN
  Izquierda := LOW(Palabras);
  Derecha := HIGH(Palabras);
  Pivote := Palabras[(Izquierda + Derecha) DIV 2];
  REPEAT
    WHILE Palabras[Izquierda][0] < Pivote[0] DO
      Izquierda := Izquierda + 1
    END;
    WHILE Pivote[0] < Palabras[Derecha][0] DO
      Derecha := Derecha - 1
    END;
    IF Izquierda <= Derecha THEN
      SWAP(Palabras[Izquierda], Palabras[Derecha]);
      Izquierda := Izquierda + 1;
      Derecha := Derecha - 1
    END
  UNTIL Izquierda > Derecha;
  IF LOW(Palabras) < Derecha THEN
    Quicksort(Palabras[LOW(Palabras)..Derecha])
  END;
  IF Izquierda < HIGH(Palabras) THEN
    Quicksort(Palabras[Izquierda..HIGH(Palabras)])
  END
END Quicksort;

PROCEDURE ImprimirPalabras(Palabras: ARRAY OF Palabras);
VAR
  i: CARDINAL;
BEGIN
  FOR i := LOW(Palabras) TO HIGH(Palabras) DO
    WriteString(Palabras[i])
  END
END ImprimirPalabras;

VAR
  Palabras: ARRAY[0..255] OF Palabras;
  i: CARDINAL;
BEGIN
  i := 0;
  REPEAT
    IF i < HIGH(Palabras) THEN
      i := i + 1
    END;
    Palabras[i] := ReadLine()
  UNTIL i = HIGH(Palabras) OR Palabras[i] = "";

  Quicksort(Palabras);
  ImprimirPalabras(Palabras)
END FuncionesAvanzadas.

```

**Explicación:**

Este código es un ejemplo de cómo utilizar la clasificación rápida (Quicksort) en MODULA-2. El código toma una matriz de cadenas de caracteres y la ordena alfabéticamente.

El procedimiento `Quicksort` se utiliza para ordenar la matriz. El procedimiento funciona seleccionando un pivote, que es un elemento de la matriz, y luego dividiendo la matriz en dos submatrices: una que contiene todos los elementos que son menores que el pivote, y otra que contiene todos los elementos que son mayores que el pivote. El procedimiento se llama entonces recursivamente a cada una de las submatrices, hasta que todas las submatrices estén ordenadas.

El procedimiento `ImprimirPalabras` se utiliza para imprimir la matriz de cadenas de caracteres. El procedimiento simplemente recorre la matriz y llama al procedimiento `WriteString` para imprimir cada cadena.

El código principal primero lee una serie de cadenas de caracteres del usuario. A continuación, llama al procedimiento `Quicksort` para ordenar la matriz. Por último, llama al procedimiento `ImprimirPalabras` para imprimir la matriz ordenada.