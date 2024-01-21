```modula-2
MODULE Sort;

FROM Terminal IMPORT WriteString, WriteLn, WriteCard, WriteSpace, WriteInt;

TYPE
  Element = INTEGER;

VAR
  Array: ARRAY 10 OF Element;
  I, J: CARDINAL;
  Temp: Element;

PROCEDURE Swap(VAR A, B: Element);
BEGIN
  Temp := A;
  A := B;
  B := Temp;
END Swap;

PROCEDURE BubbleSort;
VAR
  Swapped: BOOLEAN;
BEGIN
  Swapped := TRUE;
  WHILE Swapped DO
  BEGIN
    Swapped := FALSE;
    FOR I := 0 TO HIGH(Array) - 1 DO
      IF Array[I] > Array[I + 1] THEN
      BEGIN
        Swap(Array[I], Array[I + 1]);
        Swapped := TRUE;
      END
    END;
  END;
END BubbleSort;

PROCEDURE PrintArray;
VAR
  I: CARDINAL;
BEGIN
  FOR I := 0 TO HIGH(Array) DO
  BEGIN
    WriteCard(I, 3);
    WriteSpace;
    WriteInt(Array[I], 2);
    WriteLn;
  END;
END PrintArray;

BEGIN
  WriteString("Introduzca 10 números enteros: ");
  FOR I := 0 TO HIGH(Array) DO
    ReadCard(Array[I]);

  WriteString("Arreglo sin ordenar: ");
  PrintArray;

  BubbleSort;

  WriteString("Arreglo ordenado: ");
  PrintArray;

END Sort.
```

Explicación:

1. El módulo Sort define un tipo de datos personalizado llamado `Element` para representar los elementos del arreglo y una variable `Array` de tipo `ARRAY 10 OF Element` para almacenar los 10 elementos enteros.

2. El procedimiento `Swap` intercambia los valores de dos variables de tipo `Element`.

3. El procedimiento `BubbleSort` implementa el algoritmo de ordenación de burbuja para ordenar el arreglo `Array` en orden ascendente.

4. El procedimiento `PrintArray` imprime los elementos del arreglo `Array` en la consola.

5. En la parte principal del módulo Sort, se solicita al usuario que introduzca 10 números enteros y se almacenan en el arreglo `Array`.

6. Se llama al procedimiento `PrintArray` para imprimir el arreglo sin ordenar.

7. Se llama al procedimiento `BubbleSort` para ordenar el arreglo.

8. Finalmente, se llama de nuevo a `PrintArray` para imprimir el arreglo ordenado.