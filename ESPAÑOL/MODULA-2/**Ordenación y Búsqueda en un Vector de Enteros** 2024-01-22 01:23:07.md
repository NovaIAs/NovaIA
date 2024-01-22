```modula-2
(* Procedimiento que ordena un vector de números enteros usando el algoritmo de ordenación rápida. *)
PROCEDURE Quicksort(VAR a: ARRAY OF INTEGER; low, high: CARDINAL);
  VAR pivot, i, j: CARDINAL;
BEGIN
    IF low < high THEN
        pivot := a[low];
        i := low;
        j := high;
        WHILE i < j DO
            WHILE a[i] <= pivot AND i < j DO
                i := i + 1
            END;
            WHILE a[j] > pivot AND i < j DO
                j := j - 1
            END;
            IF i < j THEN
                SWAP(a[i], a[j])
            END
        END;
        SWAP(a[low], a[j]);
        Quicksort(a, low, j - 1);
        Quicksort(a, j + 1, high)
    END
END Quicksort;

(* Función que realiza la búsqueda binaria de un elemento en un vector de números enteros. *)
FUNCTION BinarySearch(a: ARRAY OF INTEGER; low, high, x: INTEGER): CARDINAL;
VAR mid: CARDINAL;
BEGIN
    WHILE low <= high DO
        mid := (low + high) DIV 2;
        IF a[mid] = x THEN
            RETURN mid
        ELSIF a[mid] < x THEN
            low := mid + 1
        ELSE
            high := mid - 1
        END
    END;
    RETURN -1
END BinarySearch;

(* Procedimiento que lee un vector de números enteros desde la entrada estándar. *)
PROCEDURE ReadVector(VAR a: ARRAY OF INTEGER; n: CARDINAL);
VAR i: CARDINAL;
BEGIN
    FOR i := 0 TO n - 1 DO
        READ(a[i])
    END
END ReadVector;

(* Procedimiento que escribe un vector de números enteros en la salida estándar. *)
PROCEDURE WriteVector(a: ARRAY OF INTEGER; n: CARDINAL);
VAR i: CARDINAL;
BEGIN
    FOR i := 0 TO n - 1 DO
        WRITE(a[i])
    END
END WriteVector;

(* Programa principal *)
MODULE Main;
FROM InOut IMPORT WriteString, WriteLn, ReadInt;

VAR n: CARDINAL;
    a: ARRAY OF INTEGER;

BEGIN
    WriteString("Ingrese el número de elementos: ");
    n := ReadInt;

    a := ARRAY OF INTEGER 0 .. n - 1;

    WriteString("Ingrese los elementos separados por espacios: ");
    ReadVector(a, n);

    Quicksort(a, 0, n - 1);

    WriteString("Vector ordenado: ");
    WriteVector(a, n);

    WriteString("Ingrese el elemento que desea buscar: ");
    x := ReadInt;

    i := BinarySearch(a, 0, n - 1, x);

    IF i = -1 THEN
        WriteString("El elemento no se encontró.")
    ELSE
        WriteString("El elemento se encontró en la posición ", i)
    END
END Main.
```

Este código implementa un programa en MODULA-2 que ordena un vector de números enteros usando el algoritmo de ordenación rápida y realiza una búsqueda binaria para encontrar un elemento en el vector ordenado.

El programa primero lee el número de elementos del vector y luego lee los elementos del vector desde la entrada estándar. A continuación, ordena el vector usando el algoritmo de ordenación rápida. Una vez ordenado el vector, el programa lee el elemento que se desea buscar y realiza una búsqueda binaria para encontrarlo.

Si el elemento se encuentra en el vector, el programa escribe la posición en la que se encontró. Si el elemento no se encuentra en el vector, el programa escribe un mensaje indicando que el elemento no se encontró.