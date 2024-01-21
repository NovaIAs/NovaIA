```modula-2
MODULE Matrices;
FROM FormatString IMPORT Printf;

TYPE
  Indices = ARRAY[1 .. 99] OF CARDINAL;

  Matriz = ARRAY[Indices, Indices] OF REAL;

VAR
  M, N: CARDINAL;
  A: Matriz;
  I, J: CARDINAL;

PROCEDURE InicializaMatriz(VAR A: Matriz; M, N: CARDINAL);
(* Inicializa una matriz A de tamaño M x N con ceros *)

BEGIN
  FOR I := 1 TO M DO
    FOR J := 1 TO N DO
      A[I, J] := 0.0
    END
  END
END InicializaMatriz;

PROCEDURE ImprimeMatriz(A: Matriz; M, N: CARDINAL);
(* Imprime una matriz A de tamaño M x N *)

BEGIN
  FOR I := 1 TO M DO
    FOR J := 1 TO N DO
      Printf("%8.2f  ", A[I, J])
    END;
    Println
  END
END ImprimeMatriz;

PROCEDURE LlenaMatriz(VAR A: Matriz; M, N: CARDINAL);
(* Llena una matriz A de tamaño M x N con números aleatorios *)

BEGIN
  FOR I := 1 TO M DO
    FOR J := 1 TO N DO
      A[I, J] := Random() * 1000.0
    END
  END
END LlenaMatriz;

PROCEDURE SumaMatrices(A, B: Matriz; M, N: CARDINAL; VAR C: Matriz);
(* Suma dos matrices A y B de tamaño M x N y almacena el resultado en C *)

BEGIN
  FOR I := 1 TO M DO
    FOR J := 1 TO N DO
      C[I, J] := A[I, J] + B[I, J]
    END
  END
END SumaMatrices;

PROCEDURE RestaMatrices(A, B: Matriz; M, N: CARDINAL; VAR C: Matriz);
(* Resta dos matrices A y B de tamaño M x N y almacena el resultado en C *)

BEGIN
  FOR I := 1 TO M DO
    FOR J := 1 TO N DO
      C[I, J] := A[I, J] - B[I, J]
    END
  END
END RestaMatrices;

PROCEDURE MultiplicaMatrices(A, B: Matriz; M, N, P: CARDINAL; VAR C: Matriz);
(* Multiplica dos matrices A de tamaño M x N y B de tamaño N x P,
   y almacena el resultado en C de tamaño M x P *)

VAR
  I, J, K: CARDINAL;
BEGIN
  FOR I := 1 TO M DO
    FOR J := 1 TO P DO
      C[I, J] := 0.0;
      FOR K := 1 TO N DO
        C[I, J] := C[I, J] + A[I, K] * B[K, J]
      END
    END
  END
END MultiplicaMatrices;

PROCEDURE TranspuestaMatriz(A: Matriz; M, N: CARDINAL; VAR AT: Matriz);
(* Calcula la matriz transpuesta de A de tamaño M x N y la almacena en AT *)

BEGIN
  FOR I := 1 TO M DO
    FOR J := 1 TO N DO
      AT[J, I] := A[I, J]
    END
  END
END TranspuestaMatriz;

BEGIN
  M := 3;
  N := 4;

  InicializaMatriz(A, M, N);
  LlenaMatriz(A, M, N);
  ImprimeMatriz(A, M, N);

  Println;
  Printf("Matriz transpuesta:\n");
  TranspuestaMatriz(A, M, N, AT);
  ImprimeMatriz(AT, N, M);

  Println;
  Printf("Suma de dos matrices:\n");
  SumaMatrices(A, A, M, N, C);
  ImprimeMatriz(C, M, N);

  Println;
  Printf("Resta de dos matrices:\n");
  RestaMatrices(A, A, M, N, C);
  ImprimeMatriz(C, M, N);

  Println;
  Printf("Multiplicación de dos matrices:\n");
  MultiplicaMatrices(A, A, M, N, N, C);
  ImprimeMatriz(C, M, N);
END Matrices.
```

Explicación del código:

* El código implementa operaciones básicas con matrices en MODULA-2.
* La estructura `Matriz` define matrices bidimensionales de números reales.
* Los procedimientos `InicializaMatriz`, `LlenaMatriz`, `ImprimeMatriz`, `SumaMatrices`, `RestaMatrices`, `MultiplicaMatrices`, `TranspuestaMatriz` implementan las operaciones correspondientes sobre las matrices.
* El código principal crea una matriz `A` de tamaño 3x4, la llena con números aleatorios, la imprime y luego calcula y muestra la matriz transpuesta, la suma de dos matrices, la resta de dos matrices y la multiplicación de dos matrices.