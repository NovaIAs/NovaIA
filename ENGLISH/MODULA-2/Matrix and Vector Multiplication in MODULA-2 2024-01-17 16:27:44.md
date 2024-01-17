```modula-2

MODULE complexCode;

TYPE scalar = INTEGER;
TYPE vec = ARRAY [1..10] OF scalar;
TYPE matrix = ARRAY [1..10, 1..10] OF scalar;

PROCEDURE multiVec(x, y : vec; RESULT z : vec);
BEGIN
    FOR i := 1 TO 10 DO
        z[i] := x[i] * y[i]
    END
END multiVec;

PROCEDURE multiMatrix(x, y : matrix; RESULT z : matrix);
BEGIN
    FOR i := 1 TO 10 DO
        FOR j := 1 TO 10 DO
            z[i, j] := 0
            FOR k := 1 TO 10 DO
                z[i, j] := z[i, j] + x[i, k] * y[k, j]
            END
        END
    END
END multiMatrix;

PROCEDURE printVec(x : vec);
BEGIN
    FOR i := 1 TO 10 DO
        WRITE(x[i], " ")
    END;
    NEWLINE
END printVec;

PROCEDURE printMatrix(x : matrix);
BEGIN
    FOR i := 1 TO 10 DO
        FOR j := 1 TO 10 DO
            WRITE(x[i, j], " ")
        END;
        NEWLINE
    END
END printMatrix;

VAR vecA, vecB : vec;
VAR matA, matB, matC : matrix;

BEGIN
    FOR i := 1 TO 10 DO
        vecA[i] := i
        vecB[i] := 2 * i
    END;
    multiVec(vecA, vecB, vecA);
    printVec(vecA);

    FOR i := 1 TO 10 DO
        FOR j := 1 TO 10 DO
            matA[i, j] := i * j
            matB[i, j] := (i + j) * 2
        END
    END;
    multiMatrix(matA, matB, matC);
    printMatrix(matC)
END complexCode.

```

This code performs matrix and vector multiplication. It defines two vector types, a 2D matrix type, as well as their multiplication procedures. The code then creates two vector and matrix variables, performs the multiplication, and prints the result.