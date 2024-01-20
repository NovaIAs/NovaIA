```modula-2
MODULE Librería matematicas;

FROM Interfaces IMPORT Punto, Línea, Triángulo;

TYPE Vector = ARRAY [INTEGER] OF REAL;

PROCEDURE SumaVectores (VAR v1, v2: Vector; VAR v3: Vector);
BEGIN
    FOR i := LOW(v1) TO HIGH(v1) DO
        v3[I] := v1[i] + v2[i];
    END;
END SumaVectores;

PROCEDURE ProductoEscalar (v1, v2: Vector): REAL;
VAR i: INTEGER;
BEGIN
    RESULT := 0.0;
    FOR i := LOW(v1) TO HIGH(v1) DO
        RESULT := RESULT + v1[i] * v2[i];
    END;
END ProductoEscalar;

PROCEDURE ProductoVectorial (v1, v2: Vector; VAR v3: Vector);
VAR i, j, k: INTEGER;
BEGIN
    v3[1] := v1[2] * v2[3] - v1[3] * v2[2];
    v3[2] := v1[3] * v2[1] - v1[1] * v2[3];
    v3[3] := v1[1] * v2[2] - v1[2] * v2[1];
END ProductoVectorial;

PROCEDURE NormalizaVector (VAR v: Vector);
VAR i, n: INTEGER;
BEGIN
    n := 0;
    FOR i := LOW(v) TO HIGH(v) DO
        n := n + v[i] * v[i];
    END;
    n := SQRT(n);
    FOR i := LOW(v) TO HIGH(v) DO
        v[i] := v[i] / n;
    END;
END NormalizaVector;

END Librería matematicas.
```

Este código es una biblioteca de funciones matemáticas en MODULA-2. La biblioteca contiene funciones para sumar vectores, calcular el producto escalar y el producto vectorial de dos vectores, y normalizar un vector.

La función `SumaVectores` toma dos vectores como parámetros y devuelve un nuevo vector que es la suma de los dos vectores. La función `ProductoEscalar` toma dos vectores como parámetros y devuelve el producto escalar de los dos vectores. La función `ProductoVectorial` toma dos vectores como parámetros y devuelve un nuevo vector que es el producto vectorial de los dos vectores. La función `NormalizaVector` toma un vector como parámetro y lo normaliza, es decir, lo convierte en un vector de longitud 1.

Este código es complejo porque contiene varias funciones diferentes que realizan cálculos matemáticos complejos. También es diferenciado porque contiene funciones para realizar cálculos diferentes, como la suma de vectores, el producto escalar y el producto vectorial.