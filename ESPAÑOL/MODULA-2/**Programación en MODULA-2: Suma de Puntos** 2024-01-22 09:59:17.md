```modula-2
MODULE Matrix;
FROM FormatString IMPORT Format;
FROM Terminal IMPORT GetLine;

TYPE Point = ARRAY [1..2] OF LONGINT;
VAR p1, p2: Point;

PROCEDURE ReadPoint(VAR p: Point);
VAR str: ARRAY [0..100] OF CHAR;
BEGIN
    Format(str, "Introduzca dos números separados por una coma: ");
    Write(str);
    GetLine(str);
    Read(str, p[1], p[2]);
END ReadPoint;

PROCEDURE PrintPoint(p: Point);
VAR str: ARRAY [0..100] OF CHAR;
BEGIN
    Format(str, "(%d, %d)", p[1], p[2]);
    Write(str);
END PrintPoint;

PROCEDURE AddPoint(p1, p2: Point; VAR p3: Point);
BEGIN
    p3[1] := p1[1] + p2[1];
    p3[2] := p1[2] + p2[2];
END AddPoint;

BEGIN
    ReadPoint(p1);
    ReadPoint(p2);
    PrintPoint(p1);
    Write(" + ");
    PrintPoint(p2);
    Write(" = ");
    VAR p3: Point;
    AddPoint(p1, p2, p3);
    PrintPoint(p3);
    WriteLn;
END Matrix.
```

Explicación:

* El módulo Matrix define un tipo de dato llamado Point, que es una matriz de dos enteros largos.

* El procedimiento ReadPoint() lee un punto desde la entrada estándar.

* El procedimiento PrintPoint() imprime un punto a la salida estándar.

* El procedimiento AddPoint() suma dos puntos y devuelve el resultado en un tercer punto.

* El programa principal lee dos puntos desde la entrada estándar, los suma y imprime el resultado a la salida estándar.