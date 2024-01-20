```modula-2
MODULE Complejidad;
FROM FormatString IMPORT FormatString;
FROM Storage IMPORT New;

TYPE Cadena = ARRAY 256 OF CHAR;

PROCEDURE NuevoSecuencia(): ARRAY OF CHAR;
VAR c: ARRAY 256 OF CHAR;
    p: ARRAY OF CHAR;
    i: CARDINAL;
BEGIN
    c := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    New(p, 26*2);
    i := 0;
    WHILE i < 26 DO
        p[i] := c[i];
        p[26+i] := c[i+1];
        i := i + 1;
    END;

    RETURN p;
END NuevoSecuencia;

VAR s: ARRAY OF CHAR;
    in: ARRAY OF CHAR;
    out: ARRAY OF CHAR;
    l: CARDINAL;

BEGIN
    s := NuevoSecuencia();
    in := "ABCDEFGHIJ";
    l := LENGTH(in) - 1;
    out := ARRAY OF CHAR(l);

    FOR i FROM 0 TO l DO
        out[i] := s[ORD(in[i])-ORD('A')]
    END;

    FormatString('Original : "%S"\n', ARRAY OF CHAR(in));
    FormatString('Cifrado  : "%S"\n', ARRAY OF CHAR(out));
END Complejidad.
```

Explicación:

* El módulo `Complejidad` define un tipo personalizado llamado `Cadena` que es una matriz de 256 caracteres.
* El procedimiento `NuevoSecuencia()` crea una secuencia de caracteres encriptada concatenando el alfabeto dos veces.
* La variable `s` se inicializa con la secuencia creada por `NuevoSecuencia()`.
* La variable `in` se inicializa con una cadena de texto que se quiere cifrar.
* La variable `l` se inicializa con la longitud de la cadena de texto `in`.
* La variable `out` se inicializa con una matriz de caracteres vacía de tamaño `l`.
* El bucle `FOR` recorre la cadena de texto `in` y cifra cada carácter usando la secuencia `s`.
* El procedimiento `FormatString()` se utiliza para imprimir la cadena de texto original y la cadena de texto cifrada.