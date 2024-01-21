```modula-2

MODULE TiposDatos;
FROM Formatos IMPORTA Leer;

TYPE
    TipoBool = BOOLEAN;
    TipoChar = CHAR;
    TipoShortInt = SHORTINT;
    TipoInt = INTEGER;
    TipoLongInt = LONGINT;
    TipoReal = REAL;
    TipoArray = ARRAY 0..9 OF INTEGER;
    TipoString = STRING;
    TipoPointer = POINTER TO INTEGER;
    TipoRecord = RECORD
        a: INTEGER;
        b: REAL;
        c: BOOLEAN;
    END;

PROCEDURE ImprimirEntero(entero: INTEGER);
BEGIN
    Write(entero);
END ImprimirEntero;

PROCEDURE ImprimirReal(real: REAL);
BEGIN
    Write(real);
END ImprimirReal;

PROCEDURE ImprimirCadena(cadena: STRING);
BEGIN
    Write(cadena);
END ImprimirCadena;

PROCEDURE ImprimirCaracter(caracter: CHAR);
BEGIN
    Write(caracter);
END ImprimirCaracter;

PROCEDURE ImprimirBooleano(booleano: BOOLEAN);
BEGIN
    IF booleano THEN
        Write("Verdadero")
    ELSE
        Write("Falso")
    END;
END ImprimirBooleano;

PROCEDURE ImprimirArreglo(arreglo: ARRAY OF INTEGER);
BEGIN
    Write("Arreglo: ");
    FOR i FROM 0 TO HIGH(arreglo) DO
        Write(arreglo[i]);
        IF i < HIGH(arreglo) THEN
            Write(", ");
        END;
    END;
END ImprimirArreglo;

PROCEDURE ImprimirRegistro(registro: TipoRecord);
BEGIN
    Write("Registro: {");
    ImprimirEntero(registro.a);
    Write(", ");
    ImprimirReal(registro.b);
    Write(", ");
    ImprimirBooleano(registro.c);
    Write("}");
END ImprimirRegistro;

PROCEDURE ImprimirPuntero(puntero: POINTER TO INTEGER);
BEGIN
    IF puntero = NIL THEN
        Write("NULL")
    ELSE
        ImprimirEntero(*puntero);
    END;
END ImprimirPuntero;

BEGIN
    ImprimirEntero(123);
    ImprimirReal(3.14);
    ImprimirCadena("Hola mundo!");
    ImprimirCaracter('A');
    ImprimirBooleano(TRUE);
    ImprimirArreglo([1, 2, 3, 4, 5]);
    ImprimirRegistro(TipoRecord(10, 20.5, TRUE));
    ImprimirPuntero(NIL);
    Read;
END TiposDatos.
```

Explicación del código:

* El código define varios tipos de datos, incluyendo tipos básicos como entero, real, carácter y booleano, así como tipos compuestos como arreglos, registros y punteros.
* El código también define varios procedimientos para imprimir los valores de cada tipo de datos.
* El procedimiento principal del programa crea variables de cada tipo de datos y llama a los procedimientos correspondientes para imprimir sus valores.
* El código utiliza la función `Read` del módulo `Formatos` para esperar a que el usuario presione una tecla antes de terminar el programa.

El código es complejo porque define y utiliza varios tipos de datos y procedimientos diferentes. Sin embargo, también es muy bien organizado y fácil de entender.