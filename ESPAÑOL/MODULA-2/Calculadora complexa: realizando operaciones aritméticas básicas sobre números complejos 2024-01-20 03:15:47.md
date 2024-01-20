```modula-2
MODULE CalculadoraCompleja;
FROM FormatString IMPORT Fmt, Out;

TYPE Complex = RECORD
    re: LONGREAL;
    im: LONGREAL;
END;

PROCEDURE Suma(VAR c1, c2: Complex): Complex;
BEGIN
    RETURN Complex(c1.re + c2.re, c1.im + c2.im);
END Suma;

PROCEDURE Resta(VAR c1, c2: Complex): Complex;
BEGIN
    RETURN Complex(c1.re - c2.re, c1.im - c2.im);
END Resta;

PROCEDURE Multiplicacion(VAR c1, c2: Complex): Complex;
BEGIN
    RETURN Complex(c1.re * c2.re - c1.im * c2.im, c1.re * c2.im + c1.im * c2.re);
END Multiplicacion;

PROCEDURE Division(VAR c1, c2: Complex): Complex;
BEGIN
    IF c2.re = 0 AND c2.im = 0 THEN
        ERROR "¡No se puede dividir por cero!";
    END;
    RETURN Complex((c1.re * c2.re + c1.im * c2.im) / (SQR(c2.re) + SQR(c2.im)),
                   (c1.im * c2.re - c1.re * c2.im) / (SQR(c2.re) + SQR(c2.im)));
END Division;

PROCEDURE ImprimirComplejo(c: Complex);
BEGIN
    Fmt("(%d, %d)", c.re, c.im);
END ImprimirComplejo;

PROCEDURE main;
VAR c1, c2: Complex;
    op: CHAR;
BEGIN
    WHILE TRUE DO
        c1.re := 0;
        c1.im := 0;
        c2.re := 0;
        c2.im := 0;
        Out("Introduce el primer número complejo (re, im): ");
        READ(c1.re, c1.im);
        ImprimirComplejo(c1);
        OutLn;
        Out("Introduce el segundo número complejo (re, im): ");
        READ(c2.re, c2.im);
        ImprimirComplejo(c2);
        OutLn;
        Out("Introduce el operador (+, -, *, /): ");
        READ(op, 0);
        CASE op OF
            '+': ImprimirComplejo(Suma(c1, c2));
            '-': ImprimirComplejo(Resta(c1, c2));
            '*': ImprimirComplejo(Multiplicacion(c1, c2));
            '/': ImprimirComplejo(Division(c1, c2));
            OTHERWISE: ERROR "¡Operador no válido!";
        END;
        OutLn;
    END;
END main;
END CalculadoraCompleja.
```

Este código es una calculadora compleja en MODULA-2. Permite al usuario introducir dos números complejos y realizar operaciones aritméticas básicas (+, -, *, /) con ellos.

El código primero define el tipo `Complex`, que es un registro que contiene dos números de punto flotante, `re` e `im`, que representan la parte real e imaginaria del número complejo, respectivamente.

A continuación, se definen cuatro procedimientos: Suma, Resta, Multiplicacion y Division, que realizan las operaciones aritméticas básicas con números complejos.

El procedimiento `ImprimirComplejo` imprime un número complejo en el formato "(re, im)".

El procedimiento `main` es el punto de entrada del programa. Primero crea dos variables de tipo `Complex`, c1 y c2, y les asigna el valor 0. Luego, solicita al usuario que introduzca dos números complejos. Imprime el resultado de las operaciones aritméticas básicas con los dos números utilizando los procedimientos definidos anteriormente.

El programa seguirá repitiendo este proceso hasta que el usuario introduzca un operador no válido. En ese caso, se mostrará un mensaje de error y el programa terminará.