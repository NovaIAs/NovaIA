```modula-2
MODULE CalculadoraCompleja;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT Printf;

TYPE Complex = RECORD
    real: REAL;
    imag: REAL;
END;

PROCEDURE SumaComplejos(c1: Complex; c2: Complex): Complex;
VAR
    result: Complex;
BEGIN
    result.real := c1.real + c2.real;
    result.imag := c1.imag + c2.imag;
    RETURN result;
END SumaComplejos;

PROCEDURE RestaComplejos(c1: Complex; c2: Complex): Complex;
VAR
    result: Complex;
BEGIN
    result.real := c1.real - c2.real;
    result.imag := c1.imag - c2.imag;
    RETURN result;
END RestaComplejos;

PROCEDURE MultiplicacionComplejos(c1: Complex; c2: Complex): Complex;
VAR
    result: Complex;
BEGIN
    result.real := c1.real * c2.real - c1.imag * c2.imag;
    result.imag := c1.real * c2.imag + c1.imag * c2.real;
    RETURN result;
END MultiplicacionComplejos;

PROCEDURE DivisionComplejos(c1: Complex; c2: Complex): Complex;
VAR
    result: Complex;
    denom: REAL;
BEGIN
    denom := c2.real * c2.real + c2.imag * c2.imag;
    result.real := (c1.real * c2.real + c1.imag * c2.imag) / denom;
    result.imag := (c1.imag * c2.real - c1.real * c2.imag) / denom;
    RETURN result;
END DivisionComplejos;

PROCEDURE LeerComplejo(c: POINTER TO Complex): INTEGER;
VAR
    str: ARRAY [1..80] OF CHAR;
    n: INTEGER;
BEGIN
    n := GetLine(str);
    IF n > 0 THEN
        c^.real := StrToReal(str);
        c^.imag := StrToReal(str+Pos("i",str) + 1);
        RETURN n;
    ELSE
        RETURN 0;
    END;
END LeerComplejo;

PROCEDURE ImprimirComplejo(c: Complex);
VAR
    str: ARRAY [1..80] OF CHAR;
BEGIN
    FormatString(str,"(%g, %g)",c.real,c.imag);
    Printf("Complejo: %s\n",str);
END ImprimirComplejo;

VAR
    c1, c2: Complex;
    opcion: CHAR;
BEGIN
    REPEAT
        Printf("Calculadora de números complejos\n");
        Printf("1. Suma\n");
        Printf("2. Resta\n");
        Printf("3. Multiplicación\n");
        Printf("4. División\n");
        Printf("5. Salir\n");
        opcion := GetChar;

        CASE opcion OF
            '1':
                Printf("Primer complejo:\n");
                IF LeerComplejo(c1) > 0 THEN
                    Printf("Segundo complejo:\n");
                    IF LeerComplejo(c2) > 0 THEN
                        ImprimirComplejo(SumaComplejos(c1,c2));
                    END;
                END;
            '2':
                Printf("Primer complejo:\n");
                IF LeerComplejo(c1) > 0 THEN
                    Printf("Segundo complejo:\n");
                    IF LeerComplejo(c2) > 0 THEN
                        ImprimirComplejo(RestaComplejos(c1,c2));
                    END;
                END;
            '3':
                Printf("Primer complejo:\n");
                IF LeerComplejo(c1) > 0 THEN
                    Printf("Segundo complejo:\n");
                    IF LeerComplejo(c2) > 0 THEN
                        ImprimirComplejo(MultiplicacionComplejos(c1,c2));
                    END;
                END;
            '4':
                Printf("Primer complejo:\n");
                IF LeerComplejo(c1) > 0 THEN
                    Printf("Segundo complejo:\n");
                    IF LeerComplejo(c2) > 0 THEN
                        ImprimirComplejo(DivisionComplejos(c1,c2));
                    END;
                END;
        END;
    UNTIL opcion = '5';
END CalculadoraCompleja.
```

Este código implementa una calculadora de números complejos en Modula-2. El programa primero muestra un menú de opciones al usuario, y luego le pide que introduzca dos números complejos. Una vez que el usuario ha introducido los dos números complejos, el programa realiza la operación seleccionada y muestra el resultado.

El programa utiliza el tipo de datos Complex para representar números complejos. El tipo Complex tiene dos campos, real e imag, que representan las partes real e imaginaria del número complejo, respectivamente.

El programa también utiliza una serie de procedimientos para realizar operaciones en números complejos. Estos procedimientos son SumaComplejos, RestaComplejos, MultiplicacionComplejos y DivisionComplejos. Cada uno de estos procedimientos toma dos números complejos como argumentos y devuelve un nuevo número complejo que es el resultado de la operación correspondiente.

El programa también utiliza los procedimientos LeerComplejo e ImprimirComplejo para leer y escribir números complejos desde y hacia la consola. El procedimiento LeerComplejo lee una línea de texto de la consola y la convierte en un número complejo. El procedimiento ImprimirComplejo toma un número complejo como argumento y lo imprime en la consola.

El programa principal del programa es el procedimiento CalculadoraCompleja. Este procedimiento muestra el menú de opciones al usuario y luego llama a los procedimientos LeerComplejo e ImprimirComplejo para leer y escribir números complejos. También llama a los procedimientos SumaComplejos, RestaComplejos, MultiplicacionComplejos y DivisionComplejos para realizar las operaciones seleccionadas por el usuario.