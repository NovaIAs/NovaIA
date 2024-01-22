```modula-2
MODULE CalculadoraComplejos;

FROM FormatString IMPORT Format, Aligned;
FROM Terminal IMPORT WriteString, WriteLn, ReadLine;

TYPE Complejo = RECORD
    real: REAL;
    imaginario: REAL;
END;

PROCEDURE EscribirComplejo(complejo: Complejo);
VAR
    buffer: ARRAY[0..32] OF CHAR;
BEGIN
    Format(buffer, " (%f + %fi)", complejo.real, complejo.imaginario);
    WriteString(buffer);
END EscribirComplejo;

PROCEDURE LeerComplejo(complejo: VAR Complejo);
VAR
    buffer: ARRAY[0..32] OF CHAR;
BEGIN
    ReadLine(buffer);
    Format(complejo.real, complejo.imaginario, buffer, " (%f + %fi)");
END LeerComplejo;

PROCEDURE SumarComplejos(complejo1, complejo2: Complejo): Complejo;
BEGIN
    RETURN Complejo(complejo1.real + complejo2.real, complejo1.imaginario + complejo2.imaginario);
END SumarComplejos;

PROCEDURE RestarComplejos(complejo1, complejo2: Complejo): Complejo;
BEGIN
    RETURN Complejo(complejo1.real - complejo2.real, complejo1.imaginario - complejo2.imaginario);
END RestarComplejos;

PROCEDURE MultiplicarComplejos(complejo1, complejo2: Complejo): Complejo;
BEGIN
    RETURN Complejo(complejo1.real * complejo2.real - complejo1.imaginario * complejo2.imaginario,
                   complejo1.real * complejo2.imaginario + complejo1.imaginario * complejo2.real);
END MultiplicarComplejos;

PROCEDURE DividirComplejos(complejo1, complejo2: Complejo): Complejo;
BEGIN
    IF complejo2.real = 0.0 AND complejo2.imaginario = 0.0 THEN
        ERROR "División por cero";
    END;
    RETURN Complejo((complejo1.real * complejo2.real + complejo1.imaginario * complejo2.imaginario) /
                       (complejo2.real * complejo2.real + complejo2.imaginario * complejo2.imaginario),
                   (complejo1.imaginario * complejo2.real - complejo1.real * complejo2.imaginario) /
                       (complejo2.real * complejo2.real + complejo2.imaginario * complejo2.imaginario));
END DividirComplejos;

PROCEDURE MenuPrincipal();
VAR
    opcion: CHAR;
    complejo1, complejo2, resultado: Complejo;
BEGIN
    REPEAT
        WriteString(ALIGNED("1. Sumar complejos", 30));
        WriteString(ALIGNED("2. Restar complejos", 30));
        WriteString(ALIGNED("3. Multiplicar complejos", 30));
        WriteString(ALIGNED("4. Dividir complejos", 30));
        WriteString(ALIGNED("5. Salir", 30));
        WriteString("-> ");
        opcion := ReadLine()[0];
        CASE opcion OF
            '1':
                EscribirComplejo(complejo1);
                WriteString(" + ");
                EscribirComplejo(complejo2);
                WriteString(" = ");
                EscribirComplejo(SumarComplejos(complejo1, complejo2));
                WriteLn;
            '2':
                EscribirComplejo(complejo1);
                WriteString(" - ");
                EscribirComplejo(complejo2);
                WriteString(" = ");
                EscribirComplejo(RestarComplejos(complejo1, complejo2));
                WriteLn;
            '3':
                EscribirComplejo(complejo1);
                WriteString(" * ");
                EscribirComplejo(complejo2);
                WriteString(" = ");
                EscribirComplejo(MultiplicarComplejos(complejo1, complejo2));
                WriteLn;
            '4':
                EscribirComplejo(complejo1);
                WriteString(" / ");
                EscribirComplejo(complejo2);
                WriteString(" = ");
                EscribirComplejo(DividirComplejos(complejo1, complejo2));
                WriteLn;
        END;
    UNTIL opcion = '5';
END MenuPrincipal;

BEGIN
    MenuPrincipal();
END CalculadoraComplejos.
```

Explicación:

* El código está escrito en MODULA-2, un lenguaje de programación estructurado desarrollado en los años 80.
* El programa es una calculadora de números complejos que permite sumar, restar, multiplicar y dividir números complejos.
* El programa está organizado en varios módulos:
    * El módulo `CalculadoraComplejos` es el módulo principal del programa.
    * El módulo `FormatString` proporciona funciones para formatear cadenas de caracteres.
    * El módulo `Terminal` proporciona funciones para interactuar con la terminal.
* El programa define un tipo de dato `Complejo` que representa un número complejo.
* El programa define varias funciones para operar con números complejos:
    * `EscribirComplejo` escribe un número complejo en la terminal.
    * `LeerComplejo` lee un número complejo de la terminal.
    * `SumarComplejos` suma dos números complejos.
    * `RestarComplejos` resta dos números complejos.
    * `MultiplicarComplejos` multiplica dos números complejos.
    * `DividirComplejos` divide dos números complejos.
* El programa define la función `MenuPrincipal` que muestra un menú de opciones al usuario y le permite seleccionar una opción.
* El programa invoca a la función `MenuPrincipal` para iniciar el programa.