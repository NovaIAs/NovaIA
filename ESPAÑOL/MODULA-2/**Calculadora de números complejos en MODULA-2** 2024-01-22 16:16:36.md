```modula-2
MODULE CalculadoraCompleja;

FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT Format;

TYPE Complejo = RECORD
    real: REAL;
    imaginario: REAL;
END;

PROCEDURE Compite(x, y: Complejo): Complejo;
BEGIN
    RETURN Complejo(x.real * y.real - x.imaginario * y.imaginario,
                    x.real * y.imaginario + x.imaginario * y.real);
END Compite;

PROCEDURE CompSuma(x, y: Complejo): Complejo;
BEGIN
    RETURN Complejo(x.real + y.real, x.imaginario + y.imaginario);
END CompSuma;

PROCEDURE CompResta(x, y: Complejo): Complejo;
BEGIN
    RETURN Complejo(x.real - y.real, x.imaginario - y.imaginario);
END CompResta;

PROCEDURE CompPrint(x: Complejo);
BEGIN
    FormatString("(%f, %f)i", x.real, x.imaginario);
    Format(0);
END CompPrint;

VAR
    x1, x2: Complejo;
BEGIN
    x1 := Complejo(3.5, 2.7);
    x2 := Complejo(1.2, 4.3);

    CompPrint(x1);
    FormatString(" + ");
    CompPrint(x2);
    FormatString(" = ");
    CompPrint(CompSuma(x1, x2));
    Format(1);

    CompPrint(x1);
    FormatString(" - ");
    CompPrint(x2);
    FormatString(" = ");
    CompPrint(CompResta(x1, x2));
    Format(1);

    CompPrint(x1);
    FormatString(" * ");
    CompPrint(x2);
    FormatString(" = ");
    CompPrint(Compite(x1, x2));
    Format(1);
END CalculadoraCompleja.
```

Explicación:

* El módulo `CalculadoraCompleja` define un tipo de dato llamado `Complejo` para representar números complejos. Un número complejo tiene dos partes: una real y una imaginaria.
* El módulo también define tres procedimientos: `Compite`, `CompSuma` y `CompResta` para realizar operaciones aritméticas con números complejos.
* El procedimiento `CompPrint` se utiliza para imprimir un número complejo en la consola.
* La parte principal del programa crea dos variables de tipo `Complejo` llamadas `x1` y `x2`.
* El programa utiliza los procedimientos definidos en el módulo para realizar operaciones aritméticas con `x1` y `x2` y luego imprime los resultados en la consola.