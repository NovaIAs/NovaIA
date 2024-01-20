```modula-2
MODULE CalculadoraCompleja;

FROM FormatString IMPORT FormatString, FmtStringBuf;

TYPE Complejo = RECORD
    real: REAL;
    imag: REAL;
END;

VAR sumar, restar, multiplicar, dividir: Complejo;
VAR BUF: FmtStringBuf;

PROCEDURE Suma(a, b: Complejo; VAR res: Complejo):
VAR
    fmt: ARRAY 80 OF CHAR;
BEGIN
    FormatString(fmt, "Suma de (%g, %g) y (%g, %g) da %g, %g");
    BUF := FmtStringBuf.FromChars(fmt);
    BUF.Format(a.real, a.imag, b.real, b.imag,
               (a.real + b.real), (a.imag + b.imag));
    res.real := a.real + b.real;
    res.imag := a.imag + b.imag;
END Suma;

PROCEDURE Resta(a, b: Complejo; VAR res: Complejo):
VAR
    fmt: ARRAY 80 OF CHAR;
BEGIN
    FormatString(fmt, "Resta de (%g, %g) y (%g, %g) da %g, %g");
    BUF := FmtStringBuf.FromChars(fmt);
    BUF.Format(a.real, a.imag, b.real, b.imag,
               (a.real - b.real), (a.imag - b.imag));
    res.real := a.real - b.real;
    res.imag := a.imag - b.imag;
END Resta;

PROCEDURE Multiplica(a, b: Complejo; VAR res: Complejo):
VAR
    fmt: ARRAY 80 OF CHAR;
BEGIN
    FormatString(fmt, "Multiplicación de (%g, %g) por (%g, %g) da %g, %g");
    BUF := FmtStringBuf.FromChars(fmt);
    BUF.Format(a.real, a.imag, b.real, b.imag,
               (a.real * b.real - a.imag * b.imag),
               (a.real * b.imag + a.imag * b.real));
    res.real := a.real * b.real - a.imag * b.imag;
    res.imag := a.real * b.imag + a.imag * b.real;
END Multiplica;

PROCEDURE Divide(a, b: Complejo; VAR res: Complejo):
VAR
    fmt: ARRAY 80 OF CHAR;
BEGIN
    IF b.real = 0 AND b.imag = 0 THEN
        Write("División por cero");
    ELSE
        FormatString(fmt, "División de (%g, %g) por (%g, %g) da %g, %g");
        BUF := FmtStringBuf.FromChars(fmt);
        BUF.Format(a.real, a.imag, b.real, b.imag,
                   (a.real * b.real + a.imag * b.imag) /
                       (b.real * b.real + b.imag * b.imag),
                   (a.imag * b.real - a.real * b.imag) /
                       (b.real * b.real + b.imag * b.imag));
        res.real := (a.real * b.real + a.imag * b.imag) /
                       (b.real * b.real + b.imag * b.imag);
        res.imag := (a.imag * b.real - a.real * b.imag) /
                       (b.real * b.real + b.imag * b.imag);
    END;
END Divide;

BEGIN
    sumar.real := 3.3; sumar.imag := 5.4;
    restar.real := 1.7; restar.imag := 8.3;
    multiplicar.real := 2.1; multiplicar.imag := 7.2;
    dividir.real := 5.6; dividir.imag := 3.2;
    Suma(sumar, restar, res);
    Write("Suma -> ", res.real, ", ", res.imag);
    WriteLn;
    Resta(sumar, restar, res);
    Write("Resta -> ", res.real, ", ", res.imag);
    WriteLn;
    Multiplica(sumar, multiplicar, res);
    Write("Multiplicación -> ", res.real, ", ", res.imag);
    WriteLn;
    Divide(sumar, dividir, res);
    Write("División -> ", res.real, ", ", res.imag);
    WriteLn;
END CalculadoraCompleja.
```

Explicación del código:

1. Se definen los tipos de datos `Complejo` y `FmtStringBuf` para representar números complejos y realizar formatos de cadenas, respectivamente.

2. Se declaran cuatro variables de tipo `Complejo` llamadas `sumar`, `restar`, `multiplicar` y `dividir` para almacenar los resultados de las operaciones aritméticas.

3. Se define una variable `BUF` de tipo `FmtStringBuf` para almacenar cadenas formateadas.

4. Se definen cuatro procedimientos llamados `Suma`, `Resta`, `Multiplica` y `Divide` que realizan las operaciones aritméticas básicas (suma, resta, multiplicación y división) sobre números complejos.

5. En el procedimiento `Suma`, se utiliza la función `FormatString` para crear una cadena formateada con los valores de los números complejos a sumar y el resultado de la suma. A continuación, se utiliza la variable `BUF` para formatear la cadena y mostrar el resultado en la consola.

6. Los procedimientos `Resta`, `Multiplica` y `Divide` siguen un enfoque similar, utilizando cadenas formateadas para mostrar los resultados de las operaciones aritméticas.

7. En la parte final del código, se inicializan las variables `sumar`, `restar`, `multiplicar` y `dividir` con valores complejos específicos.

8. Se invocan los procedimientos `Suma`, `Resta`, `Multiplica` y `Divide` para realizar las operaciones aritméticas y mostrar los resultados en la consola.

Al ejecutar este código, se obtendrán los resultados de las operaciones aritméticas realizadas sobre los números complejos especificados.