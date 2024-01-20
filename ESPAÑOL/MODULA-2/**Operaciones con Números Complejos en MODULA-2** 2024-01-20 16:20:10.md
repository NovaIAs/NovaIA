```modula-2
MODULE Complejo;
FROM Terminal IMPORT WriteString, WriteInt, WriteLn, ReadString, ReadInt;

TYPE
  Complejo = RECORD
    real: REAL;
    imaginario: REAL;
  END;

PROCEDURE ImprimirComplejo(c: Complejo);
BEGIN
  WriteString("(");
  WriteReal(c.real, 2);
  WriteString(", ");
  WriteReal(c.imaginario, 2);
  WriteString(")");
END ImprimirComplejo;

PROCEDURE LeerComplejo(VAR c: Complejo);
BEGIN
  WriteString("Parte real: ");
  c.real := ReadReal;
  WriteString("Parte imaginaria: ");
  c.imaginario := ReadReal;
END LeerComplejo;

PROCEDURE SumarComplejos(a, b: Complejo): Complejo;
VAR
  c: Complejo;
BEGIN
  c.real := a.real + b.real;
  c.imaginario := a.imaginario + b.imaginario;
  RETURN c;
END SumarComplejos;

PROCEDURE RestarComplejos(a, b: Complejo): Complejo;
VAR
  c: Complejo;
BEGIN
  c.real := a.real - b.real;
  c.imaginario := a.imaginario - b.imaginario;
  RETURN c;
END RestarComplejos;

PROCEDURE MultiplicarComplejos(a, b: Complejo): Complejo;
VAR
  c: Complejo;
BEGIN
  c.real := a.real * b.real - a.imaginario * b.imaginario;
  c.imaginario := a.real * b.imaginario + a.imaginario * b.real;
  RETURN c;
END MultiplicarComplejos;

PROCEDURE DividirComplejos(a, b: Complejo): Complejo;
VAR
  c: Complejo;
BEGIN
  IF b.real = 0 AND b.imaginario = 0 THEN
    WriteString("Error: división por cero");
    HALT;
  END;
  c.real := (a.real * b.real + a.imaginario * b.imaginario) / (b.real * b.real + b.imaginario * b.imaginario);
  c.imaginario := (a.imaginario * b.real - a.real * b.imaginario) / (b.real * b.real + b.imaginario * b.imaginario);
  RETURN c;
END DividirComplejos;

PROCEDURE OperacionesComplejos;
VAR
  a, b, c: Complejo;
BEGIN
  WriteString("Primer complejo: "); LeerComplejo(a);
  WriteString("Segundo complejo: "); LeerComplejo(b);
  WriteString("Suma: "); ImprimirComplejo(SumarComplejos(a, b)); WriteLn;
  WriteString("Resta: "); ImprimirComplejo(RestarComplejos(a, b)); WriteLn;
  WriteString("Multiplicación: "); ImprimirComplejo(MultiplicarComplejos(a, b)); WriteLn;
  WriteString("División: "); ImprimirComplejo(DividirComplejos(a, b)); WriteLn;
END OperacionesComplejos;

BEGIN
  OperacionesComplejos;
END Complejo.
```

Este código en MODULA-2 define un tipo `Complejo` que representa números complejos, y luego define procedimientos para imprimir, leer, sumar, restar, multiplicar y dividir números complejos.

El procedimiento `ImprimirComplejo` imprime un número complejo en la consola en el formato "(real, imaginario)", donde `real` es la parte real del número y `imaginario` es la parte imaginaria.

El procedimiento `LeerComplejo` lee un número complejo de la consola y lo almacena en una variable `Complejo`.

Los procedimientos `SumarComplejos`, `RestarComplejos`, `MultiplicarComplejos` y `DividirComplejos` realizan las operaciones de suma, resta, multiplicación y división de números complejos, respectivamente.

El procedimiento `OperacionesComplejos` pide al usuario que introduzca dos números complejos, y luego imprime los resultados de las operaciones de suma, resta, multiplicación y división de los dos números complejos.

El código termina con el procedimiento `Complejo`, que llama al procedimiento `OperacionesComplejos`.