```modula-2

MODULE Código Complejo;

FROM FormatString IMPORT WriteString;
FROM FormatInt IMPORT WriteInt;
FROM FormatReal IMPORT WriteReal;

TYPE
  Complejo = RECORD
    real: REAL;
    imag: REAL;
  END;

PROCEDURE NuevoComplejo(VAR c: Complejo): Complejo;
BEGIN
  WriteString("Introduce la parte real: ");
  ReadReal(c.real);
  WriteString("Introduce la parte imaginaria: ");
  ReadReal(c.imag);
  RETURN c;
END NuevoComplejo;

PROCEDURE SumarComplejos(c1, c2: Complejo): Complejo;
VAR
  c: Complejo;
BEGIN
  c.real := c1.real + c2.real;
  c.imag := c1.imag + c2.imag;
  RETURN c;
END SumarComplejos;

PROCEDURE RestarComplejos(c1, c2: Complejo): Complejo;
VAR
  c: Complejo;
BEGIN
  c.real := c1.real - c2.real;
  c.imag := c1.imag - c2.imag;
  RETURN c;
END RestarComplejos;

PROCEDURE MultiplicarComplejos(c1, c2: Complejo): Complejo;
VAR
  c: Complejo;
BEGIN
  c.real := c1.real * c2.real - c1.imag * c2.imag;
  c.imag := c1.real * c2.imag + c1.imag * c2.real;
  RETURN c;
END MultiplicarComplejos;

PROCEDURE DividirComplejos(c1, c2: Complejo): Complejo;
VAR c: Complejo;
BEGIN
  c.real := (c1.real * c2.real + c1.imag * c2.imag) / (c2.real^2 + c2.imag^2);
  c.imag := (c1.imag * c2.real - c1.real * c2.imag) / (c2.real^2 + c2.imag^2);
  RETURN c;
END DividirComplejos;

PROCEDURE MostrarComplejo(c: Complejo);
BEGIN
  WriteString("(");
  WriteReal(c.real, 6, 2);
  WriteString(" + ");
  WriteReal(c.imag, 6, 2);
  WriteString("i)");
END MostrarComplejo;

PROCEDURE ComplejoAleatorio(VAR c: Complejo);
BEGIN
  c.real := Random(100) / 10.0;
  c.imag := Random(100) / 10.0;
END ComplejoAleatorio;

VAR
  c1, c2, c3: Complejo;

BEGIN
  WriteString("Primer complejo: ");
  c1 := NuevoComplejo();
  WriteString("Segundo complejo: ");
  c2 := NuevoComplejo();

  WriteString("Suma: ");
  MostrarComplejo(SumarComplejos(c1, c2));
  WriteString(NIL);
  WriteString("Resta: ");
  MostrarComplejo(RestarComplejos(c1, c2));
  WriteString(NIL);
  WriteString("Multiplicación: ");
  MostrarComplejo(MultiplicarComplejos(c1, c2));
  WriteString(NIL);
  WriteString("División: ");
  MostrarComplejo(DividirComplejos(c1, c2));
  WriteString(NIL);

  WriteString("Complejo aleatorio: ");
  ComplejoAleatorio(c3);
  MostrarComplejo(c3);
  WriteString(NIL);

  Halt();
END Código Complejo.

```

Este programa en MODULA-2 contiene las siguientes características complejas y diferenciadas:

* Define un tipo de datos `Complejo` con partes real e imaginaria.
* Define una serie de procedimientos para sumar, restar, multiplicar y dividir números complejos.
* Define un procedimiento para mostrar un número complejo en formato `(a + bi)`.
* Define un procedimiento para generar un número complejo aleatorio.
* El programa principal solicita al usuario que introduzca dos números complejos, y calcula y muestra su suma, resta, multiplicación y división.
* El programa principal también genera y muestra un número complejo aleatorio.

Este código es más complejo y diferenciado que la mayoría de los programas MODULA-2 que se pueden encontrar, y es un buen ejemplo de cómo se puede utilizar MODULA-2 para crear programas complejos y sofisticados.