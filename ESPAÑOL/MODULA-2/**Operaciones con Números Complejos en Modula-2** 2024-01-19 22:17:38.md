```modula-2
MODULE Complejo;
FROM FormatString IMPORT WriteF, WriteLn;
FROM Terminal IMPORT WriteChar, WriteString, ReadChar;

TYPE Complejo = RECORD
real: REAL;
imaginario: REAL;
END;

PROCEDURE ComplejoNuevo(real, imaginario: REAL): Complejo;
VAR c: Complejo;
BEGIN
c.real := real;
c.imaginario := imaginario;
RETURN c;
END ComplejoNuevo;

PROCEDURE ComplejoSumar(c1, c2: Complejo): Complejo;
VAR c: Complejo;
BEGIN
c.real := c1.real + c2.real;
c.imaginario := c1.imaginario + c2.imaginario;
RETURN c;
END ComplejoSumar;

PROCEDURE ComplejoRestar(c1, c2: Complejo): Complejo;
VAR c: Complejo;
BEGIN
c.real := c1.real - c2.real;
c.imaginario := c1.imaginario - c2.imaginario;
RETURN c;
END ComplejoRestar;

PROCEDURE ComplejoMultiplicar(c1, c2: Complejo): Complejo;
VAR c: Complejo;
BEGIN
c.real := c1.real * c2.real - c1.imaginario * c2.imaginario;
c.imaginario := c1.real * c2.imaginario + c1.imaginario * c2.real;
RETURN c;
END ComplejoMultiplicar;

PROCEDURE ComplejoDividir(c1, c2: Complejo): Complejo;
VAR c: Complejo;
BEGIN
c.real := (c1.real * c2.real + c1.imaginario * c2.imaginario) / (c2.real * c2.real + c2.imaginario * c2.imaginario);
c.imaginario := (c1.imaginario * c2.real - c1.real * c2.imaginario) / (c2.real * c2.real + c2.imaginario * c2.imaginario);
RETURN c;
END ComplejoDividir;

PROCEDURE ComplejoImprimir(c: Complejo);
BEGIN
WriteF("(%g, %g)", [c.real, c.imaginario]);
END ComplejoImprimir;

VAR c1, c2, c3: Complejo;

BEGIN
WriteLn("Ingrese el primer número complejo (real e imaginario): ");
ReadChar;
WriteString("real: ");
ReadR(c1.real);
WriteString("imaginario: ");
ReadR(c1.imaginario);

WriteLn("Ingrese el segundo número complejo (real e imaginario): ");
ReadChar;
WriteString("real: ");
ReadR(c2.real);
WriteString("imaginario: ");
ReadR(c2.imaginario);

WriteLn("Suma: ");
c3 := ComplejoSumar(c1, c2);
ComplejoImprimir(c3);
WriteLn;

WriteLn("Resta: ");
c3 := ComplejoRestar(c1, c2);
ComplejoImprimir(c3);
WriteLn;

WriteLn("Multiplicación: ");
c3 := ComplejoMultiplicar(c1, c2);
ComplejoImprimir(c3);
WriteLn;

WriteLn("División: ");
c3 := ComplejoDividir(c1, c2);
ComplejoImprimir(c3);
WriteLn;
END Complejo.
```

Explicación:

* El módulo `Complejo` define un tipo `Complejo` que representa un número complejo con partes real e imaginaria.
* Las funciones `ComplejoNuevo`, `ComplejoSumar`, `ComplejoRestar`, `ComplejoMultiplicar` y `ComplejoDividir` realizan las operaciones básicas de suma, resta, multiplicación y división de números complejos.
* La función `ComplejoImprimir` imprime un número complejo en formato "(real, imaginario)".
* La función `main` solicita al usuario que ingrese dos números complejos y luego imprime los resultados de las operaciones de suma, resta, multiplicación y división de dichos números.