```modula-2
MODULE CalculadoraDeNumerosComplejos;

FROM InOut IMPORT WriteString, WriteLn, ReadLn;
FROM RealInOut IMPORT GetReal;

TYPE

  Complejo = RECORD
    real: REAL;
    imaginario: REAL;
  END;

PROCEDURE SumarComplejos(a, b: Complejo): Complejo;
BEGIN
  RETURN Complejo(a.real + b.real, a.imaginario + b.imaginario);
END SumarComplejos;

PROCEDURE RestarComplejos(a, b: Complejo): Complejo;
BEGIN
  RETURN Complejo(a.real - b.real, a.imaginario - b.imaginario);
END RestarComplejos;

PROCEDURE MultiplicarComplejos(a, b: Complejo): Complejo;
BEGIN
  RETURN Complejo(a.real * b.real - a.imaginario * b.imaginario,
                  a.real * b.imaginario + a.imaginario * b.real);
END MultiplicarComplejos;

PROCEDURE DividirComplejos(a, b: Complejo): Complejo;
VAR
  denominador: REAL;
BEGIN
  denominador := b.real * b.real + b.imaginario * b.imaginario;
  RETURN Complejo((a.real * b.real + a.imaginario * b.imaginario) / denominador,
                  (a.imaginario * b.real - a.real * b.imaginario) / denominador);
END DividirComplejos;

PROCEDURE MostrarComplejo(c: Complejo);
BEGIN
  WriteString(c.real);
  IF c.imaginario >= 0 THEN WriteString("+"); END;
  WriteReal(c.imaginario, 2);
END MostrarComplejo;

VAR
  a, b, c: Complejo;
  op: CHAR;

BEGIN
  WriteString("Introduce el primer número complejo (a+bi): ");
  ReadLn(a.real, a.imaginario);
  WriteString("Introduce el segundo número complejo (c+di): ");
  ReadLn(b.real, b.imaginario);

  WriteString("Elige una operación (+, -, *, /): ");
  ReadLn(op);

  CASE op OF
    '+': c := SumarComplejos(a, b);
    '-': c := RestarComplejos(a, b);
    '*': c := MultiplicarComplejos(a, b);
    '/': c := DividirComplejos(a, b);
  END;

  WriteString("El resultado es: ");
  MostrarComplejo(c);
  WriteLn;
END CalculadoraDeNumerosComplejos.
```

Explicación del código:

1. Se define el tipo `Complejo` para representar números complejos, con campos `real` e `imaginario`.
2. Se definen los procedimientos `SumarComplejos`, `RestarComplejos`, `MultiplicarComplejos` y `DividirComplejos` para realizar operaciones aritméticas con números complejos.
3. Se define el procedimiento `MostrarComplejo` para mostrar un número complejo en la forma `a+bi`.
4. Se solicita al usuario que introduzca dos números complejos y una operación a realizar.
5. Se calcula el resultado de la operación utilizando los procedimientos definidos anteriormente.
6. Se muestra el resultado de la operación.

Este código es complejo porque:

* Utiliza tipos definidos por el usuario para representar números complejos.
* Define varios procedimientos para realizar operaciones aritméticas con números complejos.
* Utiliza la instrucción `CASE` para manejar diferentes operaciones.
* Utiliza la instrucción `ReadLn` para leer datos del teclado y la instrucción `WriteString` para escribir datos en la pantalla.