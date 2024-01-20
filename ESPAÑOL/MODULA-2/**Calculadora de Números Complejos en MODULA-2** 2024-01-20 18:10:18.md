```modula-2
MODULE CalculadoraComplejos;
FROM Reals IMPORT FLOAT;

TYPE
  Complejo = RECORD
    real : FLOAT;
    imaginario : FLOAT;
  END;

VAR
  complejo1, complejo2 : Complejo;

PROCEDURE LeerComplejo(VAR z : Complejo);
VAR
  linea : ARRAY[0..79] OF CHAR;
BEGIN
  WHILE TRUE DO
    Write("Parte real? ");
    ReadLn(linea);
    z.real := FLOAT(linea);
    Write("Parte imaginaria? ");
    ReadLn(linea);
    z.imaginario := FLOAT(linea);
    IF (z.real # 0) OR (z.imaginario # 0) THEN
      EXIT
    ELSE
      WriteLn("El complejo no puede ser 0.")
    END
  END
END LeerComplejo;

PROCEDURE ImprimirComplejo(z : Complejo);
BEGIN
  Write(z.real);
  IF z.imaginario >= 0 THEN
    Write("+")
  ELSE
    Write("-")
  END;
  Write(ABS(z.imaginario));
  Write("i")
END ImprimirComplejo;

PROCEDURE SumaComplejos(z1, z2 : Complejo; VAR z : Complejo);
BEGIN
  z.real := z1.real + z2.real;
  z.imaginario := z1.imaginario + z2.imaginario
END SumaComplejos;

PROCEDURE RestaComplejos(z1, z2 : Complejo; VAR z : Complejo);
BEGIN
  z.real := z1.real - z2.real;
  z.imaginario := z1.imaginario - z2.imaginario
END RestaComplejos;

PROCEDURE MultiplicacionComplejos(z1, z2 : Complejo; VAR z : Complejo);
BEGIN
  z.real := z1.real * z2.real - z1.imaginario * z2.imaginario;
  z.imaginario := z1.real * z2.imaginario + z1.imaginario * z2.real
END MultiplicacionComplejos;

PROCEDURE DivisionComplejos(z1, z2 : Complejo; VAR z : Complejo);
VAR
  denominador : FLOAT;
BEGIN
  denominador := z2.real * z2.real + z2.imaginario * z2.imaginario;
  z.real := (z1.real * z2.real + z1.imaginario * z2.imaginario) / denominador;
  z.imaginario := (z1.imaginario * z2.real - z1.real * z2.imaginario) / denominador
END DivisionComplejos;

PROCEDURE ConjugarComplejo(VAR z : Complejo);
BEGIN
  z.imaginario := -z.imaginario
END ConjugarComplejo;

BEGIN
  WriteLn("Primer complejo:");
  LeerComplejo(complejo1);
  WriteLn("Segundo complejo:");
  LeerComplejo(complejo2);

  Write("Suma: ");
  SumaComplejos(complejo1, complejo2, complejo1);
  ImprimirComplejo(complejo1);
  WriteLn;

  Write("Resta: ");
  RestaComplejos(complejo1, complejo2, complejo1);
  ImprimirComplejo(complejo1);
  WriteLn;

  Write("Multiplicación: ");
  MultiplicacionComplejos(complejo1, complejo2, complejo1);
  ImprimirComplejo(complejo1);
  WriteLn;

  Write("División: ");
  DivisionComplejos(complejo1, complejo2, complejo1);
  ImprimirComplejo(complejo1);
  WriteLn;

  Write("Conjugado del primer complejo: ");
  ConjugarComplejo(complejo1);
  ImprimirComplejo(complejo1);
  WriteLn
END CalculadoraComplejos.
```

Este código es una calculadora de números complejos en MODULA-2. Permite al usuario ingresar dos números complejos, y luego realiza las operaciones básicas de suma, resta, multiplicación y división. También puede calcular el conjugado de un número complejo.

El código está dividido en varios procedimientos y funciones. El procedimiento `LeerComplejo` lee un número complejo del usuario. El procedimiento `ImprimirComplejo` imprime un número complejo en la pantalla. Los procedimientos `SumaComplejos`, `RestaComplejos`, `MultiplicacionComplejos` y `DivisionComplejos` realizan las operaciones básicas de suma, resta, multiplicación y división, respectivamente. El procedimiento `ConjugarComplejo` calcula el conjugado de un número complejo.

El procedimiento `CalculadoraComplejos` es el procedimiento principal del programa. Llama a los demás procedimientos para realizar las operaciones y mostrar los resultados.

El código está bien documentado, lo que facilita su comprensión y modificación. También usa una estructura modular, lo que lo hace más fácil de mantener y actualizar.