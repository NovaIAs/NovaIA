```modula-2
MODULE Complejo;

FROM Terminal IMPORT WriteString, WriteInt;
FROM Strings IMPORT GetLine;
FROM FormatString IMPORT Fmt;

TYPE
  Complejo = RECORD
    real: LONGINT;
    imag: LONGINT;
  END;

PROCEDURE LeerComplejo(VAR c: Complejo);
BEGIN
  WriteString("Introduce la parte real: ");
  c.real := GetLine;
  WriteString("Introduce la parte imaginaria: ");
  c.imag := GetLine;
END LeerComplejo;

PROCEDURE EscribirComplejo(c: Complejo);
BEGIN
  Fmt("(%d,%d)\n", c.real, c.imag);
END EscribirComplejo;

PROCEDURE SumaComplejos(c1, c2: Complejo; VAR r: Complejo);
BEGIN
  r.real := c1.real + c2.real;
  r.imag := c1.imag + c2.imag;
END SumaComplejos;

PROCEDURE RestaComplejos(c1, c2: Complejo; VAR r: Complejo);
BEGIN
  r.real := c1.real - c2.real;
  r.imag := c1.imag - c2.imag;
END RestaComplejos;

PROCEDURE MultiplicacionComplejos(c1, c2: Complejo; VAR r: Complejo);
BEGIN
  r.real := c1.real * c2.real - c1.imag * c2.imag;
  r.imag := c1.real * c2.imag + c1.imag * c2.real;
END MultiplicacionComplejos;

PROCEDURE DivisionComplejos(c1, c2: Complejo; VAR r: Complejo);
BEGIN
  IF c2.real = 0 AND c2.imag = 0 THEN
    WriteString("Error: división por cero\n");
  ELSE
    r.real := (c1.real * c2.real + c1.imag * c2.imag) / (c2.real * c2.real + c2.imag * c2.imag);
    r.imag := (c1.imag * c2.real - c1.real * c2.imag) / (c2.real * c2.real + c2.imag * c2.imag);
  END;
END DivisionComplejos;

PROCEDURE PotenciaCompleja(c: Complejo; n: LONGINT; VAR r: Complejo);
BEGIN
  r := c;
  FOR i := 2 TO n DO
    MultiplicacionComplejos(r, c, r);
  END;
END PotenciaCompleja;

PROCEDURE RaizCompleja(c: Complejo; n: LONGINT; VAR r: Complejo);
BEGIN
  IF n = 1 THEN
    r := c;
  ELSE
    PotenciaCompleja(c, n - 1, r);
    DivisionComplejos(c, r, r);
  END;
END RaizCompleja;

PROCEDURE Menu();
BEGIN
  WriteString("1. Sumar complejos\n");
  WriteString("2. Restar complejos\n");
  WriteString("3. Multiplicar complejos\n");
  WriteString("4. Dividir complejos\n");
  WriteString("5. Calcular potencia compleja\n");
  WriteString("6. Calcular raíz compleja\n");
  WriteString("7. Salir\n");
END Menu;

VAR
  opcion: CHAR;
  c1, c2, r: Complejo;
  n: LONGINT;

BEGIN
  REPEAT
    Menu();
    WriteString("Introduce una opción: ");
    opcion := GetLine;
    CASE opcion OF
      '1':
        LeerComplejo(c1);
        LeerComplejo(c2);
        SumaComplejos(c1, c2, r);
        EscribirComplejo(r);
      '2':
        LeerComplejo(c1);
        LeerComplejo(c2);
        RestaComplejos(c1, c2, r);
        EscribirComplejo(r);
      '3':
        LeerComplejo(c1);
        LeerComplejo(c2);
        MultiplicacionComplejos(c1, c2, r);
        EscribirComplejo(r);
      '4':
        LeerComplejo(c1);
        LeerComplejo(c2);
        DivisionComplejos(c1, c2, r);
        EscribirComplejo(r);
      '5':
        LeerComplejo(c1);
        WriteString("Introduce la potencia: ");
        n := GetLine;
        PotenciaCompleja(c1, n, r);
        EscribirComplejo(r);
      '6':
        LeerComplejo(c1);
        WriteString("Introduce el índice de la raíz: ");
        n := GetLine;
        RaizCompleja(c1, n, r);
        EscribirComplejo(r);
      '7':
      ELSE
        WriteString("Opción no válida\n");
    END;
  UNTIL opcion = '7';
END Complejo.
```

Explicación del código:

1. Constantes y tipos:

* Definimos el tipo `Complejo` que almacena un número complejo con sus partes real e imaginaria.
* También definimos algunas constantes que usaremos en el programa.

2. Procedimientos:

* `LeerComplejo()`: Lee un número complejo del usuario y lo almacena en su variable de argumento.
* `EscribirComplejo()`: Escribe un número complejo en la salida estándar.
* `SumaComplejos()`, `RestaComplejos()`, `MultiplicacionComplejos()` y `DivisionComplejos()`: Son procedimientos que realizan las operaciones aritméticas básicas entre números complejos.
* `PotenciaCompleja()`: Calcula la potencia de un número complejo elevado a un entero.
* `RaizCompleja()`: Calcula la raíz enésima de un número complejo.
* `Menu()`: Muestra un menú de opciones al usuario.

3. Programa principal:

* El programa principal primero muestra un menú de opciones al usuario.
* Luego, lee una opción del usuario y ejecuta el procedimiento correspondiente.
* El programa continúa repitiendo este proceso hasta que el usuario elige salir.

4. Ejecución del programa:

* Para ejecutar el programa, simplemente compila y ejecuta el archivo `Complejo.mod`.
* El programa pedirá al usuario que introduzca dos números complejos y una opción.
* A continuación, realizará la operación correspondiente y mostrará el resultado en la salida estándar.