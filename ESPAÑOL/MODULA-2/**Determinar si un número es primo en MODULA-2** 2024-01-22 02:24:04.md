```modula-2
MODULE DeterminarSiUnNumeroEsPrimo;
FROM FormatString IMPORT FormatDouble;
FROM InOut IMPORT Read, WriteLn, WriteString, WriteCard;
FROM Tokens IMPORT Token, TokenKind;

VAR
  n: CARDINAL;
  esPrimo: BOOLEAN;

PROCEDURE LeerNumero(VAR n: CARDINAL);
VAR
  s: ARRAY[0..31] OF CHAR;
BEGIN
  WriteString("Introduzca un número entero positivo: ");
  Read(s);
  n := CARDINAL(s);
END LeerNumero;

PROCEDURE EsPrimo(VAR n: CARDINAL): BOOLEAN;
VAR
  i: CARDINAL;
  divisorEncontrado: BOOLEAN;
BEGIN
  divisorEncontrado := FALSE;
  i := 2;
  WHILE (i * i <= n) AND NOT divisorEncontrado DO
    IF n MOD i = 0 THEN
      divisorEncontrado := TRUE
    ELSE
      i := i + 1
    END
  END;
  EsPrimo := NOT divisorEncontrado
END EsPrimo;

BEGIN
  LeerNumero(n);
  esPrimo := EsPrimo(n);
  IF esPrimo THEN
    WriteLn("El número ", FormatDouble(0, 0, 0, n), " es primo.")
  ELSE
    WriteLn("El número ", FormatDouble(0, 0, 0, n), " no es primo.")
  END
END DeterminarSiUnNumeroEsPrimo.
```

Este código en MODULA-2 determina si un número entero positivo introducido por el usuario es primo o no. El código está dividido en tres partes:

* El módulo `DeterminarSiUnNumeroEsPrimo` define el programa principal y contiene las declaraciones globales.
* El procedimiento `LeerNumero` lee un número entero positivo del usuario y lo almacena en la variable `n`.
* El procedimiento `EsPrimo` comprueba si el número `n` es primo o no.

El procedimiento `EsPrimo` utiliza un algoritmo simple para determinar si un número es primo. El algoritmo funciona dividiendo el número por todos los números enteros desde 2 hasta la raíz cuadrada del número. Si el número es divisible por alguno de estos números, entonces no es primo. De lo contrario, es primo.

El programa principal llama al procedimiento `LeerNumero` para leer un número del usuario y luego llama al procedimiento `EsPrimo` para determinar si el número es primo o no. El resultado se muestra en la consola.

Este código es un ejemplo de cómo utilizar los procedimientos y las declaraciones globales en MODULA-2 para crear un programa complejo y diferenciado.