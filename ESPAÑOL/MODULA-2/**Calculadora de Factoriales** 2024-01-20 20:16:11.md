```modula-2
MODULE Factorial;
FROM FormatString IMPORT WriteString, WriteLn;
FROM InOut IMPORT WriteCard, ReadCard, WriteString, ReadString;

PROCEDURE Factorial(n: CARDINAL): CARDINAL;
BEGIN
  IF n = 0 THEN
    RETURN 1
  ELSE
    RETURN n * Factorial(n - 1)
  END;
END Factorial;

VAR num: CARDINAL;

BEGIN
  WriteString("Introduzca un número entero positivo: ");
  ReadCard(num);
  WriteString("El factorial de ", num, " es ", Factorial(num), ".");
  WriteLn;
END Factorial.
```

Explicación:

Este código calcula el factorial de un número entero positivo introducido por el usuario. El factorial de un número se calcula como el producto de todos los números enteros positivos menores o iguales que él. Por ejemplo, el factorial de 5 es 5 * 4 * 3 * 2 * 1 = 120.

El código se divide en dos partes:

* La primera parte declara el módulo `Factorial` y sus procedimientos y variables.
* La segunda parte contiene el código principal del programa, que solicita al usuario un número entero positivo y luego calcula y muestra el factorial de ese número.

El procedimiento `Factorial` se define en la primera parte del código. Este procedimiento calcula el factorial de un número entero positivo utilizando el algoritmo recursivo siguiente:

* Si el número es igual a 0, entonces el factorial es 1.
* De lo contrario, el factorial se calcula como el producto del número y el factorial del número menos 1.

El procedimiento `Factorial` se llama desde la segunda parte del código para calcular el factorial del número introducido por el usuario. El resultado se muestra luego en la consola.