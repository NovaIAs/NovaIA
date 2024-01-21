```modula-2
MODULE Factorial;
FROM InOut IMPORT WriteString, WriteLn, WriteCard, WriteInt, ReadCard, ReadInt;

PROCEDURE Factorial(n: CARDINAL): CARDINAL;
BEGIN
  IF n = 0 THEN
    RETURN 1
  ELSE
    RETURN n * Factorial(n - 1)
  END
END Factorial;

VAR n: CARDINAL;

BEGIN
  WriteString("Ingrese un número no negativo: ");
  n := ReadCard;
  WriteString("El factorial de ");
  WriteCard(n);
  WriteString(" es ");
  WriteCard(Factorial(n));
  WriteLn
END Factorial.
```

Explicación del código:

* El módulo Factorial define un procedimiento llamado Factorial que calcula el factorial de un número entero no negativo. El factorial de un número n se define como el producto de todos los números enteros positivos desde 1 hasta n. Por ejemplo, el factorial de 5 es 5 * 4 * 3 * 2 * 1 = 120.

* El procedimiento Factorial utiliza la recursión para calcular el factorial de un número. Si el número es 0, entonces el factorial es 1. De lo contrario, el factorial es el producto del número y el factorial del número menos 1.

* La variable n se utiliza para almacenar el número cuyo factorial se va a calcular.

* El procedimiento main() solicita al usuario que introduzca un número no negativo y luego llama al procedimiento Factorial para calcular el factorial del número. El resultado se imprime en la consola.