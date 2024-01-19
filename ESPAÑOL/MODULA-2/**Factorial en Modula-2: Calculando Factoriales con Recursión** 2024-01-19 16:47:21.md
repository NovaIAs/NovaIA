```modula-2
MODULE Factorial;
FROM InOut IMPORT WriteLn;

PROCEDURE Factorial(n: INTEGER): INTEGER;
BEGIN
  IF n = 0 THEN
    RETURN 1;
  ELSE
    RETURN n * Factorial(n - 1);
  END;
END Factorial;

PROCEDURE Demo;
VAR i: INTEGER;
BEGIN
  FOR i:=1 TO 10 DO
    WriteLn(i,"! =",Factorial(i));
  END;
END Demo;

BEGIN
  Demo;
END Factorial.
```

Explicación:

* El módulo Factorial define un procedimiento llamado Factorial que calcula el factorial de un número.
* El procedimiento Factorial utiliza la recursión para calcular el factorial.
* El procedimiento Demo llama al procedimiento Factorial para calcular los factoriales de los números del 1 al 10 y los imprime.
* El programa principal llama al procedimiento Demo para ejecutar el programa.