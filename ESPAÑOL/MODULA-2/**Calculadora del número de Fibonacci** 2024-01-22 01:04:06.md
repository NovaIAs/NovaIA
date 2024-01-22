```modula-2

MODULE Fibonacci;

IMPORT TextIO;

VAR Numero: INTEGER;
    Fibonacci1, Fibonacci2, Auxiliar: LONGINT;

PROCEDURE CalcularFibonacci(numero: INTEGER): LONGINT;
BEGIN
  Fibonacci1 := 0;
  Fibonacci2 := 1;
  FOR i := 1 TO numero - 1 DO
    Auxiliar := Fibonacci1 + Fibonacci2;
    Fibonacci1 := Fibonacci2;
    Fibonacci2 := Auxiliar;
  END;
  RETURN Fibonacci2;
END CalcularFibonacci;

BEGIN
  TextIO.Write("Ingrese un número: ");
  TextIO.Read(Numero);
  TextIO.WriteLn("El ", Numero, "-ésimo número de Fibonacci es: ", CalcularFibonacci(Numero));

END Fibonacci.

```
Explicación del código:

* El módulo Fibonacci define un procedimiento llamado CalcularFibonacci que calcula el n-ésimo número de Fibonacci.
* El procedimiento CalcularFibonacci toma un número entero n como argumento y devuelve un número entero largo como resultado.
* El procedimiento CalcularFibonacci utiliza dos variables enteras largas, Fibonacci1 y Fibonacci2, para almacenar los dos últimos números de Fibonacci.
* El procedimiento CalcularFibonacci utiliza un bucle for para iterar desde 1 hasta n - 1.
* En cada iteración del bucle, el procedimiento CalcularFibonacci calcula el siguiente número de Fibonacci sumando Fibonacci1 y Fibonacci2.
* El procedimiento CalcularFibonacci actualiza Fibonacci1 y Fibonacci2 para que Fibonacci1 sea igual a Fibonacci2 y Fibonacci2 sea igual al siguiente número de Fibonacci.
* Una vez que el bucle for ha terminado, el procedimiento CalcularFibonacci devuelve Fibonacci2 como resultado.
* El programa principal del módulo Fibonacci pide al usuario que introduzca un número entero.
* El programa principal llama al procedimiento CalcularFibonacci con el número introducido por el usuario como argumento.
* El programa principal imprime el n-ésimo número de Fibonacci en la consola.