```modula-2

MODULE Fibonacci;
FROM FormatString IMPORT FormatString, FormatPos;

PROCEDURE Fib(n: CARDINAL): LONGCARDINAL;
VAR i, j: CARDINAL;
BEGIN
  i := 0;
  j := 1;
  WHILE n > 0 DO
    i := i + j;
    j := i - j;
    n := n - 1;
  END;
  RETURN i;
END Fib;

VAR buffer: ARRAY [0 .. 63] OF CHAR;
VAR number: LONGCARDINAL;
VAR i: CARDINAL;

BEGIN
  FOR i := 0 TO 20 DO
    number := Fib(i);
    FormatString(buffer, "Fibonacci(%u) = %u", [i, number]);
    WriteString(buffer);
    WriteLn;
  END;
END Fibonacci.

```

Este código calcula los primeros 21 números de Fibonacci y los imprime en la consola.

El código está dividido en dos partes: la primera parte define el módulo Fibonacci, que contiene una única función, la función Fib, que calcula el n-ésimo número de Fibonacci.

La segunda parte del código es el cuerpo del módulo, que llama a la función Fib 21 veces, cada vez con un valor diferente de n, y luego formatea el resultado de la llamada utilizando la función FormatString y la imprime en la consola.

Aquí hay una explicación más detallada del código:

* El módulo Fibonacci define una función llamada Fib que toma un número n como argumento y devuelve el n-ésimo número de Fibonacci.
* La función Fib utiliza un algoritmo iterativo para calcular el número de Fibonacci. El algoritmo comienza con los dos primeros números de Fibonacci, 0 y 1. Luego, itera sobre los números restantes de Fibonacci, sumando los dos números anteriores para obtener el siguiente número.
* El cuerpo del módulo Fibonacci llama a la función Fib 21 veces, cada vez con un valor diferente de n. Los valores de n van de 0 a 20.
* La función FormatString se utiliza para formatear el resultado de la llamada a Fib. La función FormatString toma dos argumentos: una cadena de formato y una lista de valores que se van a formatear.
* La cadena de formato contiene marcadores de posición que indican dónde se van a insertar los valores. Los marcadores de posición son %u, donde u indica el tipo de valor que se va a insertar (en este caso, un número sin signo de 32 bits).
* La lista de valores contiene los valores que se van a insertar en los marcadores de posición. En este caso, la lista contiene dos valores: el valor de n y el resultado de la llamada a Fib.
* La función FormatString devuelve una cadena formateada. La cadena formateada contiene el resultado de la llamada a Fib, formateado de acuerdo con la cadena de formato.
* La función WriteString se utiliza para imprimir la cadena formateada en la consola.
* La función WriteLn se utiliza para imprimir una nueva línea en la consola.