```modula-2
MODULE Potencias;
FROM FormatString IMPORT Printf;

PROCEDURE Potencia(base: LONGCARD; exponente: LONGCARD): LONGCARD;
VAR i: LONGCARD;
    resultado: LONGCARD;
BEGIN
    resultado := 1;
    FOR i := 1 TO exponente DO
        resultado := resultado * base
    END;
    RETURN resultado
END Potencia;

PROCEDURE Main;
VAR base, exponente, resultado: LONGCARD;
BEGIN
    Printf("Introduzca la base: ");
    Read(base);
    Printf("Introduzca el exponente: ");
    Read(exponente);
    resultado := Potencia(base, exponente);
    Printf("El resultado es: %d\n", resultado)
END Main.
```

Explicación:

El código que se muestra arriba es un programa en Modula-2 que calcula la potencia de un número utilizando un método recursivo.

El programa está dividido en dos partes: la definición del módulo `Potencias` y la definición del procedimiento `Main`.

El módulo `Potencias` contiene una sola función, `Potencia`, que calcula la potencia de un número utilizando un método recursivo.

La función `Potencia` toma dos parámetros: la base y el exponente, y devuelve el resultado de multiplicar la base por sí misma el número de veces especificado por el exponente.

El procedimiento `Main` es el punto de entrada del programa.

El procedimiento `Main` primero solicita al usuario que introduzca la base y el exponente, y luego llama a la función `Potencia` para calcular el resultado.

Finalmente, el procedimiento `Main` imprime el resultado en la consola.

El programa funciona de la siguiente manera:

1. El usuario introduce la base y el exponente.
2. El procedimiento `Main` llama a la función `Potencia` para calcular el resultado.
3. La función `Potencia` utiliza un método recursivo para calcular el resultado.
4. El procedimiento `Main` imprime el resultado en la consola.