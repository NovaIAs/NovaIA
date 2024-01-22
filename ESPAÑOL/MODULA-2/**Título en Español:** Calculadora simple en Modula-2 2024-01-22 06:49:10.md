```modula-2
MODULE Calculadora;
FROM InOut IMPORT WriteString, WriteFlt, ReadInt, ReadFlt;

VAR
    op: CHAR;
    n1, n2, resultado: FLOAT;

BEGIN
    WriteString("Introduce el primer número: ");
    n1 := ReadFlt;

    WriteString("Introduce el segundo número: ");
    n2 := ReadFlt;

    WriteString("Introduce la operación (+, -, *, /): ");
    op := ReadChar;

    CASE op OF
        '+': resultado := n1 + n2;
        '-': resultado := n1 - n2;
        '*': resultado := n1 * n2;
        '/': resultado := n1 / n2;
        ELSE: WriteString("Operación no válida")
    END;

    WriteString("El resultado es: ");
    WriteFlt(resultado, 2);
    WriteString("\n");
END Calculadora.
```

Explicación:

* El programa `Calculadora` es un programa simple que calcula el resultado de una operación aritmética entre dos números.
* El programa comienza leyendo los dos operandos y la operación a realizar.
* A continuación, utiliza una sentencia `CASE` para determinar qué operación se debe realizar.
* Si la operación es `+`, el programa suma los dos operandos. Si la operación es `-`, el programa resta el segundo operando del primero. Si la operación es `*`, el programa multiplica los dos operandos. Si la operación es `/`, el programa divide el primer operando por el segundo.
* Finalmente, el programa escribe el resultado de la operación en la pantalla.