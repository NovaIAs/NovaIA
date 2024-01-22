```cool

-- Programa principal
programa main {
    -- Declaración de variables
    var a: Integer;
    var b: Integer;
    var c: Integer;

    -- Inicialización de variables
    a := 10;
    b := 20;
    c := 30;

    -- Impresión de variables
    writeInt(a);
    writeInt(b);
    writeInt(c);

    -- Llamada a función
    printSum(a, b, c);

    -- Fin del programa
    return;
}

-- Función para imprimir la suma de tres números
proc printSum(a: Integer, b: Integer, c: Integer) {
    -- Declaración de variable local
    var sum: Integer;

    -- Cálculo de la suma
    sum := a + b + c;

    -- Impresión de la suma
    writeInt(sum);

    -- Fin de la función
    return;
}

```

**Explicación:**

1. El programa principal `main` declara tres variables enteras, `a`, `b` y `c`, y las inicializa con los valores 10, 20 y 30, respectivamente.
2. A continuación, el programa imprime los valores de las tres variables en la consola.
3. Luego, el programa llama a la función `printSum` y le pasa como argumentos los valores de las tres variables.
4. La función `printSum` declara una variable local `sum`, que se inicializa con la suma de los valores de los tres argumentos.
5. A continuación, la función imprime el valor de la variable `sum` en la consola.
6. Finalmente, la función `printSum` termina y el programa principal continúa su ejecución.