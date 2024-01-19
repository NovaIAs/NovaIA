```portuguol
programa ordena_array(array)
{
    // Este programa ordena un array de números en orden ascendente.

    // Declaración de variables.
    índice = 0;
    valor_actual = 0;
    valor_anterior = 0;
    longitud_array = comprimento(array);

    // Ordenar el array.
    para (índice = 1; índice < longitud_array; índice++) {
        valor_actual = array[índice];
        valor_anterior = índice - 1;

        enquanto (valor_anterior >= 0 && valor_actual < array[valor_anterior]) {
            array[valor_anterior + 1] = array[valor_anterior];
            valor_anterior--;
        }

        array[valor_anterior + 1] = valor_actual;
    }

    // Imprimir el array ordenado.
    para (índice = 0; índice < longitud_array; índice++) {
        escreva(array[índice], " ");
    }

    escreva("\n");
}

// Probar el programa.
array = [7, 3, 2, 1, 5, 4, 6];
ordena_array(array);
```

Explicación del código:

* El programa comienza definiendo una función llamada `ordena_array()` que recibe un array como argumento. Esta función ordena el array en orden ascendente.
* La función `ordena_array()` comienza declarando las variables que necesitará. La variable `índice` se utilizará para recorrer el array, `valor_actual` y `valor_anterior` se utilizarán para almacenar los valores actuales y anteriores en el array, y `longitud_array` se utilizará para almacenar la longitud del array.
* La función `ordena_array()` utiliza un bucle `para` para recorrer el array desde el segundo elemento hasta el último.
* Dentro del bucle `para`, la función `ordena_array()` almacena el valor actual del array en la variable `valor_actual` y el índice del elemento anterior en la variable `valor_anterior`.
* La función `ordena_array()` utiliza un bucle `mientras` para comparar el valor actual del array con el valor anterior. Si el valor actual es menor que el valor anterior, la función `ordena_array()` mueve el valor anterior un elemento hacia adelante en el array.
* Después de mover el valor anterior, la función `ordena_array()` almacena el valor actual en el elemento anterior.
* Una vez que el valor actual ha sido insertado en el lugar correcto, la función `ordena_array()` continúa con el siguiente elemento del array.
* Después de ordenar el array, la función `ordena_array()` utiliza un bucle `para` para imprimir el array ordenado.
* La función `ordena_array()` termina devolviendo el array ordenado.

El programa principal llama a la función `ordena_array()` con el array `[7, 3, 2, 1, 5, 4, 6]` como argumento. La función `ordena_array()` ordena el array en orden ascendente y lo imprime. La salida del programa es `1 2 3 4 5 6 7`.