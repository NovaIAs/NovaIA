```
// Este programa calcula la suma de los primeros 100 números naturales.

programa SumarNaturales;

// Declaración de variables
entero suma; // Variable para almacenar la suma de los números naturales
entero i; // Variable para iterar sobre los números naturales

// Inicialización de variables
suma = 0;
i = 1;

// Bucle que itera sobre los números naturales
mientras (i <= 100) hacer
    // Agregar el valor de i a la variable suma
    suma = suma + i;
    
    // Incrementar el valor de i
    i = i + 1;
fin mientras;

// Imprimir el valor de la suma
escribir("La suma de los primeros 100 números naturales es ", suma);

fin programa;
```

Explicación del código:

* El programa comienza con la declaración del programa `SumarNaturales`.
* A continuación, se declaran las variables `suma` e `i` para almacenar la suma de los números naturales y el número natural actual, respectivamente.
* Las variables se inicializan con los valores `0` y `1`, respectivamente.
* El programa contiene un bucle `mientras` que itera sobre los números naturales desde `1` hasta `100`.
* Dentro del bucle, se agrega el valor de `i` a la variable `suma` y se incrementa el valor de `i` en `1`.
* Después de que el bucle haya terminado, se imprime el valor de la variable `suma` en la consola.
* Finalmente, el programa termina con la instrucción `fin programa`.