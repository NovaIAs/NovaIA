```d
import std.stdio, std.range;

void main() {
    // Crear un array de números enteros
    int[] numeros = [1, 2, 3, 4, 5];

    // Crear una función lambda para multiplicar cada número por 2
    auto multiplicarPor2 = (int n) { return n * 2; };

    // Usar map! para aplicar la función lambda a cada número en el array
    int[] numerosMultiplicados = numeros.map!(multiplicarPor2);

    // Imprimir el array de números multiplicados
    foreach (int n; numerosMultiplicados) {
        writefln("%d", n);
    }
}
```

Explicación del código:

1. Se importa la biblioteca estándar de D, que incluye funciones básicas de entrada y salida, y también la biblioteca de rangos, que proporciona las funciones necesarias para trabajar con arrays.
2. Se define una función `main()` que es el punto de entrada del programa.
3. Se crea un array de números enteros llamado `numeros`, que contiene los valores 1, 2, 3, 4 y 5.
4. Se define una función lambda llamada `multiplicarPor2`, que toma un número entero como argumento y devuelve el resultado de multiplicarlo por 2.
5. Se usa la función `map!` para aplicar la función lambda a cada número en el array `numeros`. Esto devuelve un nuevo array con los números multiplicados por 2.
6. Se imprime el array de números multiplicados usando la función `foreach`.

El resultado del programa es el siguiente:

```
2
4
6
8
10
```