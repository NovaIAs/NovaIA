```d
import std.algorithm, std.stdio;

void main() {
    // Crear una lista de números
    immutable numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    // Utilizar la función 'map' para aplicar una operación a cada elemento de la lista
    immutable cuadrados = numeros.map!((x) => x * x);

    // Utilizar la función 'filter' para seleccionar los elementos que cumplen una condición
    immutable pares = numeros.filter!((x) => x % 2 == 0);

    // Utilizar la función 'reduce' para combinar todos los elementos de la lista en un solo valor
    immutable suma = numeros.reduce!((x, y) => x + y);

    // Imprimir los resultados
    writeln("Números:", join(numeros, ", "));
    writeln("Cuadrados:", join(cuadrados, ", "));
    writeln("Pares:", join(pares, ", "));
    writeln("Suma:", suma);
}
```

**Explicación del código:**

* La primera línea importa las bibliotecas estándar de algoritmos y de entrada/salida.
* La segunda línea crea una lista de números del 1 al 10.
* La tercera línea utiliza la función `map!` para aplicar la operación de elevar al cuadrado a cada elemento de la lista. La función `map!` devuelve una nueva lista con los resultados de la operación.
* La cuarta línea utiliza la función `filter!` para seleccionar los elementos de la lista que cumplen la condición de ser par. La función `filter!` devuelve una nueva lista con los elementos que cumplen la condición.
* La quinta línea utiliza la función `reduce!` para combinar todos los elementos de la lista en un solo valor. La función `reduce!` devuelve el resultado de la combinación de los elementos.
* La sexta línea imprime los resultados en la consola.