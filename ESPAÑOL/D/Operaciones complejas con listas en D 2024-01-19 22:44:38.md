```d
import std.stdio, std.algorithm, std.container, std.string;

int main() {
    // Crear una lista de números
    auto numeros = [1, 2, 3, 4, 5];

    // Filtrar la lista para obtener los números pares
    auto pares = numeros.filter!(x => x % 2 == 0);

    // Sumar los números pares
    auto sumaPares = pares.reduce!((suma, x) => suma + x, 0);

    // Buscar el número máximo en la lista
    auto maximo = numeros.maxBy!(x => x);

    // Ordenar la lista de forma ascendente
    numeros.sort!();

    // Imprimir la lista de números
    foreach (auto numero; numeros) {
        writefln(numero);
    }

    // Imprimir la suma de los números
    writefln("Suma de los números: %d", sumaPares);

    // Imprimir el número máximo
    writefln("Número máximo: %d", maximo);

    return 0;
}
```

Este código es un ejemplo de cómo se puede utilizar la biblioteca estándar de D para realizar operaciones complejas con listas de datos.

El código comienza importando las bibliotecas necesarias. La biblioteca `std.stdio` proporciona las funciones de entrada y salida estándar, la biblioteca `std.algorithm` proporciona las funciones para trabajar con listas, la biblioteca `std.container` proporciona las clases para trabajar con contenedores, y la biblioteca `std.string` proporciona las clases para trabajar con cadenas de caracteres.

A continuación, se crea una lista de números utilizando la sintaxis `[1, 2, 3, 4, 5]`.

A continuación, se utiliza la función `filter!` para filtrar la lista de números para obtener los números pares. La función `filter!` toma una función booleana como argumento y devuelve una nueva lista que contiene sólo los elementos de la lista original que cumplen con la condición de la función. En este caso, la función booleana es `x % 2 == 0`, que devuelve `true` si el número `x` es par.

A continuación, se utiliza la función `reduce!` para sumar los números pares de la lista. La función `reduce!` toma dos argumentos: una función que toma dos valores y devuelve un nuevo valor, y un valor inicial. En este caso, la función es `(suma, x) => suma + x`, que devuelve la suma de los dos números `suma` y `x`. El valor inicial es `0`.

A continuación, se utiliza la función `maxBy!` para encontrar el número máximo de la lista. La función `maxBy!` toma una función que toma un valor y devuelve un valor comparable. En este caso, la función es `x => x`, que devuelve el propio número `x`.

A continuación, se utiliza la función `sort!` para ordenar la lista de números de forma ascendente. La función `sort!` toma un comparador como argumento. El comparador es una función que toma dos valores y devuelve un valor entero que indica si el primer valor es menor que, igual a o mayor que el segundo valor. En este caso, el comparador es `(a, b) => a - b`, que devuelve la diferencia entre los dos números `a` y `b`.

Por último, se imprimen la lista de números, la suma de los números, y el número máximo en la consola.