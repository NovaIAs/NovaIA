```d
import std.stdio, std.range, std.algorithm, std.bitmanip, std.traits;

void main() {
    // Crea una lista de números del 1 al 10.
    auto lista = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    // Imprime la suma total de los números en la lista.
    writeln("**Suma total**:", sum(lista).stringof());

    // Devuelve el índice del número 5 en la lista.
    writeln("**Índice del 5**:", findIndex(lista, 5));

    // Ordena la lista en orden ascendente.
    lista.sort();
    writeln("**Lista ordenada**:", lista.stringof());

    // Filtra la lista para obtener los números pares.
    auto listaPares = lista.filter!((n) => n % 2 == 0);
    writeln("**Lista de números pares**:", listaPares.stringof());

    // Calcula la multiplicación de todos los números en la lista.
    writeln("**Multiplicación total**:", reduce(lista, 1, (a, b) => a * b));

    // Obtiene los números primos de la lista, usando una función lambda.
    writeln("**Números primos**:",
            lista.filter!((n) => {
                if (n <= 1) return false;
                foreach (i; 2 .. floor(sqrt(n))) {
                    if (n % i == 0) return false;
                }
                return true;
            }).stringof()
    );

    // Crea un mapa de los números en la lista con sus cuadrados, usando una expresión lambda.
    auto mapaCuadrados = new HashMap!(int, int);
    foreach (n; lista) {
        mapaCuadrados[n] = n * n;
    }
    writeln("**Mapa de números y sus cuadrados**:",
            mapaCuadrados.map!((k, v) => "\(k): \(v)").stringof()
    );

    // Crea un conjunto de los números en la lista, usando una expresión lambda.
    auto conjunto = new HashSet!(int);
    foreach (n; lista) {
        conjunto.insert(n + 5);
    }
    writeln("**Conjunto de números**:", conjunto.stringof());

    // Suma todos los números del 1 al 100 que son divisibles por 3 o 5, usando una función lambda.
    writeln("**Suma de los números divisibles por 3 o 5**:",
            (1 .. 100).filter!((n) => n % 3 == 0 || n % 5 == 0).sum().stringof()
    );

    // Comprueba si la lista contiene el número 7, usando una expresión lambda.
    writeln("**La lista contiene el 7**:", lista.any!((n) => n == 7));

    // Obtiene la posición de la primera aparición del número 7 en la lista, usando una expresión lambda.
    writeln("**Primera aparición del 7**:", lista.findPos!((n) => n == 7));

    // Elimina el primer número de la lista, usando una expresión lambda.
    lista.removeAt!(0);
    writeln("**Lista con el primer número eliminado**:", lista.stringof());
}
```

Este código demuestra la complejidad y versatilidad del lenguaje D al realizar una serie de operaciones complejas de procesamiento de datos:

1. Crea una lista de números del 1 al 10 y realiza diversas operaciones sobre ella:
    - Suma total de los números.
    - Devuelve el índice del número 5.
    - Ordena la lista en orden ascendente.
    - Filtra la lista para obtener los números pares.
    - Calcula la multiplicación total de los números.
    - Obtiene los números primos de la lista usando una función lambda.

2. Crea un mapa de los números en la lista con sus cuadrados, utilizando una expresión lambda.

3. Crea un conjunto de los números en la lista, usando una expresión lambda.

4. Suma todos los números del 1 al 100 que son divisibles por 3 o 5, usando una función lambda.

5. Comprueba si la lista contiene el número 7, usando una expresión lambda.

6. Obtiene la posición de la primera aparición del número 7 en la lista, usando una expresión lambda.

7. Elimina el primer número de la lista, utilizando una expresión lambda.

Este código demuestra cómo D puede utilizarse para resolver problemas complejos de procesamiento de datos de una manera concisa y eficiente.