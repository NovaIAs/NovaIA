```d
import std.stdio, std.range, std.string, std.traits;

void main() {
    // Definir a lista de números
    immutable números = [1, 2, 3, 4, 5];

    // Definir la función para calcular el cuadrado de un número
    pure doub multiplica(int n) {
        return n * n;
    }

    // Utilizar la función de multiplicación para obtener una lista de cuadrados
    immutable cuadrados = números.map!multiplica;

    // Imprimir la lista original y la lista de cuadrados
    writeln("Lista original:", números);
    writeln("Lista de cuadrados:", cuadrados);

    // Definir la función para filtrar los números pares
    pure bool esPar(int n) {
        return n % 2 == 0;
    }

    // Utilizar la función de filtrado para obtener una lista de números pares
    immutable pares = números.filter!esPar;

    // Imprimir la lista de números pares
    writeln("Lista de números pares:", pares);

    // Encontrar el primer número par en la lista original
    immutable primerPar = números.find!esPar;

    // Imprimir el primer número par
    writeln("Primer número par:", primerPar);

    // Encontrar el índice del primer número par en la lista original
    immutable índicePrimerPar = números.indexWhere!esPar;

    // Imprimir el índice del primer número par
    writeln("Índice del primer número par:", índicePrimerPar);

    // Calcular la suma de los cuadrados de los números pares
    immutable sumaCuadradosPares = cuadrados.filter!esPar.sum;

    // Imprimir la suma de los cuadrados de los números pares
    writeln("Suma de los cuadrados de los números pares:", sumaCuadradosPares);

    // Ordenar la lista de cuadrados en orden ascendente
    immutable cuadradosOrdenados = cuadrados.sort;

    // Imprimir la lista de cuadrados ordenados
    writeln("Lista de cuadrados ordenados:", cuadradosOrdenados);

    // Ordenar la lista de cuadrados en orden descendente
    immutable cuadradosOrdenadosDescendentemente = cuadrados.sort!isDescending;

    // Imprimir la lista de cuadrados ordenados descendentemente
    writeln("Lista de cuadrados ordenados descendentemente:", cuadradosOrdenadosDescendentemente);

    // Crear un mapa de los números y sus cuadrados
    immutable mapaNumerosCuadrados = new!HashMap!int => doub;
    for (i, n) in enumerate(números) {
        mapaNumerosCuadrados[n] = cuadrados[i];
    }

    // Imprimir el mapa de los números y sus cuadrados
    writeln("Mapa de los números y sus cuadrados:");
    for (n, c) in mapaNumerosCuadrados {
        writeln("$n: $c");
    }

    // Crear un conjunto de los números pares
    immutable conjuntoPares = pares.to!HashSet;

    // Imprimir el conjunto de los números pares
    writeln("Conjunto de los números pares:");
    for (n in conjuntoPares) {
        writeln(n);
    }

    // Convertir la lista de números en una cadena de texto
    immutable cadenaNumeros = números.join(",");

    // Imprimir la cadena de texto
    writeln("Cadena de texto de los números:", cadenaNumeros);
}
```

Explicación del código:

* Se define una lista de números llamada `números`.
* Se define una función llamada `multiplica` que calcula el cuadrado de un número.
* Se utiliza la función `map` para aplicar la función `multiplica` a cada elemento de la lista `números`, generando una nueva lista llamada `cuadrados` que contiene los cuadrados de los números originales.
* Se imprimen las listas `números` y `cuadrados`.
* Se define una función llamada `esPar` que determina si un número es par.
* Se utiliza la función `filter` para aplicar la función `esPar` a cada elemento de la lista `números`, generando una nueva lista llamada `pares` que contiene solo los números pares de la lista original.
* Se imprime la lista `pares`.
* Se utiliza la función `find` para encontrar el primer elemento de la lista `números` que satisface la función `esPar`.
* Se imprime el primer número par encontrado.
* Se utiliza la función `indexWhere` para encontrar el índice del primer elemento de la lista `números` que satisface la función `esPar`.
* Se imprime el índice del primer número par encontrado.
* Se utiliza la función `sum` para calcular la suma de los elementos de la lista `cuadrados`.
* Se imprime la suma de los cuadrados de los números pares.
* Se utiliza la función `sort` para ordenar la lista `cuadrados` en orden ascendente.
* Se imprime la lista `cuadrados` ordenada ascendentemente.
* Se utiliza la función `sort!isDescending` para ordenar la lista `cuadrados` en orden descendente.
* Se imprime la lista `cuadrados` ordenada descendentemente.
* Se crea un mapa llamado `mapaNumerosCuadrados` que asocia cada número de la lista `números` con su cuadrado correspondiente.
* Se imprime el mapa `mapaNumerosCuadrados`.
* Se crea un conjunto llamado `conjuntoPares` que contiene los números pares de la lista `números`.
* Se imprime el conjunto `conjuntoPares`.
* Se utiliza la función `join` para convertir la lista `números` en una cadena de texto.
* Se imprime la cadena de texto.