```d
import std.algorithm, std.stdio, std.range, std.string;

void main() {
    // Definir una lista de enteros
    int[] lista = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19];

    // Imprimir la lista original
    writeln("Lista original:");
    writeln(lista.join(", "));

    // Filtrar la lista para obtener los números pares
    int[] pares = lista.filter!(x => x % 2 == 0);

    // Imprimir la lista de números pares
    writeln("Lista de números pares:");
    writeln(pares.join(", "));

    // Ordenar la lista de números pares en orden ascendente
    pares.sort!();

    // Imprimir la lista de números pares ordenada
    writeln("Lista de números pares ordenada:");
    writeln(pares.join(", "));

    // Encontrar el primer número par mayor o igual a 10
    int primerParMayorOIgualA10 = lista.findFirst!(x => x % 2 == 0 && x >= 10);

    // Imprimir el primer número par mayor o igual a 10
    writeln("Primer número par mayor o igual a 10:");
    writeln(primerParMayorOIgualA10);

    // Sumar todos los números pares de la lista
    int sumaPares = lista.sum!();

    // Imprimir la suma de los números pares
    writeln("Suma de los números pares:");
    writeln(sumaPares);

    // Crear un mapa de los números pares y sus cuadrados
    var mapaParesCuadrados = pares.toMap!((x) => x, (x) => x * x);

    // Imprimir el mapa de los números pares y sus cuadrados
    writeln("Mapa de los números pares y sus cuadrados:");
    writeln(mapaParesCuadrados);
}
```

Este código realiza las siguientes operaciones:

1. Define una lista de enteros llamada `lista`.
2. Imprime la lista original.
3. Filtra la lista para obtener los números pares y los almacena en una nueva lista llamada `pares`.
4. Imprime la lista de números pares.
5. Ordena la lista de números pares en orden ascendente.
6. Imprime la lista de números pares ordenada.
7. Encuentra el primer número par mayor o igual a 10 en la lista y lo almacena en la variable `primerParMayorOIgualA10`.
8. Imprime el primer número par mayor o igual a 10.
9. Suma todos los números pares de la lista y almacena el resultado en la variable `sumaPares`.
10. Imprime la suma de los números pares.
11. Crea un mapa de los números pares y sus cuadrados, donde los números pares son las claves y los cuadrados son los valores. El mapa se almacena en la variable `mapaParesCuadrados`.
12. Imprime el mapa de los números pares y sus cuadrados.