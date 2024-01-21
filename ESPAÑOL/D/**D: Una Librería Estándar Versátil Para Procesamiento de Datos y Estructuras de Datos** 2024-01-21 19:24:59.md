```d
import std.stdio, std.algorithm, std.range, std.string;

const algoritmos = ["Burbuja", "Selección", "Inserción", "Merge Sort", "Quick Sort"];

void main() {
    // Crear una lista de números aleatorios.
    auto lista = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100];

    // Mostrar la lista original.
    writefln("Lista original:");
    for (auto elemento in lista)
        writef(" %d", elemento);
    writefln();

    // Ordenar la lista.
    sort(lista);

    // Mostrar la lista ordenada.
    writefln("Lista ordenada:");
    for (auto elemento in lista)
        writef(" %d", elemento);
    writefln();

    // Crear un mapa de algoritmos de ordenación.
    auto mapaAlgoritmos = new HashMap!(String, [10, 20, 30, 40, 50, 60, 70, 80, 90, 100];

    // Agregar los algoritmos de ordenación al mapa.
    for (auto algoritmo in algoritmos)
        mapaAlgoritmos[algoritmo] = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100];

    // Ordenar los algoritmos de ordenación por su nombre.
    sort(mapaAlgoritmos.keys);

    // Mostrar los algoritmos de ordenación ordenados.
    writefln("Algoritmos de ordenación ordenados:");
    for (auto algoritmo in mapaAlgoritmos.keys)
        writefln(" %s", algoritmo);

    // Generar una matriz de números aleatorios.
    auto matriz = [[10, 20, 30],
                    [40, 50, 60],
                    [70, 80, 90]];

    // Mostrar la matriz original.
    writefln("Matriz original:");
    for (auto fila in matriz) {
        writef("[");
        for (auto columna in fila)
            writef(" %d", columna);
        writefln("]");
    }

    // Transponer la matriz.
    matriz = matriz.transpose;

    // Mostrar la matriz transpuesta.
    writefln("Matriz transpuesta:");
    for (auto fila in matriz) {
        writef("[");
        for (auto columna in fila)
            writef(" %d", columna);
        writefln("]");
    }

    // Convertir una cadena de caracteres en una lista de palabras.
    auto cadena = "Hola mundo";
    auto palabras = cadena.split(" ");

    // Mostrar la lista de palabras.
    writefln("Lista de palabras:");
    for (auto palabra in palabras)
        writefln(" %s", palabra);

    // Unir una lista de palabras en una cadena de caracteres.
    cadena = palabras.join(" ");

    // Mostrar la cadena de caracteres.
    writefln("Cadena de caracteres:");
    writefln(cadena);

    // Crear un árbol binario de búsqueda.
    auto arbol = new Tree!(int);
    arbol.insert(10);
    arbol.insert(20);
    arbol.insert(30);
    arbol.insert(40);
    arbol.insert(50);

    // Mostrar el árbol binario de búsqueda.
    writefln("Árbol binario de búsqueda:");
    arbol.print;

    // Crear una tabla hash.
    auto tablaHash = new HashTable!(String, int);
    tablaHash["Juan"] = 20;
    tablaHash["María"] = 30;
    tablaHash["Pedro"] = 40;

    // Mostrar la tabla hash.
    writefln("Tabla hash:");
    for (auto clave in tablaHash.keys)
        writefln(" %s: %d", clave, tablaHash[clave]);
}
```

Explicación del código:

* El código crea una lista de números aleatorios, la ordena y la muestra.
* Luego, crea un mapa de algoritmos de ordenación y los ordena por su nombre.
* A continuación, genera una matriz de números aleatorios, la transpone y la muestra.
* Después, convierte una cadena de caracteres en una lista de palabras, la une en una cadena de caracteres y la muestra.
* Finalmente, crea un árbol binario de búsqueda, una tabla hash y los muestra.

Este código es un ejemplo de cómo utilizar algunas de las características más avanzadas del lenguaje D, como los mapas, las matrices, los árboles binarios de búsqueda y las tablas hash.