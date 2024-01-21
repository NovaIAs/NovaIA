```d
import std.stdio, std.algorithm, std.range, std.sort, std.traits;

void main() {
    // Definición de types alias para simplificar el código
    type MyInt = int;
    type MyString = string;
    type MyPair = tuple(MyInt, MyString);
    type MyVector = Vector!MyPair;

    // Creación de un vector de pares
    MyVector miVector = [
        (1, "Uno"),
        (2, "Dos"),
        (3, "Tres"),
        (4, "Cuatro"),
        (5, "Cinco")
    ];

    // Ordenamiento del vector utilizando la función sort
    miVector.sort!tupleFirst!reversed; // Ordenar por el primer elemento en orden descendente

    // Impresión del vector ordenado
    writeln("Vector ordenado:");
    foreach (pair; miVector)
        writeln("({!pair.0}, {!pair.1})");

    // Búsqueda de un elemento en el vector utilizando la función find
    MyPair parABuscar = (3, "Tres");
    auto indice = miVector.find!tupleFirst(parABuscar);
    if (indice != -1)
        writeln("El par ({!parABuscar.0}, {!parABuscar.1}) se encuentra en el índice {!indice}");
    else
        writeln("El par ({!parABuscar.0}, {!parABuscar.1}) no se encuentra en el vector");

    // Eliminación de un elemento del vector utilizando la función erase
    miVector.erase(indice);

    // Impresión del vector después de la eliminación
    writeln("Vector después de la eliminación:");
    foreach (pair; miVector)
        writeln("({!pair.0}, {!pair.1})");

    // Creación de un mapa utilizando la clase HashMap
    HashMap!MyInt, MyString> miMapa;

    // Adición de pares al mapa
    miMapa[1] = "Uno";
    miMapa[2] = "Dos";
    miMapa[3] = "Tres";

    // Iteración sobre el mapa utilizando la función foreach
    writeln("Mapa:");
    foreach (clave, valor; miMapa)
        writeln("{!clave} => {!valor}");

    // Búsqueda de un elemento en el mapa utilizando la función find
    MyInt claveABuscar = 2;
    auto valorEncontrado = miMapa.find(claveABuscar);
    if (valorEncontrado != null)
        writeln("El valor {!valorEncontrado} se encuentra asociado a la clave {!claveABuscar}");
    else
        writeln("La clave {!claveABuscar} no se encuentra en el mapa");

    // Eliminación de un elemento del mapa utilizando la función erase
    miMapa.erase(claveABuscar);

    // Impresión del mapa después de la eliminación
    writeln("Mapa después de la eliminación:");
    foreach (clave, valor; miMapa)
        writeln("{!clave} => {!valor}");

    // Definición de una función genérica para ordenar una secuencia
    template ordenar(T)(seq!T t) pure @safe {
        t.sort!tupleFirst; // Ordenar por el primer elemento en orden ascendente
    }

    // Demostración de la función genérica ordenando una secuencia de cadenas
    MyVector miSecuencia = ["e", "a", "c", "d", "b"];
    ordenar(miSecuencia);
    writeln("Secuencia ordenada:");
    foreach (cadena; miSecuencia)
        writeln(cadena);
}
```

Explicación del código:

* Se definen una serie de types alias para simplificar el código.
* Se crea un vector de pares de enteros y cadenas de caracteres.
* Se ordena el vector utilizando la función sort.
* Se busca un elemento en el vector utilizando la función find.
* Se elimina un elemento del vector utilizando la función erase.
* Se crea un mapa utilizando la clase HashMap.
* Se añaden pares al mapa.
* Se itera sobre el mapa utilizando la función foreach.
* Se busca un elemento en el mapa utilizando la función find.
* Se elimina un elemento del mapa utilizando la función erase.
* Se define una función genérica para ordenar una secuencia.
* Se demuestra el uso de la función genérica ordenando una secuencia de cadenas de caracteres.