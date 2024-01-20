```d
import std.stdio, std.algorithm, std.range, std.string, std.typecons;
import std.algorithm.find;
import std.algorithm.findIf;
import std.algorithm.count;
import std.algorithm.reduce;
import std.algorithm.sort;
import std.algorithm.reverse;

typedef Libro = struct {
    immutable int isbn;
    immutable string title;
    immutable int year;
    immutable string author;
    immutable double price;
};

auto libro_comparer = [](Libro a, Libro b) pure nothrow return a.isbn < b.isbn;

void main() {
    auto libros = new Libro[] {
        new Libro(1234567890, "El Quijote", 1605, "Miguel de Cervantes Saavedra", 12.99),
        new Libro(9876543210, "La Divina Comedia", 1308, "Dante Alighieri", 14.99),
        new Libro(1112223333, "Cien años de soledad", 1967, "Gabriel García Márquez", 16.99),
        new Libro(4445556666, "El Señor de los Anillos", 1954, "J.R.R. Tolkien", 24.99),
        new Libro(7778889999, "Harry Potter y la Piedra Filosofal", 1997, "J.K. Rowling", 11.99),
    };

    // Ordenar los libros por ISBN.
    sort(libros, libro_comparer);

    // Buscar un libro por ISBN.
    auto isbn = 1234567890;
    auto libro = find(libros, isbn, libro_comparer);
    if (libro != null) {
        writefln("Libro encontrado: %s", libro.title);
    } else {
        writefln("Libro no encontrado.");
    }

    // Buscar un libro por título.
    auto titulo = "Cien años de soledad";
    auto libro2 = findIf(libros, [](Libro libro) pure nothrow return libro.title == titulo);
    if (libro2 != null) {
        writefln("Libro encontrado: %s", libro2.title);
    } else {
        writefln("Libro no encontrado.");
    }

    // Contar cuántos libros tienen un precio mayor a 15.
    auto libros_caros = count(libros, [](Libro libro) pure nothrow return libro.price > 15);
    writefln("Hay %d libros caros.", libros_caros);

    // Calcular el precio total de los libros.
    auto precio_total = reduce(libros, 0.0, [](double total, Libro libro) pure nothrow return total + libro.price);
    writefln("El precio total de los libros es: %f", precio_total);

    // Mostrar los títulos de los libros en orden alfabético inverso.
    reverse(libros);
    foreach (libro; libros) {
        writefln(libro.title);
    }
}
```

Este código es un programa completo en la lengua D. El programa tiene una función principal, `main`, que se ejecuta cuando se ejecuta el programa.

El programa crea un arreglo de libros, `libros`, con 5 elementos. Cada elemento del arreglo es una estructura `Libro` que contiene los siguientes campos:

* `isbn`: el número ISBN del libro.
* `title`: el título del libro.
* `year`: el año en que se publicó el libro.
* `author`: el nombre del autor del libro.
* `price`: el precio del libro.

El programa then ordena los libros por su número ISBN usando la función `sort`.

El programa then busca un libro con el número ISBN `1234567890` usando la función `find`. Si se encuentra el libro, el programa imprime su título. De lo contrario, el programa imprime un mensaje que indica que el libro no se ha encontrado.

El programa then busca un libro con el título `"Cien años de soledad"` usando la función `findIf`. Si se encuentra el libro, el programa imprime su título. De lo contrario, el programa imprime un mensaje que indica que el libro no se ha encontrado.

El programa then cuenta el número de libros que tienen un precio mayor de 15 usando la función `count`. El programa then imprime el resultado.

El programa then calcula el precio total de los libros usando la función `reduce`. El programa then imprime el resultado.

Finalmente, el programa muestra los títulos de los libros en orden alfabético inverso usando la función `reverse`. El programa then imprime los títulos.