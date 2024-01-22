```d
import std.algorithm, std.containers;
import std.stdio, std.string;
import std.range;

void main() {
    immutable vec = Vector!char["a", "b", "c"];

    // Iterar sobre el vector
    //    recorre sobre elem en vec
    //    escribe "¡Hola! ", elem
    foreach (auto elem; vec) {
        writefln("¡Hola! %s") % elem;
    }

    // Filtrar el vector
    // crea un nuevo vector con los elementos de vec que sean mayores que "b"
    auto filtrado = vec.filter!char((c) => c > 'b');

    // Ordenar el vector
    //    ordena vec
    //    usa el comparador lambda para comparar los elementos
    vec.sort!char((a, b) => a < b);

    // Encontrar el primer elemento que cumpla con la condición
    //    encuentra el primer elemento de vec que coincida con la condición
    auto encontrado = vec.findFirst!char((c) => c == 'c');

    // Encontrar el índice del primer elemento que cumpla con la condición
    //    encuentra el índice del primer elemento de vec que coincida con la condición
    auto índice = vec.findIndex!char((c) => c == 'c');

    // Eliminar el primer elemento que cumpla con la condición
    //    elimina el primer elemento de vec que coincida con la condición
    vec.removeFirst!char((c) => c == 'c');

    // Eliminar el elemento en el índice especificado
    //    elimina el elemento en el índice 1 de vec
    vec.removeAtIndex!char(1);

    // Insertar un elemento en el índice especificado
    //    inserta el elemento 'd' en el índice 1 de vec
    vec.insertAt!char(1, 'd');

    // Concatenar dos vectores
    //    concatena vec y el vector ["e", "f", "g"]
    vec ~= ["e", "f", "g"];

    // Convertir el vector a una cadena
    //    convierte vec a una cadena
    auto cadena = vec.join;

    // Mostrar el vector
    //    escribe el vector vec
    writeln(vec);

    // Mostrar la cadena
    //    escribe la cadena
    writeln(cadena);
}
```

Este código es un ejemplo de cómo utilizar algunas de las funciones de la biblioteca estándar de D para trabajar con vectores. El código crea un vector de caracteres, lo itera, lo filtra, lo ordena, encuentra el primer elemento que cumpla con una condición, elimina el primer elemento que cumpla con una condición, elimina el elemento en un índice especificado, inserta un elemento en un índice especificado, concatena dos vectores, convierte el vector a una cadena y muestra el vector y la cadena.