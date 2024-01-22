```d
import std.stdio, std.algorithm, std.range, std.typecons;

struct Person {
    string name;
    int age;
}

auto main() pure {
    immutable persons = [
        Person("Juan", 20),
        Person("Maria", 25),
        Person("Pedro", 30),
        Person("Ana", 35),
        Person("Luis", 40),
    ];

    // Imprimir los nombres de las personas mayores de 30 años
    foreach (person; persons) {
        if (person.age > 30)
            writeln(person.name);
    }

    // Ordenar las personas por edad
    persons.sort!((a, b) => a.age <=> b.age);

    // Imprimir los nombres de las personas ordenadas por edad
    foreach (person; persons) {
        writeln(person.name);
    }

    // Obtener la persona más joven
    auto youngest = persons.min!((a, b) => a.age <=> b.age);

    // Imprimir el nombre de la persona más joven
    writeln(youngest.name);

    // Obtener la persona más vieja
    auto oldest = persons.max!((a, b) => a.age <=> b.age);

    // Imprimir el nombre de la persona más vieja
    writeln(oldest.name);

    // Obtener la edad promedio de las personas
    immutable ages = persons.map!(person => person.age);
    immutable averageAge = ages.sum / ages.length;

    // Imprimir la edad promedio
    writeln(averageAge);
}
```

Este código es un programa que utiliza la biblioteca estándar de D para realizar diversas operaciones con una lista de personas.

El programa primero define una estructura `Person` que tiene dos miembros: `name` y `age`.

Luego, el programa define una lista de personas llamada `persons`.

A continuación, el programa realiza las siguientes operaciones con la lista de personas:

* Imprime los nombres de las personas mayores de 30 años.

* Ordena las personas por edad.

* Imprime los nombres de las personas ordenadas por edad.

* Obtiene la persona más joven de la lista.

* Imprime el nombre de la persona más joven.

* Obtiene la persona más vieja de la lista.

* Imprime el nombre de la persona más vieja.

* Obtiene la edad promedio de las personas de la lista.

* Imprime la edad promedio.