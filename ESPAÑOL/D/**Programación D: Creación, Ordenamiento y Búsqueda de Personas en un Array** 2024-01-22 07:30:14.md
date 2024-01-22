```d
import std.stdio, std.string, std.range, std.algorithm, std.traits;

struct Persona {
    string nombre;
    int edad;

    Persona(string nombre, int edad) {
        this.nombre = nombre;
        this.edad = edad;
    }

    void print() {
        writefln("Nombre: %s, Edad: %d", nombre, edad);
    }
};

void main() {
    // Crear un array de personas
    Persona[] personas = [
        Persona("Juan", 20),
        Persona("María", 25),
        Persona("Pedro", 30),
        Persona("Ana", 35),
        Persona("Luis", 40)
    ];

    // Imprimir el array de personas
    foreach (persona; personas) {
        persona.print();
    }

    // Ordenar el array de personas por edad
    personas.sort!(&edad);

    // Imprimir el array de personas ordenado
    writefln("\nPersonas ordenadas por edad:");
    foreach (persona; personas) {
        persona.print();
    }

    // Buscar una persona por nombre
    string nombre = "Pedro";
    int index = personas.findIndexOf!(&nombre, &nombre);

    if (index == -1) {
        writefln("La persona '%s' no fue encontrada.", nombre);
    } else {
        writefln("La persona '%s' fue encontrada en el índice %d.", nombre, index);
    }

    // Eliminar una persona por índice
    personas.removeAt(2);

    // Imprimir el array de personas después de eliminar una persona
    writefln("\nPersonas después de eliminar una persona:");
    foreach (persona; personas) {
        persona.print();
    }
}
```

Explicación:

* El código comienza importando los módulos necesarios.
* Luego se define un struct llamado `Persona` que tiene dos campos: `nombre` y `edad`.
* A continuación se crea un array de personas utilizando el literal de array `[]`.
* El array de personas se imprime utilizando un bucle `foreach`.
* Luego se ordena el array de personas por edad utilizando el método `sort!`.
* El array de personas ordenado se imprime utilizando otro bucle `foreach`.
* A continuación se busca una persona por nombre utilizando el método `findIndexOf!`.
* Si la persona se encuentra, se imprimen su nombre e índice en el array. De lo contrario, se imprime un mensaje indicando que la persona no fue encontrada.
* Por último, se elimina una persona del array por índice utilizando el método `removeAt`.
* El array de personas después de eliminar una persona se imprime utilizando un bucle `foreach`.