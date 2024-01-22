```d
import std.stdio, std.algorithm, std.string;

struct Persona {
    string nombre;
    int edad;
    Persona(string nombre, int edad) pure nothrow {
        this.nombre = nombre;
        this.edad = edad;
    }
    string toString() const pure nothrow {
        return "Persona " + nombre + " con " + edad.toString() + " años de edad.";
    }
};

int main() {
    // Crear un array de personas
    Persona[] personas = new Persona[](new Persona("Juan", 20), new Persona("María", 25), new Persona("Pedro", 30));

    // Ordenar el array de personas por edad
    sort(personas, [](const Persona& a, const Persona& b) pure nothrow {
        return a.edad < b.edad;
    });

    // Imprimir el array de personas
    foreach (Persona persona; personas) {
        writeln(persona.toString());
    }

    // Buscar a una persona en el array de personas
    Persona juan = new Persona("Juan", 20);
    int indiceDeJuan = findIndex(personas, juan, [](const Persona& a, const Persona& b) pure nothrow {
        return a.nombre == b.nombre && a.edad == b.edad;
    });

    if (indiceDeJuan != -1) {
        writeln("Juan encontrado en el índice " + indiceDeJuan.toString());
    } else {
        writeln("Juan no encontrado");
    }

    return 0;
}
```

Explicación del código:

* Se define una estructura `Persona` con dos miembros: `nombre` (de tipo `string`) y `edad` (de tipo `int`).
* Se define una función `main` que es el punto de entrada del programa.
* Se crea un array de personas `personas` con tres elementos: `Juan` (20 años), `María` (25 años) y `Pedro` (30 años).
* Se ordena el array de personas por edad utilizando el algoritmo de ordenación `sort`.
* Se imprime el array de personas utilizando la función `foreach`.
* Se busca a una persona en el array de personas utilizando el algoritmo de búsqueda `findIndex`.
* Si se encuentra la persona, se imprime su índice en el array. Si no se encuentra, se imprime un mensaje indicando que no se encontró.