```d
import std.stdio, std.algorithm, std.typecons;

class Persona {
    string nombre;
    int edad;

    Persona() { }
    Persona(string nombre, int edad) {
        this.nombre = nombre;
        this.edad = edad;
    }

    string to!string() {
        return "Nombre: " ~ nombre ~ ", Edad: " ~ edad.to!string;
    }
}

int main() {
    // Crear una lista de personas
    ArrayList personas = new ArrayList();
    personas.add(new Persona("Juan", 20));
    personas.add(new Persona("María", 25));
    personas.add(new Persona("Pedro", 30));

    // Ordenar la lista de personas por edad
    personas.sort(compareBy!(Persona, edad));

    // Imprimir la lista de personas ordenada
    for (Persona persona in personas) {
        writeln(persona);
    }

    return 0;
}
```

Explicación del código:

* El código define una clase `Persona` que representa una persona con un nombre y una edad.
* La función `main` crea una lista de `Persona` y añade tres personas a la lista.
* La función `sort` ordena la lista de `Persona` por edad utilizando la función `compareBy`.
* La función `compareBy` compara dos `Persona` y devuelve un entero que indica si la primera `Persona` es menor que, igual que o mayor que la segunda `Persona`.
* El bucle `for` recorre la lista de `Persona` ordenada e imprime cada `Persona` en la consola.