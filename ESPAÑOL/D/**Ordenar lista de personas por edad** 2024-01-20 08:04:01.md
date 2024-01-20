```d
// Programa principal

import std.stdio, std.algorithm;

// Clase que representa una persona
class Persona {
    private string _nombre;
    private int _edad;

    public Persona(string nombre, int edad) {
        _nombre = nombre;
        _edad = edad;
    }

    public string getNombre() {
        return _nombre;
    }

    public int getEdad() {
        return _edad;
    }

    public override string toString() {
        return "Persona: {nombre: " + _nombre + ", edad: " + _edad + "}";
    }
}

// Clase que representa una lista de personas
class ListaPersonas {
    private Persona[] _personas;
    private int _count;

    public ListaPersonas() {
        _personas = new Persona[10];
        _count = 0;
    }

    public void agregar(Persona persona) {
        if (_count == _personas.length) {
            Persona[] newPersonas = new Persona[_personas.length * 2];
            for (int i = 0; i < _count; i++) {
                newPersonas[i] = _personas[i];
            }
            _personas = newPersonas;
        }
        _personas[_count++] = persona;
    }

    public Persona[] toArray() {
        Persona[] personas = new Persona[_count];
        for (int i = 0; i < _count; i++) {
            personas[i] = _personas[i];
        }
        return personas;
    }
}

// Función que compara dos personas por edad
int compararPersonasPorEdad(Persona p1, Persona p2) {
    return p1.getEdad() - p2.getEdad();
}

// Función principal
void main() {
    ListaPersonas personas = new ListaPersonas();

    personas.agregar(new Persona("Juan", 20));
    personas.agregar(new Persona("María", 25));
    personas.agregar(new Persona("Pedro", 30));
    personas.agregar(new Persona("Ana", 35));
    personas.agregar(new Persona("Carlos", 40));

    // Ordenar la lista de personas por edad
    Persona[] personasOrdenadas = personas.toArray();
    sort!(personasOrdenadas, compararPersonasPorEdad);

    // Imprimir la lista de personas ordenadas
    for (Persona persona : personasOrdenadas) {
        writefln(persona);
    }
}
```

Este código crea una lista de personas y luego la ordena por edad. La lista de personas se implementa utilizando una clase `ListaPersonas` que tiene un campo privado `_personas` que es un arreglo de personas. El campo privado `_count` mantiene el número de personas en la lista.

La función `agregar` agrega una persona a la lista de personas. Si la lista de personas está llena, se crea un nuevo arreglo de personas con el doble del tamaño del arreglo anterior y las personas de la lista de personas anterior se copian al nuevo arreglo.

La función `toArray` convierte la lista de personas en un arreglo de personas.

La función `compararPersonasPorEdad` compara dos personas por edad. La función devuelve un número negativo si la primera persona es más joven que la segunda persona, un número cero si las dos personas tienen la misma edad y un número positivo si la primera persona es mayor que la segunda persona.

La función `sort!` ordena un arreglo de personas utilizando la función `compararPersonasPorEdad` como función de comparación.

La función `writefln` imprime una persona en la consola.

El código principal crea una lista de personas y luego llama a la función `agregar` para agregar cinco personas a la lista de personas. Luego, el código principal llama a la función `toArray` para convertir la lista de personas en un arreglo de personas. Luego, el código principal llama a la función `sort!` para ordenar el arreglo de personas por edad. Finalmente, el código principal llama a la función `writefln` para imprimir cada persona en la consola.