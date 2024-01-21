```c++
// Librerías necesarias
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>

using namespace std;

// Definición de una clase Persona
class Persona {
public:
    string nombre;
    int edad;

    // Constructor
    Persona(string nombre, int edad) {
        this->nombre = nombre;
        this->edad = edad;
    }

    // Método para imprimir la información de la persona
    void imprimir() {
        cout << "Nombre: " << nombre << ", Edad: " << edad << endl;
    }
};

// Definición de una clase GestorPersonas
class GestorPersonas {
public:
    // Mapa para almacenar las personas
    map<string, Persona> personas;

    // Método para añadir una persona al mapa
    void addPersona(Persona persona) {
        personas[persona.nombre] = persona;
    }

    // Método para eliminar una persona del mapa
    void deletePersona(string nombre) {
        personas.erase(nombre);
    }

    // Método para buscar una persona en el mapa
    Persona* searchPersona(string nombre) {
        if (personas.find(nombre) != personas.end()) {
            return &personas[nombre];
        } else {
            return nullptr;
        }
    }

    // Método para imprimir todas las personas del mapa
    void printPersonas() {
        for (auto it = personas.begin(); it != personas.end(); it++) {
            it->second.imprimir();
        }
    }
};

// Función principal
int main() {
    // Creamos un gestor de personas
    GestorPersonas gestorPersonas;

    // Añadimos algunas personas al gestor
    gestorPersonas.addPersona(Persona("Juan", 20));
    gestorPersonas.addPersona(Persona("María", 25));
    gestorPersonas.addPersona(Persona("Pedro", 30));

    // Imprimimos todas las personas del gestor
    gestorPersonas.printPersonas();

    // Buscamos una persona en el gestor
    Persona* persona = gestorPersonas.searchPersona("María");

    // Si la persona existe, la imprimimos
    if (persona != nullptr) {
        persona->imprimir();
    } else {
        cout << "La persona no existe" << endl;
    }

    // Eliminamos una persona del gestor
    gestorPersonas.deletePersona("Pedro");

    // Imprimimos todas las personas del gestor
    gestorPersonas.printPersonas();

    return 0;
}
```

Explicación del código:

* La clase `Persona` representa a una persona con un nombre y una edad.
* La clase `GestorPersonas` gestiona un conjunto de personas.
* La función `main()` crea un gestor de personas, añade algunas personas al gestor, busca una persona en el gestor, elimina una persona del gestor e imprime todas las personas del gestor.

El código utiliza un mapa para almacenar las personas. Un mapa es una estructura de datos que asocia claves con valores. En este caso, la clave es el nombre de la persona y el valor es la persona misma.

El código también utiliza punteros. Un puntero es una variable que almacena la dirección de otra variable. En este caso, el puntero `persona` almacena la dirección de la persona que se encuentra en el mapa.

El código también utiliza la biblioteca estándar de C++, que proporciona una serie de funciones y clases útiles. Por ejemplo, la función `cout` se utiliza para imprimir datos en la consola.