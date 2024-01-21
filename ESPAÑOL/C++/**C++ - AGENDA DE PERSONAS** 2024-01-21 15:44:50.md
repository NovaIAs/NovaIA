```c++
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>

using namespace std;

// Definición de la clase Persona
class Persona {
public:
  string nombre;
  int edad;
  string direccion;

  // Constructor
  Persona(string nombre, int edad, string direccion) {
    this->nombre = nombre;
    this->edad = edad;
    this->direccion = direccion;
  }

  // Imprime la información de la persona
  void imprimir() {
    cout << "Nombre: " << nombre << endl;
    cout << "Edad: " << edad << endl;
    cout << "Dirección: " << direccion << endl;
  }
};

// Definición de la clase Agenda
class Agenda {
public:
  vector<Persona> personas;

  // Agrega una persona a la agenda
  void agregarPersona(Persona persona) {
    personas.push_back(persona);
  }

  // Elimina una persona de la agenda
  void eliminarPersona(string nombre) {
    for (int i = 0; i < personas.size(); i++) {
      if (personas[i].nombre == nombre) {
        personas.erase(personas.begin() + i);
        break;
      }
    }
  }

  // Busca una persona en la agenda
  Persona buscarPersona(string nombre) {
    for (int i = 0; i < personas.size(); i++) {
      if (personas[i].nombre == nombre) {
        return personas[i];
      }
    }

    // Si no se encontró la persona, se retorna una persona vacía
    return Persona("", 0, "");
  }

  // Imprime todas las personas de la agenda
  void imprimirAgenda() {
    for (int i = 0; i < personas.size(); i++) {
      personas[i].imprimir();
      cout << endl;
    }
  }
};

// Función principal
int main() {
  // Se crea una agenda
  Agenda agenda;

  // Se agregan algunas personas a la agenda
  agenda.agregarPersona(Persona("Juan", 25, "Calle 1"));
  agenda.agregarPersona(Persona("María", 30, "Calle 2"));
  agenda.agregarPersona(Persona("Pedro", 35, "Calle 3"));

  // Se imprime la agenda
  agenda.imprimirAgenda();

  // Se busca una persona en la agenda
  Persona persona = agenda.buscarPersona("María");

  // Se imprime la persona encontrada
  persona.imprimir();

  // Se elimina una persona de la agenda
  agenda.eliminarPersona("Pedro");

  // Se imprime la agenda después de eliminar a la persona
  agenda.imprimirAgenda();

  return 0;
}
```

Este código crea una clase `Persona` con tres atributos: `nombre`, `edad` y `direccion`. 

Luego, se crea una clase `Agenda` que tiene un vector de `Persona` llamado `personas`. Esta clase tiene métodos para agregar, eliminar y buscar personas en la agenda, así como para imprimir toda la agenda.

Por último, en la función `main()` se crea una agenda, se agregan algunas personas, se busca una persona por su nombre, se elimina una persona y se imprime la agenda.

Este código es más complejo que el anterior porque utiliza clases y métodos para organizar el código de manera más eficiente y escalable.