```c++
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <fstream>
#include <exception>

using namespace std;

// Definición de la clase Persona
class Persona {
public:
  // Constructor
  Persona(string nombre, int edad) {
    this->nombre = nombre;
    this->edad = edad;
  }

  // Métodos getters
  string getNombre() {
    return nombre;
  }

  int getEdad() {
    return edad;
  }

  // Método para imprimir la información de la persona
  void imprimir() {
    cout << "Nombre: " << nombre << endl;
    cout << "Edad: " << edad << endl;
  }

private:
  string nombre;
  int edad;
};

// Definición de la clase Agenda
class Agenda {
public:
  // Constructor
  Agenda() {
    personas.clear();
  }

  // Métodos para agregar y eliminar personas de la agenda
  void agregarPersona(Persona persona) {
    personas.push_back(persona);
  }

  void eliminarPersona(string nombre) {
    for (vector<Persona>::iterator it = personas.begin(); it != personas.end(); it++) {
      if (it->getNombre() == nombre) {
        personas.erase(it);
        break;
      }
    }
  }

  // Método para buscar una persona en la agenda
  Persona buscarPersona(string nombre) {
    for (vector<Persona>::iterator it = personas.begin(); it != personas.end(); it++) {
      if (it->getNombre() == nombre) {
        return *it;
      }
    }

    throw runtime_error("Persona no encontrada");
  }

  // Método para imprimir la lista de personas de la agenda
  void imprimirAgenda() {
    cout << "Lista de personas:" << endl;
    for (vector<Persona>::iterator it = personas.begin(); it != personas.end(); it++) {
      it->imprimir();
      cout << endl;
    }
  }

  // Método para guardar la agenda en un archivo
  void guardarAgenda(string nombreArchivo) {
    ofstream archivo(nombreArchivo);
    if (!archivo) {
      throw runtime_error("No se pudo abrir el archivo");
    }

    for (vector<Persona>::iterator it = personas.begin(); it != personas.end(); it++) {
      archivo << it->getNombre() << "," << it->getEdad() << endl;
    }

    archivo.close();
  }

  // Método para cargar la agenda desde un archivo
  void cargarAgenda(string nombreArchivo) {
    ifstream archivo(nombreArchivo);
    if (!archivo) {
      throw runtime_error("No se pudo abrir el archivo");
    }

    string linea;
    while (getline(archivo, linea)) {
      int pos = linea.find(",");
      if (pos != string::npos) {
        string nombre = linea.substr(0, pos);
        string edad = linea.substr(pos + 1);

        Persona persona(nombre, stoi(edad));
        personas.push_back(persona);
      }
    }

    archivo.close();
  }

private:
  vector<Persona> personas;
};

// Función principal
int main() {
  try {
    // Creación de la agenda
    Agenda agenda;

    // Agregar algunas personas a la agenda
    agenda.agregarPersona(Persona("Juan", 20));
    agenda.agregarPersona(Persona("María", 25));
    agenda.agregarPersona(Persona("Pedro", 30));

    // Imprimir la lista de personas de la agenda
    agenda.imprimirAgenda();

    // Buscar una persona en la agenda
    Persona persona = agenda.buscarPersona("María");
    cout << "Persona encontrada: " << endl;
    persona.imprimir();

    // Eliminar una persona de la agenda
    agenda.eliminarPersona("Pedro");

    // Imprimir la lista de personas de la agenda
    agenda.imprimirAgenda();

    // Guardar la agenda en un archivo
    agenda.guardarAgenda("agenda.txt");

    // Cargar la agenda desde un archivo
    agenda.cargarAgenda("agenda.txt");

    // Imprimir la lista de personas de la agenda
    agenda.imprimirAgenda();
  } catch (exception& e) {
    cout << "Error: " << e.what() << endl;
  }

  return 0;
}
```

Explicación del código:

* La clase `Persona` representa a una persona con un nombre y una edad.
* La clase `Agenda` representa una agenda que puede almacenar una lista de personas.
* La función principal `main()` crea una instancia de la clase `Agenda`, agrega algunas personas a la agenda, imprime la lista de personas de la agenda, busca una persona en la agenda, elimina una persona de la agenda, guarda la agenda en un archivo, carga la agenda desde un archivo e imprime la lista de personas de la agenda.
* El código utiliza la biblioteca estándar de C++ para manejar archivos, excepciones y algoritmos.