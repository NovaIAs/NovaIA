```c++
// Importar las bibliotecas necesarias
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>

// Definir una clase Persona con atributos y métodos
class Persona {
public:
  // Atributos
  std::string nombre;
  int edad;
  std::string ciudad;

  // Métodos
  void saludar() {
    std::cout << "Hola, mi nombre es " << nombre << " y tengo " << edad << " años." << std::endl;
  }
};

// Definir una clase Estudiante que hereda de la clase Persona
class Estudiante : public Persona {
public:
  // Atributos
  std::string universidad;
  std::string carrera;

  // Métodos
  void estudiar() {
    std::cout << "Estoy estudiando en la universidad " << universidad << " la carrera de " << carrera << "." << std::endl;
  }
};

// Definir una clase Profesor que hereda de la clase Persona
class Profesor : public Persona {
public:
  // Atributos
  std::string universidad;
  std::string materia;

  // Métodos
  void enseñar() {
    std::cout << "Estoy enseñando la materia " << materia << " en la universidad " << universidad << "." << std::endl;
  }
};

// Crear una función para imprimir una lista de personas
void imprimirListaPersonas(std::vector<Persona*> personas) {
  for (Persona* persona : personas) {
    persona->saludar();
  }
}

// Crear una función para ordenar una lista de personas por edad
bool compararPersonasPorEdad(Persona* persona1, Persona* persona2) {
  return persona1->edad < persona2->edad;
}

// Crear una función para encontrar la persona más joven en una lista
Persona* encontrarPersonaMasJoven(std::vector<Persona*> personas) {
  Persona* personaMasJoven = personas[0];
  for (Persona* persona : personas) {
    if (persona->edad < personaMasJoven->edad) {
      personaMasJoven = persona;
    }
  }
  return personaMasJoven;
}

// Crear una función para agrupar a las personas por ciudad
std::map<std::string, std::vector<Persona*>> agruparPersonasPorCiudad(std::vector<Persona*> personas) {
  std::map<std::string, std::vector<Persona*>> personasPorCiudad;
  for (Persona* persona : personas) {
    personasPorCiudad[persona->ciudad].push_back(persona);
  }
  return personasPorCiudad;
}

// Crear una función para imprimir un mapa de personas agrupadas por ciudad
void imprimirMapaPersonasPorCiudad(std::map<std::string, std::vector<Persona*>> personasPorCiudad) {
  for (auto it = personasPorCiudad.begin(); it != personasPorCiudad.end(); ++it) {
    std::cout << "Ciudad: " << it->first << std::endl;
    imprimirListaPersonas(it->second);
    std::cout << std::endl;
  }
}

// Crear una función para encontrar la persona con el nombre más largo
Persona* encontrarPersonaConNombreMasLargo(std::vector<Persona*> personas) {
  Persona* personaConNombreMasLargo = personas[0];
  for (Persona* persona : personas) {
    if (persona->nombre.length() > personaConNombreMasLargo->nombre.length()) {
      personaConNombreMasLargo = persona;
    }
  }
  return personaConNombreMasLargo;
}

// Crear una función para encontrar el nombre más común entre las personas
std::string encontrarNombreMasComun(std::vector<Persona*> personas) {
  std::map<std::string, int> nombreFrecuencias;
  for (Persona* persona : personas) {
    nombreFrecuencias[persona->nombre]++;
  }
  std::string nombreMasComun;
  int frecuenciaMasAlta = 0;
  for (auto it = nombreFrecuencias.begin(); it != nombreFrecuencias.end(); ++it