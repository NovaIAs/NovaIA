```c++
// Cabeceras necesarias
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>

using namespace std;

// Definición de la clase Estudiante
class Estudiante {
public:
  string nombre;
  int edad;
  double nota;

  Estudiante(string nombre, int edad, double nota) : nombre(nombre), edad(edad), nota(nota) {}

  // Operador de comparación para ordenar estudiantes por nota
  bool operator<(const Estudiante& otro) const {
    return nota < otro.nota;
  }
};

// Función para imprimir un estudiante
void imprimir_estudiante(const Estudiante& estudiante) {
  cout << "Nombre: " << estudiante.nombre << ", edad: " << estudiante.edad << ", nota: " << estudiante.nota << endl;
}

// Función principal
int main() {

  // Creación de una lista de estudiantes
  vector<Estudiante> estudiantes = {
    Estudiante("Juan", 20, 8.5),
    Estudiante("María", 21, 9.0),
    Estudiante("Pedro", 19, 7.5),
    Estudiante("Ana", 22, 8.0),
    Estudiante("Carlos", 23, 9.5),
  };

  // Ordenamiento de la lista de estudiantes por nota
  sort(estudiantes.begin(), estudiantes.end());

  // Impresión de la lista de estudiantes ordenada
  cout << "Lista de estudiantes ordenada por nota:" << endl;
  for_each(estudiantes.begin(), estudiantes.end(), imprimir_estudiante);

  // Creación de un mapa de estudiantes por nombre
  map<string, Estudiante> estudiantes_por_nombre;
  for (const Estudiante& estudiante : estudiantes) {
    estudiantes_por_nombre[estudiante.nombre] = estudiante;
  }

  // Búsqueda de un estudiante por nombre
  string nombre_buscado = "María";
  auto it = estudiantes_por_nombre.find(nombre_buscado);
  if (it != estudiantes_por_nombre.end()) {
    cout << "Estudiante encontrado:" << endl;
    imprimir_estudiante(it->second);
  } else {
    cout << "Estudiante no encontrado" << endl;
  }

  return 0;
}
```

Explicación del código:

1. En primer lugar, se incluye las cabeceras necesarias para el programa.
2. Se define una clase llamada `Estudiante` que representa a un estudiante con su nombre, edad y nota.
3. Se define una función llamada `imprimir_estudiante` que imprime la información de un estudiante.
4. En la función `main`, se crea una lista de estudiantes y se ordena por nota utilizando la función `sort`.
5. Luego, se crea un mapa de estudiantes por nombre utilizando la función `find`.
6. Finalmente, se busca un estudiante por nombre y se imprime su información.