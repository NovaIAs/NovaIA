```c++
// Programa para encontrar el valor máximo y mínimo de un conjunto de números.

#include <iostream>

using namespace std;

// Función para encontrar el valor máximo y mínimo de un conjunto de números.
pair<int, int> encontrar_max_min(int* array, int size) {
  // Inicializar el valor máximo y mínimo.
  int max = array[0];
  int min = array[0];

  // Recorrer el conjunto de números.
  for (int i = 1; i < size; i++) {
    // Actualizar el valor máximo y mínimo si es necesario.
    if (array[i] > max) {
      max = array[i];
    }
    if (array[i] < min) {
      min = array[i];
    }
  }

  // Devolver el valor máximo y mínimo.
  return make_pair(max, min);
}

// Función principal.
int main() {
  // Crear un conjunto de números.
  int array[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  int size = sizeof(array) / sizeof(array[0]);

  // Encontrar el valor máximo y mínimo del conjunto de números.
  pair<int, int> max_min = encontrar_max_min(array, size);

  // Imprimir el valor máximo y mínimo.
  cout << "Valor máximo: " << max_min.first << endl;
  cout << "Valor mínimo: " << max_min.second << endl;

  return 0;
}
```

Explicación:

* La función `encontrar_max_min` toma un conjunto de números y su tamaño como argumentos, devuelve una tupla con el valor máximo y mínimo del conjunto de números.
* La función principal crea un conjunto de números y encuentra su valor máximo y mínimo utilizando la función `encontrar_max_min`.
* Imprime el valor máximo y mínimo en la consola.