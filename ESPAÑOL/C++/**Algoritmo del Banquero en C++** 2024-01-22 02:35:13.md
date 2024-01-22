```c++
// Este programa es una implementación simplificada del Algoritmo del Banquero en C++.
// El Algoritmo del Banquero es un algoritmo de asignación de recursos que evita el interbloqueo.

#include <iostream>
#include <vector>

using namespace std;

// Función para comprobar si un proceso puede solicitar recursos de forma segura.
bool isSafe(vector<vector<int>>& available, vector<vector<int>>& allocation, vector<vector<int>>& max) {
  // Crear una matriz de trabajo que sea una copia de la matriz de asignación.
  vector<vector<int>> work = available;

  // Crear un vector de booleanos para indicar si un proceso está terminado.
  vector<bool> finished(allocation.size(), false);

  // Mientras haya procesos no terminados, hacer lo siguiente:
  while (true) {
    // Buscar un proceso no terminado que pueda solicitar recursos de forma segura.
    int i = -1;
    for (int j = 0; j < allocation.size(); ++j) {
      if (!finished[j] && canRequest(work, allocation[j], max[j])) {
        i = j;
        break;
      }
    }

    // Si no se encuentra ningún proceso que pueda solicitar recursos de forma segura, devolver false.
    if (i == -1) {
      return false;
    }

    // Asignar los recursos solicitados al proceso i.
    for (int j = 0; j < available.size(); ++j) {
      work[j] += allocation[i][j];
    }

    // Marcar el proceso i como terminado.
    finished[i] = true;

    // Comprobar si todos los procesos están terminados.
    if (allFinished(finished)) {
      return true;
    }
  }
}

// Función para comprobar si un proceso puede solicitar recursos de forma segura.
bool canRequest(vector<int>& available, vector<int>& allocation, vector<int>& max) {
  // Comprobar si el proceso ya está asignado con el máximo de recursos.
  for (int i = 0; i < available.size(); ++i) {
    if (allocation[i] == max[i]) {
      return false;
    }
  }

  // Comprobar si el proceso puede solicitar recursos sin exceder el máximo.
  for (int i = 0; i < available.size(); ++i) {
    if (available[i] < max[i] - allocation[i]) {
      return false;
    }
  }

  // El proceso puede solicitar recursos de forma segura.
  return true;
}

// Función para comprobar si todos los procesos están terminados.
bool allFinished(vector<bool>& finished) {
  for (bool f : finished) {
    if (!f) {
      return false;
    }
  }
  return true;
}

int main() {
  // Crear los datos de entrada.

  // Recursos disponibles.
  vector<vector<int>> available = {
    {3, 3, 2},
    {1, 0, 2},
    {2, 1, 1},
  };

  // Recursos asignados a los procesos.
  vector<vector<int>> allocation = {
    {0, 0, 1},
    {2, 0, 0},
    {3, 0, 1},
  };

  // Máximos recursos que los procesos pueden solicitar.
  vector<vector<int>> max = {
    {7, 5, 3},
    {6, 1, 2},
    {8, 0, 2},
  };

  // Comprobar si el sistema está en un estado seguro.
  bool safe = isSafe(available, allocation, max);

  // Mostrar el resultado.
  cout << "Estado seguro: " << (safe ? "Sí" : "No") << endl;

  return 0;
}
```

Explicación:

* El código implementa el Algoritmo del Banquero en C++.

* El Algoritmo del Banquero es un algoritmo de asignación de recursos que evita el interbloqueo.

* El interbloqueo es una situación en la que dos o más procesos esperan recursos que están asignados a otros procesos.

* El Algoritmo del Banquero funciona manteniendo una matriz de recursos disponibles, una matriz de recursos asignados y una matriz de recursos máximos.

* El algoritmo comprueba si un proceso puede solicitar recursos de forma segura.

* Un proceso puede solicitar recursos de forma segura si el proceso ya no está asignado con el máximo de recursos y si el proceso puede solicitar recursos sin exceder el máximo.

* El algoritmo también comprueba si todos los procesos están terminados.

* El algoritmo devuelve true si el sistema está en un estado seguro, y false en caso contrario.

* El código de ejemplo crea los datos de entrada y luego llama a la función `isSafe()` para comprobar si el sistema está en un estado seguro.

* El código de ejemplo muestra el resultado en la consola.