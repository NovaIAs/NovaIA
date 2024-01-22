```c++
// Este código implementa el algoritmo de búsqueda binaria en C++.

#include <iostream>
#include <vector>

using namespace std;

// Función que realiza la búsqueda binaria en un vector ordenado.
int busquedaBinaria(vector<int>& vec, int valor) {
  int izquierda = 0;
  int derecha = vec.size() - 1;

  while (izquierda <= derecha) {
    int mitad = (izquierda + derecha) / 2;

    if (vec[mitad] == valor) {
      return mitad;
    } else if (vec[mitad] < valor) {
      izquierda = mitad + 1;
    } else {
      derecha = mitad - 1;
    }
  }

  return -1;
}

int main() {
  // Creamos un vector de números ordenados.
  vector<int> vec = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19};

  // Solicitamos al usuario que ingrese un valor a buscar.
  int valor;
  cout << "Ingrese un valor a buscar: ";
  cin >> valor;

  // Realizamos la búsqueda binaria.
  int resultado = busquedaBinaria(vec, valor);

  // Mostramos el resultado de la búsqueda.
  if (resultado == -1) {
    cout << "El valor no se encuentra en el vector." << endl;
  } else {
    cout << "El valor se encuentra en la posición " << resultado << endl;
  }

  return 0;
}
```

El código anterior implementa el algoritmo de búsqueda binaria en C++. La búsqueda binaria es un algoritmo de búsqueda eficiente que se utiliza para buscar un valor específico en un vector ordenado. El algoritmo funciona dividiendo repetidamente el vector por la mitad hasta que se encuentra el valor o se determina que no está en el vector.

El código primero crea un vector de números ordenados. A continuación, solicita al usuario que ingrese un valor a buscar. Luego, realiza la búsqueda binaria utilizando la función `busquedaBinaria()`. La función `busquedaBinaria()` toma como parámetros el vector ordenado y el valor a buscar. La función devuelve la posición del valor en el vector si se encuentra, o -1 si no se encuentra.

El código finalmente muestra el resultado de la búsqueda. Si el valor se encuentra en el vector, se muestra la posición del valor. Si el valor no se encuentra en el vector, se muestra un mensaje indicando que el valor no se encuentra en el vector.