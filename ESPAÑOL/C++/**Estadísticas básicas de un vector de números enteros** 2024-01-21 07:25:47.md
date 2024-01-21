```c++
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>

using namespace std;

// Función para imprimir un vector de números enteros
void imprimir_vector(vector<int> v) {
  for (int i = 0; i < v.size(); i++) {
    cout << v[i] << " ";
  }
  cout << endl;
}

// Función para encontrar el elemento máximo en un vector de números enteros
int encontrar_maximo(vector<int> v) {
  int maximo = v[0];
  for (int i = 1; i < v.size(); i++) {
    if (v[i] > maximo) {
      maximo = v[i];
    }
  }
  return maximo;
}

// Función para encontrar la suma de los elementos en un vector de números enteros
int encontrar_suma(vector<int> v) {
  int suma = 0;
  for (int i = 0; i < v.size(); i++) {
    suma += v[i];
  }
  return suma;
}

// Función para encontrar la media de los elementos en un vector de números enteros
double encontrar_media(vector<int> v) {
  int suma = encontrar_suma(v);
  return (double)suma / v.size();
}

// Función para encontrar la mediana de los elementos en un vector de números enteros
double encontrar_mediana(vector<int> v) {
  sort(v.begin(), v.end());
  int mitad = v.size() / 2;
  if (v.size() % 2 == 0) {
    return (double)(v[mitad] + v[mitad - 1]) / 2;
  } else {
    return v[mitad];
  }
}

// Función para encontrar la moda de los elementos en un vector de números enteros
int encontrar_moda(vector<int> v) {
  map<int, int> mapa;
  for (int i = 0; i < v.size(); i++) {
    mapa[v[i]]++;
  }
  int maximo = 0;
  int moda;
  for (auto it = mapa.begin(); it != mapa.end(); it++) {
    if (it->second > maximo) {
      maximo = it->second;
      moda = it->first;
    }
  }
  return moda;
}

// Función principal
int main() {
  // Crear un vector de números enteros
  vector<int> v = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

  // Imprimir el vector original
  cout << "Vector original: ";
  imprimir_vector(v);

  // Encontrar el elemento máximo en el vector
  int maximo = encontrar_maximo(v);
  cout << "Elemento máximo: " << maximo << endl;

  // Encontrar la suma de los elementos en el vector
  int suma = encontrar_suma(v);
  cout << "Suma de los elementos: " << suma << endl;

  // Encontrar la media de los elementos en el vector
  double media = encontrar_media(v);
  cout << "Media de los elementos: " << media << endl;

  // Encontrar la mediana de los elementos en el vector
  double mediana = encontrar_mediana(v);
  cout << "Mediana de los elementos: " << mediana << endl;

  // Encontrar la moda de los elementos en el vector
  int moda = encontrar_moda(v);
  cout << "Moda de los elementos: " << moda << endl;

  return 0;
}
```

Este código calcula las estadísticas básicas de un vector de números enteros. Las estadísticas calculadas son:

* Elemento máximo
* Suma de los elementos
* Media de los elementos
* Mediana de los elementos
* Moda de los elementos

El código utiliza las siguientes funciones:

* `imprimir_vector()`: Imprime un vector de números enteros.
* `encontrar_maximo()`: Encuentra el elemento máximo en un vector de números enteros.
* `encontrar_suma()`: Encuentra la suma de los elementos en un vector de números enteros.
* `encontrar_media()`: Encuentra la media de los elementos en un vector de números enteros.
* `encontrar_mediana()`: Encuentra la mediana de los elementos en un vector de números enteros.
* `encontrar_moda()`: Encuentra la moda de los elementos en un vector de números enteros.

La función `main()` crea un vector de números enteros, imprime el vector original y llama a las funciones para calcular las estadísticas básicas del vector. Las estadísticas calculadas se imprimen en la consola.