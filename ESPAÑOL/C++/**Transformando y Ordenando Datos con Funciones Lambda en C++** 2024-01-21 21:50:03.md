```c++

// Biblioteca estándar de C++.
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <functional>

// Espacio de nombres de C++.
using namespace std;

// Función principal del programa.
int main() {
  // Creamos un vector de números enteros.
  vector<int> numeros = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

  // Creamos un mapa de cadenas a enteros.
  map<string, int> mapa = {{"uno", 1}, {"dos", 2}, {"tres", 3}, {"cuatro", 4}, {"cinco", 5}};

  // Creamos una función lambda que eleva un número al cuadrado.
  auto cuadrado = [](int x) { return x * x; };

  // Aplicamos la función lambda al vector de números enteros.
  vector<int> cuadrados = transform(numeros.begin(), numeros.end(), cuadrados);

  // Imprimimos el vector de números cuadrados.
  cout << "Números cuadrados:" << endl;
  for (int cuadrado : cuadrados) {
    cout << cuadrado << " ";
  }
  cout << endl;

  // Aplicamos la función lambda al mapa de cadenas a enteros.
  map<string, int> cuadrados_mapa = transform_keys(mapa, [](string clave) { return clave + " al cuadrado"; });

  // Imprimimos el mapa de cadenas a enteros cuadrados.
  cout << "Mapa de cadenas a enteros cuadrados:" << endl;
  for (auto par : cuadrados_mapa) {
    cout << par.first << " -> " << par.second << endl;
  }

  // Ordenamos el vector de números enteros en orden descendente.
  sort(numeros.begin(), numeros.end(), greater<int>());

  // Imprimimos el vector de números enteros ordenados en orden descendente.
  cout << "Números ordenados en orden descendente:" << endl;
  for (int numero : numeros) {
    cout << numero << " ";
  }
  cout << endl;

  // Retornamos 0 para indicar que el programa se ha ejecutado correctamente.
  return 0;
}

```

Explicación del código:

* La función `main()` es la función principal del programa y es donde se ejecuta el código.
* Creamos un vector de números enteros llamado `numeros` y un mapa de cadenas a enteros llamado `mapa`.
* Creamos una función lambda que eleva un número al cuadrado y la aplicamos al vector de números enteros. Imprimimos el vector de números cuadrados.
* Aplicamos la función lambda al mapa de cadenas a enteros y creamos un nuevo mapa de cadenas a enteros cuadrados. Imprimimos el nuevo mapa.
* Ordenamos el vector de números enteros en orden descendente y lo imprimimos.
* El programa retorna 0 para indicar que se ha ejecutado correctamente.