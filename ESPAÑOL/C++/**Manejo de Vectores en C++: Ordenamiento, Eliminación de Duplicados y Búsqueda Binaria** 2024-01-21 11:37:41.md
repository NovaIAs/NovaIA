```c++
// Incluir las bibliotecas necesarias
#include <iostream>
#include <vector>
#include <algorithm>

// Definir el tipo de dato para los elementos del vector
typedef int MiTipoDeDato;

// Crear la función para comparar los elementos del vector
bool comparar(const MiTipoDeDato& a, const MiTipoDeDato& b) {
  return a < b;
}

// Crear el vector
std::vector<MiTipoDeDato> vector1;

// Añadir elementos al vector
vector1.push_back(1);
vector1.push_back(3);
vector1.push_back(5);
vector1.push_back(2);
vector1.push_back(4);

// Ordenar el vector utilizando la función de comparación
std::sort(vector1.begin(), vector1.end(), comparar);

// Imprimir el vector
for (MiTipoDeDato elemento : vector1) {
  std::cout << elemento << " ";
}

std::cout << std::endl;

// Crear una nueva función para comparar los elementos del vector
bool compararInversa(const MiTipoDeDato& a, const MiTipoDeDato& b) {
  return a > b;
}

// Ordenar el vector en orden inverso utilizando la nueva función de comparación
std::sort(vector1.begin(), vector1.end(), compararInversa);

// Imprimir el vector
for (MiTipoDeDato elemento : vector1) {
  std::cout << elemento << " ";
}

std::cout << std::endl;

// Eliminar los elementos duplicados del vector
vector1.erase(std::unique(vector1.begin(), vector1.end()), vector1.end());

// Imprimir el vector
for (MiTipoDeDato elemento : vector1) {
  std::cout << elemento << " ";
}

std::cout << std::endl;

// Buscar un elemento en el vector utilizando el algoritmo de búsqueda binaria
MiTipoDeDato elementoABuscar = 3;
if (std::binary_search(vector1.begin(), vector1.end(), elementoABuscar)) {
  std::cout << "El elemento " << elementoABuscar << " se encuentra en el vector." << std::endl;
} else {
  std::cout << "El elemento " << elementoABuscar << " no se encuentra en el vector." << std::endl;
}
```

Este código es un ejemplo de cómo utilizar algunas de las funciones de la biblioteca estándar de C++ para trabajar con vectores. El código crea un vector de números, lo ordena en orden ascendente y descendente, elimina los elementos duplicados y busca un elemento específico en el vector utilizando el algoritmo de búsqueda binaria.

El código está dividido en varias funciones, cada una de las cuales realiza una tarea específica. La función `comparar` se utiliza para comparar dos elementos del vector y determinar cuál es mayor. La función `compararInversa` se utiliza para comparar dos elementos del vector y determinar cuál es menor. La función `ordenar` se utiliza para ordenar el vector utilizando la función de comparación especificada. La función `unique` se utiliza para eliminar los elementos duplicados del vector. La función `binary_search` se utiliza para buscar un elemento específico en el vector utilizando el algoritmo de búsqueda binaria.

El código también utiliza algunas de las características de la programación genérica de C++, como el tipo de dato genérico `MiTipoDeDato` y la función `sort`. Esto permite que el código sea reutilizado para diferentes tipos de datos sin tener que modificarlo.