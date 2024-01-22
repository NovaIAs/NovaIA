```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>

using namespace std;

// Una función para generar un vector de números aleatorios
vector<int> generarVectorAleatorio(int tamaño, int min, int max) {
  vector<int> v(tamaño);
  for (int i = 0; i < tamaño; i++) {
    v[i] = min + rand() % (max - min + 1);
  }
  return v;
}

// Una función para calcular el promedio de un vector de números
double calcularPromedio(const vector<int>& v) {
  return accumulate(v.begin(), v.end(), 0.0) / v.size();
}

// Una función para calcular la mediana de un vector de números
double calcularMediana(const vector<int>& v) {
  vector<int> v_copia = v;
  sort(v_copia.begin(), v_copia.end());
  int mitad = v_copia.size() / 2;
  if (v_copia.size() % 2 == 0) {
    return (v_copia[mitad] + v_copia[mitad - 1]) / 2.0;
  } else {
    return v_copia[mitad];
  }
}

// Una función para calcular la moda de un vector de números
int calcularModa(const vector<int>& v) {
  unordered_map<int, int> mapa_de_frecuencias;
  for (int i = 0; i < v.size(); i++) {
    mapa_de_frecuencias[v[i]]++;
  }
  int moda = -1;
  int max_frecuencia = 0;
  for (auto& par : mapa_de_frecuencias) {
    if (par.second > max_frecuencia) {
      moda = par.first;
      max_frecuencia = par.second;
    }
  }
  return moda;
}

// Una función para calcular el rango de un vector de números
int calcularRango(const vector<int>& v) {
  return *max_element(v.begin(), v.end()) - *min_element(v.begin(), v.end());
}

// Una función para calcular la desviación estándar de un vector de números
double calcularDesviaciónEstándar(const vector<int>& v) {
  double promedio = calcularPromedio(v);
  double suma_de_las_diferencias_al_cuadrado = 0;
  for (int i = 0; i < v.size(); i++) {
    suma_de_las_diferencias_al_cuadrado += pow(v[i] - promedio, 2);
  }
  return sqrt(suma_de_las_diferencias_al_cuadrado / (v.size() - 1));
}

// Una función para calcular los cuartiles de un vector de números
vector<int> calcularCuartiles(const vector<int>& v) {
  vector<int> v_copia = v;
  sort(v_copia.begin(), v_copia.end());
  int cuartil_1 = v_copia[v_copia.size() / 4];
  int cuartil_2 = v_copia[v_copia.size() / 2];
  int cuartil_3 = v_copia[3 * v_copia.size() / 4];
  return {cuartil_1, cuartil_2, cuartil_3};
}

int main() {
  // Generar un vector de 100 números aleatorios entre 1 y 100
  vector<int> v = generarVectorAleatorio(100, 1, 100);

  // Calcular el promedio, la mediana, la moda, el rango, la desviación estándar y los cuartiles del vector
  double promedio = calcularPromedio(v);
  double mediana = calcularMediana(v);
  int moda = calcularModa(v);
  int rango = calcularRango(v);
  double desviación_estándar = calcularDesviaciónEstándar(v);
  vector<int> cuartiles = calcularCuartiles(v);

  // Mostrar los resultados en la consola
  cout << "Promedio: " << promedio << endl;
  cout << "Mediana: " << mediana << endl;
  cout << "Moda: " << moda << endl;
  cout << "Rango: " << rango << endl;
  cout << "Desviación estándar: " << desviación_estándar << endl;
  cout << "Cuartiles: " << cuartiles[0] << ", " << cuartiles[1] << ", " << cuartiles[2] << endl;

  return 0;
}
```

Este código es una implementación en C++ de un programa que calcula varias estadísticas de un vector de números aleatorios. El programa primero genera un vector de 100 números aleatorios entre 1 y 100, y luego calcula el promedio, la mediana, la moda, el rango, la desviación estándar y los cuartiles del vector. Los resultados se muestran en la consola.

El código utiliza varias funciones para calcular las estadísticas. La función `calcularPromedio()` calcula el promedio de un vector de números, la función `calcularMediana()` calcula la mediana de un vector de números, la función `calcularModa()` calcula la moda de un vector de números, la función `calcularRango()` calcula el rango de un vector de números, la función `calcularDesviaciónEstándar()` calcula la desviación estándar de un vector de números y la función `calcularCuartiles()` calcula los cuartiles de un vector de números.

El programa utiliza la biblioteca estándar de C++ para generar números aleatorios, ordenar vectores y calcular estadísticas.