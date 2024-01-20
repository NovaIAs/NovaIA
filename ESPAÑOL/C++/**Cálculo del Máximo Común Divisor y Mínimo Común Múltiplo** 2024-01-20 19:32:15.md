```c++
#include <iostream>
#include <cmath>
#include <vector>
#include <algorithm>

using namespace std;

// Función para encontrar el máximo común divisor (MCD) de dos números
int mcd(int a, int b) {
  return (b == 0) ? a : mcd(b, a % b);
}

// Función para encontrar el mínimo común múltiplo (MCM) de dos números
int mcm(int a, int b) {
  return (a * b) / mcd(a, b);
}

// Función para generar una lista de los divisores de un número
vector<int> divisores(int n) {
  vector<int> res;
  for (int i = 1; i <= sqrt(n); i++) {
    if (n % i == 0) {
      res.push_back(i);
      if (i * i != n) {
        res.push_back(n / i);
      }
    }
  }
  return res;
}

// Función para encontrar el máximo común divisor (MCD) de una lista de números
int mcd_lista(vector<int> nums) {
  int res = nums[0];
  for (int i = 1; i < nums.size(); i++) {
    res = mcd(res, nums[i]);
  }
  return res;
}

// Función para encontrar el mínimo común múltiplo (MCM) de una lista de números
int mcm_lista(vector<int> nums) {
  int res = nums[0];
  for (int i = 1; i < nums.size(); i++) {
    res = mcm(res, nums[i]);
  }
  return res;
}


// Función principal
int main() {
  // Demostrar el uso de la función mcd
  int a = 12, b = 18;
  cout << "MCD de " << a << " y " << b << " es: " << mcd(a, b) << endl;

  // Demostrar el uso de la función mcm
  cout << "MCM de " << a << " y " << b << " es: " << mcm(a, b) << endl;

  // Demostrar el uso de la función divisores
  int c = 24;
  cout << "Divisores de " << c << ": ";
  vector<int> divs = divisores(c);
  for (int div : divs) {
    cout << div << " ";
  }
  cout << endl;

  // Demostrar el uso de la función mcd_lista
  vector<int> nums1 = {12, 18, 24};
  cout << "MCD de la lista {12, 18, 24} es: " << mcd_lista(nums1) << endl;

  // Demostrar el uso de la función mcm_lista
  vector<int> nums2 = {12, 18, 24};
  cout << "MCM de la lista {12, 18, 24} es: " << mcm_lista(nums2) << endl;

  return 0;
}
```

Explicación del código:

1. Se incluyen las bibliotecas necesarias para el programa.

2. Se definen las funciones `mcd`, `mcm`, `divisores`, `mcd_lista` y `mcm_lista`.

3. La función `mcd` calcula el máximo común divisor de dos números utilizando el algoritmo de Euclides.

4. La función `mcm` calcula el mínimo común múltiplo de dos números utilizando la fórmula `mcm(a, b) = (a * b) / mcd(a, b)`.

5. La función `divisores` genera una lista de todos los divisores de un número dado.

6. La función `mcd_lista` calcula el máximo común divisor de una lista de números utilizando la función `mcd`.

7. La función `mcm_lista` calcula el mínimo común múltiplo de una lista de números utilizando la función `mcm`.

8. En la función `main`, se demuestran las funciones definidas anteriormente.

9. Se imprime la salida del programa.