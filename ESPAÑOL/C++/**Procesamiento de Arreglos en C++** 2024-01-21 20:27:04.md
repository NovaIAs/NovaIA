```c++
#include <bits/stdc++.h>
using namespace std;

// DEFINICIÓN DE CONSTANTES
const int MAX_N = 100000; // Tamaño máximo del arreglo
const int MOD = 1000000007; // Módulo para operaciones aritméticas

// DECLARACIÓN DE FUNCIONES
int suma_arreglo(int arr[], int n); // Función para sumar los elementos de un arreglo
int producto_arreglo(int arr[], int n); // Función para calcular el producto de los elementos de un arreglo
int maximo_arreglo(int arr[], int n); // Función para encontrar el elemento máximo de un arreglo
int minimo_arreglo(int arr[], int n); // Función para encontrar el elemento mínimo de un arreglo
int busqueda_binaria(int arr[], int n, int x); // Función para realizar una búsqueda binaria en un arreglo ordenado

// IMPLEMENTACIÓN DE FUNCIONES
int suma_arreglo(int arr[], int n) {
  int suma = 0;
  for (int i = 0; i < n; i++) {
    suma += arr[i];
  }
  return suma;
}

int producto_arreglo(int arr[], int n) {
  int producto = 1;
  for (int i = 0; i < n; i++) {
    producto *= arr[i];
  }
  return producto;
}

int maximo_arreglo(int arr[], int n) {
  int maximo = INT_MIN;
  for (int i = 0; i < n; i++) {
    if (arr[i] > maximo) {
      maximo = arr[i];
    }
  }
  return maximo;
}

int minimo_arreglo(int arr[], int n) {
  int minimo = INT_MAX;
  for (int i = 0; i < n; i++) {
    if (arr[i] < minimo) {
      minimo = arr[i];
    }
  }
  return minimo;
}

int busqueda_binaria(int arr[], int n, int x) {
  int izquierda = 0;
  int derecha = n - 1;
  while (izquierda <= derecha) {
    int mitad = (izquierda + derecha) / 2;
    if (arr[mitad] == x) {
      return mitad;
    } else if (arr[mitad] < x) {
      izquierda = mitad + 1;
    } else {
      derecha = mitad - 1;
    }
  }
  return -1;
}

// FUNCIÓN PRINCIPAL
int main() {
  // DECLARACIÓN DE VARIABLES
  int n; // Tamaño del arreglo
  cin >> n;

  int arr[MAX_N]; // Arreglo de enteros

  // LECTURA DE ELEMENTOS DEL ARREGLO
  for (int i = 0; i < n; i++) {
    cin >> arr[i];
  }

  // CALCULAR Y MOSTRAR LA SUMA DEL ARREGLO
  int suma = suma_arreglo(arr, n);
  cout << "Suma del arreglo: " << suma << endl;

  // CALCULAR Y MOSTRAR EL PRODUCTO DEL ARREGLO
  int producto = producto_arreglo(arr, n);
  cout << "Producto del arreglo: " << producto << endl;

  // ENCONTRAR Y MOSTRAR EL ELEMENTO MÁXIMO DEL ARREGLO
  int maximo = maximo_arreglo(arr, n);
  cout << "Elemento máximo del arreglo: " << maximo << endl;

  // ENCONTRAR Y MOSTRAR EL ELEMENTO MÍNIMO DEL ARREGLO
  int minimo = minimo_arreglo(arr, n);
  cout << "Elemento mínimo del arreglo: " << minimo << endl;

  // BUSCAR Y MOSTRAR EL ÍNDICE DE UN ELEMENTO EN EL ARREGLO
  int x; // Elemento a buscar
  cin >> x;
  int indice = busqueda_binaria(arr, n, x);
  if (indice == -1) {
    cout << "El elemento " << x << " no se encuentra en el arreglo." << endl;
  } else {
    cout << "El elemento " << x << " se encuentra en el índice " << indice << " del arreglo." << endl;
  }

  return 0;
}
```

Explicación del código:

1. El código comienza con la inclusión de cabeceras necesarias, como `bits/stdc++.h`, y el uso del espacio de nombres `std`.

2. Se definen varias constantes, como `MAX_N` y `MOD`, que se utilizan en el código.

3. Se declaran varias funciones, que se explican en detalle a continuación:

   - `suma_arreglo`: Esta función suma los elementos de un arreglo y devuelve la suma total.

   - `producto_arreglo`: Esta función calcula el producto de los elementos de un arreglo y devuelve el producto total.

   - `maximo_arreglo`: Esta función encuentra el elemento máximo de un arreglo y lo devuelve.

   - `minimo_arreglo`: Esta función encuentra el elemento mínimo de un arreglo y lo devuelve.

   - `busqueda_binaria`: Esta función realiza una búsqueda binaria en un arreglo ordenado para encontrar el índice de un elemento dado.

4. La función `main` es la función principal del programa.

5. Dentro de la función `main`, se declaran variables necesarias, como `n` (el tamaño del arreglo) y `arr` (el arreglo de enteros).

6. Se lee el valor de `n` y los elementos del arreglo `arr` de la entrada estándar.

7. Se calculan y muestran la suma, el producto, el elemento máximo, el elemento mínimo y el índice de un elemento buscado en el arreglo.

8. El programa termina devolviendo `0`.