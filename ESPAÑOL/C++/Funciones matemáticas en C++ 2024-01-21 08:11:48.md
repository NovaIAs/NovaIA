```c++
// Importamos las bibliotecas necesarias.
#include <iostream>
#include <vector>
#include <algorithm>

// Creamos una función para encontrar el máximo común divisor de dos números.
int mcd(int a, int b) {
  while (b != 0) {
    int t = b;
    b = a % b;
    a = t;
  }
  return a;
}

// Creamos una función para encontrar el mínimo común múltiplo de dos números.
int mcm(int a, int b) {
  return (a * b) / mcd(a, b);
}

// Creamos una función para encontrar el producto de dos números.
int producto(int a, int b) {
  return a * b;
}

// Creamos una función para encontrar la suma de dos números.
int suma(int a, int b) {
  return a + b;
}

// Creamos una función para encontrar la resta de dos números.
int resta(int a, int b) {
  return a - b;
}

// Creamos una función para encontrar la división de dos números.
int division(int a, int b) {
  return a / b;
}

// Creamos una función para encontrar el resto de la división de dos números.
int resto(int a, int b) {
  return a % b;
}

// Creamos una función para encontrar el factorial de un número.
int factorial(int n) {
  if (n == 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
}

// Creamos una función para encontrar el número de combinaciones de n elementos tomados de k en k.
int combinaciones(int n, int k) {
  return factorial(n) / (factorial(k) * factorial(n - k));
}

// Creamos una función para encontrar el número de permutaciones de n elementos tomados de k en k.
int permutaciones(int n, int k) {
  return factorial(n) / factorial(n - k);
}

// Creamos una función para encontrar el número de variaciones de n elementos tomados de k en k.
int variaciones(int n, int k) {
  return factorial(n) / factorial(n - k);
}

// Creamos una función para encontrar el número de arreglos de n elementos tomados de k en k.
int arreglos(int n, int k) {
  return n * (n - 1) * (n - 2) * ... * (n - k + 1);
}

// Creamos una función para encontrar el número de subconjuntos de n elementos.
int subconjuntos(int n) {
  return (1 << n);
}

// Creamos una función para encontrar el número de particiones de n elementos.
int particiones(int n) {
  if (n == 0) {
    return 1;
  } else {
    int p = 0;
    for (int i = 1; i <= n - 1; i++) {
      p += particiones(i) * particiones(n - i);
    }
    return p;
  }
}

// Creamos una función para encontrar el número de soluciones de la ecuación de Diofanto x + y = n.
int diofanto(int n) {
  int s = 0;
  for (int i = 0; i <= n; i++) {
    s += combinaciones(n, i);
  }
  return s;
}

// Creamos una función para encontrar el número de soluciones de la ecuación de Pell x^2 - Dy^2 = 1, donde D es un número entero positivo.
int pell(int D) {
  int x = 1;
  int y = 0;
  int z = 1;
  int w = 1;
  int p = 0;
  int q = 1;

  while (true) {
    int x_new = x * z + D * y * w;
    int y_new = y * z + x * w;
    int z_new = 2 * z * x + D * 2 * y * w;
    int w_new = w * z + x * w;
    int q_new = p + q * D;
    int p_new = q;

    if (x_new == 1 && y_new == 0) {
      return p_new;
    }

    x = x_new;
    y = y_new;
    z = z_new;
    w = w_new;
    p = p_new;
    q = q_new;
  }
}

// Creamos una función para encontrar el número de soluciones de la ecuación de Fermat x^n + y^n = z^n, donde n es un número entero mayor que 2.
int fermat(int n) {
  int s = 0;
  for (int i = 0; i <= (n - 1) / 2; i++) {
    for (int j = 0; j <= (n - 1) / 2; j++) {
      for (int k = 0; k <= (n - 1) / 2; k++) {
        if (i + j + k == n - 1) {
          s++;
        }
      }
    }
  }
  return s;