```dart
// Definir una lista de números
List<int> números = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

// Definir una función para calcular la suma de una lista de números
int suma(List<int> números) {
  int suma = 0;
  for (int número in números) {
    suma += número;
  }
  return suma;
}

// Definir una función para calcular el promedio de una lista de números
double promedio(List<int> números) {
  return suma(números) / números.length;
}

// Definir una función para encontrar el número máximo de una lista de números
int máximo(List<int> números) {
  int max = números[0];
  for (int número in números) {
    if (número > max) {
      max = número;
    }
  }
  return max;
}

// Definir una función para encontrar el número mínimo de una lista de números
int mínimo(List<int> números) {
  int min = números[0];
  for (int número in números) {
    if (número < min) {
      min = número;
    }
  }
  return min;
}

// Definir una función para invertir una lista de números
List<int> invertir(List<int> números) {
  List<int> invertida = [];
  for (int i = números.length - 1; i >= 0; i--) {
    invertida.add(números[i]);
  }
  return invertida;
}

// Definir una función para generar una lista de números pares
List<int> pares(int limite) {
  List<int> pares = [];
  for (int i = 0; i <= limite; i++) {
    if (i % 2 == 0) {
      pares.add(i);
    }
  }
  return pares;
}

// Definir una función para generar una lista de números impares
List<int> impares(int limite) {
  List<int> impares = [];
  for (int i = 0; i <= limite; i++) {
    if (i % 2 != 0) {
      impares.add(i);
    }
  }
  return impares;
}

// Definir una función para generar una lista de números primos
List<int> primos(int limite) {
  List<int> primos = [];
  for (int i = 2; i <= limite; i++) {
    bool esPrimo = true;
    for (int j = 2; j < i; j++) {
      if (i % j == 0) {
        esPrimo = false;
        break;
      }
    }
    if (esPrimo) {
      primos.add(i);
    }
  }
  return primos;
}

// Definir una función para generar una lista de números perfectos
List<int> perfectos(int limite) {
  List<int> perfectos = [];
  for (int i = 1; i <= limite; i++) {
    int sumaDivisores = 0;
    for (int j = 1; j < i; j++) {
      if (i % j == 0) {
        sumaDivisores += j;
      }
    }
    if (sumaDivisores == i) {
      perfectos.add(i);
    }
  }
  return perfectos;
}

// Imprimir los resultados de las funciones
print("Suma de la lista: ${suma(números)}");
print("Promedio de la lista: ${promedio(números)}");
print("Número máximo de la lista: ${máximo(números)}");
print("Número mínimo de la lista: ${mínimo(números)}");
print("Lista invertida: ${invertir(números)}");
print("Lista de números pares: ${pares(10)}");
print("Lista de números impares: ${impares(10)}");
print("Lista de números primos: ${primos(100)}");
print("Lista de números perfectos: ${perfectos(100)}");
```

Explicación del código:

1. **Definir una lista de números:** Se define una lista de números enteros llamada `números` que contiene los números del 1 al 10.


2. **Definir funciones para calcular la suma, el promedio, el número máximo y el número mínimo de una lista de números:** Se definen cuatro funciones, `suma`, `promedio`, `máximo` y `mínimo`, que calculan la suma, el promedio, el número máximo y el número mínimo de una lista de números, respectivamente.


3. **Definir una función para invertir una lista de números:** Se define una función llamada `invertir` que invierte el orden de los elementos de una lista de números.


4. **Definir funciones para generar listas de números pares, impares, primos y perfectos:** Se definen cuatro funciones, `pares`, `impares`, `primos` y `perfectos`, que generan listas de números pares, impares, primos y perfectos, respectivamente.


5. **Imprimir los resultados de las funciones:** Se imprimen los resultados de las funciones en la consola.