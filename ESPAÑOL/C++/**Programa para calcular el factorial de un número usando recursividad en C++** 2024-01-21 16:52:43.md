```c++
// Programa que calcula el factorial de un número usando recursividad

#include <iostream>

using namespace std;

// Función factorial recursiva
int factorial(int numero) {
  if (numero == 0) {
    return 1;
  } else {
    return numero * factorial(numero - 1);
  }
}

// Función principal
int main() {
  // Pedir al usuario que ingrese un número
  cout << "Ingrese un número para calcular su factorial: ";
  int numero;
  cin >> numero;

  // Calcular el factorial del número
  int factorial_resultado = factorial(numero);

  // Mostrar el resultado
  cout << "El factorial de " << numero << " es: " << factorial_resultado << endl;

  return 0;
}
```

Explicación:

* El programa comienza incluyendo la librería `iostream` que contiene las funciones de entrada y salida estándar.

* La función `factorial` es una función recursiva que calcula el factorial de un número.

* La función `main` es la función principal del programa.

* En la función `main`, se pide al usuario que ingrese un número.

* Se llama a la función `factorial` para calcular el factorial del número.

* Se muestra el resultado.