```c++
// Programa que calcula la suma de los números enteros desde 1 hasta n ingresado por el usuario.

// Incluye la biblioteca de entrada y salida estándar.
#include <iostream>

// Utiliza el espacio de nombres estándar.
using namespace std;

// Define la función principal del programa.
int main() {
  // Declara una variable entera para almacenar la suma.
  int suma = 0;

  // Pide al usuario que ingrese un número entero.
  cout << "Ingrese un número entero positivo: ";

  // Declara una variable entera para almacenar el número ingresado por el usuario.
  int n;

  // Lee el número ingresado por el usuario.
  cin >> n;

  // Verifica si el número ingresado es positivo.
  if (n > 0) {
    // Recorre los números desde 1 hasta n.
    for (int i = 1; i <= n; i++) {
      // Agrega el número actual a la suma.
      suma += i;
    }

    // Imprime la suma de los números desde 1 hasta n.
    cout << "La suma de los números desde 1 hasta " << n << " es: " << suma << endl;
  } else {
    // Si el número ingresado no es positivo, imprime un mensaje de error.
    cout << "El número ingresado no es positivo." << endl;
  }

  // Retorna 0 para indicar que el programa se ejecutó correctamente.
  return 0;
}
```

Explicación del código:

* La función `main` es la función principal del programa. Es donde se ejecuta el código principal.
* La variable `suma` se utiliza para almacenar la suma de los números desde 1 hasta n.
* El código `cout << "Ingrese un número entero positivo: ";` imprime un mensaje en la consola pidiendo al usuario que ingrese un número entero positivo.
* La variable `n` se utiliza para almacenar el número ingresado por el usuario.
* El código `cin >> n;` lee el número ingresado por el usuario y lo almacena en la variable `n`.
* El código `if (n > 0)` verifica si el número ingresado es positivo.
* El código `for (int i = 1; i <= n; i++)` recorre los números desde 1 hasta n.
* El código `suma += i;` agrega el número actual a la suma.
* El código `cout << "La suma de los números desde 1 hasta " << n << " es: " << suma << endl;` imprime la suma de los números desde 1 hasta n en la consola.
* El código `return 0;` indica que el programa se ejecutó correctamente.