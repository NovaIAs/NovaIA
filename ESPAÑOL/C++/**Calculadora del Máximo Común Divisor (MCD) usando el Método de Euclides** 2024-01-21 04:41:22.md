```cpp
// Programa que calcula el máximo común divisor (MCD) de dos números usando el método de Euclides.

#include <iostream>

using namespace std;

// Función que calcula el MCD de dos números.
int mcd(int a, int b) {
  if (b == 0) {
    return a;
  } else {
    return mcd(b, a % b);
  }
}

// Función principal.
int main() {
  // Pedimos al usuario que introduzca dos números.
  cout << "Introduzca dos números: ";
  int a, b;
  cin >> a >> b;

  // Calculamos el MCD de los dos números.
  int mcd_ab = mcd(a, b);

  // Mostramos el resultado.
  cout << "El MCD de " << a << " y " << b << " es " << mcd_ab << endl;

  return 0;
}
```

Explicación del código:

* La función `mcd()` calcula el MCD de dos números utilizando el método de Euclides. El método de Euclides es un algoritmo recursivo que funciona de la siguiente manera:

  * Si uno de los números es 0, el MCD es el otro número.
  * De lo contrario, el MCD es el MCD del segundo número y el resto de la división del primer número entre el segundo.

* La función `main()` es la función principal del programa. En esta función:

  * Pedimos al usuario que introduzca dos números.
  * Calculamos el MCD de los dos números utilizando la función `mcd()`.
  * Mostramos el resultado.