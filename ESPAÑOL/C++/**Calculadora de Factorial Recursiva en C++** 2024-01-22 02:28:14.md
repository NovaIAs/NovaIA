```c++
// Importación de la biblioteca estándar de entrada-salida de C++
#include <iostream>

// Definición de una función recursiva para calcular el factorial de un número natural
int factorial(int n) {
    // Caso base: si n es 0, el factorial es 1
    if (n == 0) {
        return 1;
    }
    // Caso recursivo: si n es mayor que 0, el factorial es n por el factorial de n-1
    else {
        return n * factorial(n-1);
    }
}

// Definición de la función principal del programa
int main() {
    // Declaración de variables
    int n;

    // Solicitar al usuario que introduzca un número natural
    std::cout << "Introduzca un número natural: ";
    std::cin >> n;

    // Calcular el factorial de n usando la función recursiva
    int factorial_n = factorial(n);

    // Mostrar el resultado en la consola
    std::cout << "El factorial de " << n << " es: " << factorial_n << std::endl;

    // Fin del programa
    return 0;
}
```

Explicación del código:

1. **Importación de la biblioteca estándar de entrada-salida de C++**:
   - `#include <iostream>`: Esta línea incluye la biblioteca estándar de C++ que proporciona funciones para la entrada y salida de datos.

2. **Definición de una función recursiva para calcular el factorial de un número natural**:
   - `int factorial(int n)`: Esta es la definición de la función recursiva `factorial()` que calcula el factorial de un número natural `n`.
   - **Caso base**: `if (n == 0) { return 1; }`: Esta es la condición de parada de la recursión. Si `n` es 0, el factorial es 1.
   - **Caso recursivo**: `return n * factorial(n-1);`: Si `n` es mayor que 0, el factorial es `n` multiplicado por el factorial de `n-1`.

3. **Definición de la función principal del programa**:
   - `int main()`: Esta es la función principal del programa. En C++, el programa se ejecuta desde la función `main()`.

4. **Declaración de variables**:
   - `int n;`: Esta es la variable que almacenará el número natural introducido por el usuario.

5. **Solicitar al usuario que introduzca un número natural**:
   - `std::cout << "Introduzca un número natural: ";`: Esta línea envía el mensaje "Introduzca un número natural: " a la consola, solicitando al usuario que introduzca un número.
   - `std::cin >> n;`: Esta línea lee el número introducido por el usuario y lo almacena en la variable `n`.

6. **Calcular el factorial de n usando la función recursiva**:
   - `int factorial_n = factorial(n);`: Esta línea llama a la función `factorial()` con el valor de `n` y almacena el resultado en la variable `factorial_n`.

7. **Mostrar el resultado en la consola**:
   - `std::cout << "El factorial de " << n << " es: " << factorial_n << std::endl;`: Esta línea envía el resultado del factorial de `n` a la consola, junto con un mensaje que indica el número del que se ha calculado el factorial.

8. **Fin del programa**:
   - `return 0;`: Esta línea indica el final del programa y devuelve un código de retorno de 0, que indica que el programa se ha ejecutado correctamente.