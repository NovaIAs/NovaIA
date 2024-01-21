```c++
// Biblioteca estándar de C++
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>

// Definición de constantes
const int MAX_ELEMENTOS = 100;
const std::string MENSAJE_BIENVENIDA = "¡Bienvenido al programa!";
const std::string MENSAJE_DESPEDIDA = "¡Gracias por usar el programa!";

// Definición de tipos de datos
typedef std::vector<int> VectorEnteros;
typedef std::map<std::string, int> MapaCadenasEnteros;

// Función principal del programa
int main() {
  // Mostrar mensaje de bienvenida
  std::cout << MENSAJE_BIENVENIDA << std::endl;

  // Declaración de variables
  VectorEnteros vectorEnteros;
  MapaCadenasEnteros mapaCadenasEnteros;
  int opcion;
  std::string cadena;
  int entero;

  // Bucle principal del programa
  do {
    // Mostrar menú de opciones
    std::cout << std::endl;
    std::cout << "Elija una opción:" << std::endl;
    std::cout << "1. Añadir un entero al vector" << std::endl;
    std::cout << "2. Añadir un par cadena-entero al mapa" << std::endl;
    std::cout << "3. Mostrar el vector de enteros" << std::endl;
    std::cout << "4. Mostrar el mapa de cadenas-enteros" << std::endl;
    std::cout << "5. Salir del programa" << std::endl;

    // Leer la opción elegida por el usuario
    std::cin >> opcion;

    // Procesar la opción elegida
    switch (opcion) {
      case 1:
        // Añadir un entero al vector
        std::cout << "Introduzca un entero: ";
        std::cin >> entero;
        vectorEnteros.push_back(entero);
        break;
      case 2:
        // Añadir un par cadena-entero al mapa
        std::cout << "Introduzca una cadena: ";
        std::cin >> cadena;
        std::cout << "Introduzca un entero: ";
        std::cin >> entero;
        mapaCadenasEnteros[cadena] = entero;
        break;
      case 3:
        // Mostrar el vector de enteros
        std::cout << "El vector de enteros es: ";
        for (int i = 0; i < vectorEnteros.size(); i++) {
          std::cout << vectorEnteros[i] << " ";
        }
        std::cout << std::endl;
        break;
      case 4:
        // Mostrar el mapa de cadenas-enteros
        std::cout << "El mapa de cadenas-enteros es: ";
        for (auto it = mapaCadenasEnteros.begin(); it != mapaCadenasEnteros.end(); it++) {
          std::cout << it->first << " -> " << it->second << std::endl;
        }
        break;
      case 5:
        // Salir del programa
        std::cout << MENSAJE_DESPEDIDA << std::endl;
        break;
      default:
        // Opción no válida
        std::cout << "Opción no válida" << std::endl;
    }
  } while (opcion != 5);

  // Retornar 0 para indicar que el programa ha finalizado correctamente
  return 0;
}
```

Este código es un programa en C++ que permite al usuario añadir enteros a un vector y pares cadena-entero a un mapa. El programa también permite mostrar el vector de enteros y el mapa de cadenas-enteros.

El programa comienza mostrando un mensaje de bienvenida al usuario. A continuación, declara las variables que se utilizarán en el programa, incluyendo un vector de enteros, un mapa de cadenas-enteros, una variable de tipo entero para almacenar la opción elegida por el usuario, una variable de tipo cadena para almacenar la cadena introducida por el usuario y una variable de tipo entero para almacenar el entero introducido por el usuario.

El programa entonces entra en un bucle principal, donde muestra un menú de opciones al usuario. El usuario puede elegir entre añadir un entero al vector, añadir un par cadena-entero al mapa, mostrar el vector de enteros, mostrar el mapa de cadenas-enteros o salir del programa.

Si el usuario elige añadir un entero al vector, el programa le pide que introduzca un entero. El programa entonces añade el entero al vector.

Si el usuario elige añadir un par cadena-entero al mapa, el programa le pide que introduzca una cadena y un entero. El programa entonces añade el par cadena-entero al mapa.

Si el usuario elige mostrar el vector de enteros, el programa muestra el vector de enteros en la consola.

Si el usuario elige mostrar el mapa de cadenas-enteros, el programa muestra el mapa de cadenas-enteros en la consola.

Si el usuario elige salir del programa, el programa muestra un mensaje de despedida al usuario y sale del programa.