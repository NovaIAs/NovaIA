```c++
// Programa para calcular el área de un triángulo rectángulo.

#include <iostream>
#include <math.h>

using namespace std;

int main() {
  // Declaración de variables.
  double base, altura, area;

  // Petición de datos al usuario.
  cout << "Introduce la base del triángulo: ";
  cin >> base;
  cout << "Introduce la altura del triángulo: ";
  cin >> altura;

  // Cálculo del área del triángulo.
  area = (base * altura) / 2;

  // Muestra el resultado por pantalla.
  cout << "El área del triángulo es: " << area << " unidades cuadradas." << endl;

  return 0;
}
```

Explicación del código:

1. **Inclusión de bibliotecas:**
   ```c++
   #include <iostream>
   #include <math.h>
   ```

   Incluir estas bibliotecas permite utilizar funciones estándar de entrada y salida (iostream) y funciones matemáticas (math.h).


2. **Definición del espacio de nombres:**
   ```c++
   using namespace std;
   ```

   Esta línea indica al compilador que se van a utilizar las funciones y objetos definidos en el espacio de nombres estándar (`std`) sin tener que especificar `std::` delante de ellos.


3. **Declaración de variables:**
   ```c++
   double base, altura, area;
   ```

   Se declaran tres variables de tipo `double` para almacenar la base, la altura y el área del triángulo.


4. **Petición de datos al usuario:**
   ```c++
   cout << "Introduce la base del triángulo: ";
   cin >> base;
   cout << "Introduce la altura del triángulo: ";
   cin >> altura;
   ```

   Estas líneas muestran mensajes en la consola solicitando al usuario que introduzca la base y la altura del triángulo. A continuación, se utilizan los objetos `cin` y `cout` para leer los valores introducidos por el usuario y almacenarlos en las variables `base` y `altura`.


5. **Cálculo del área del triángulo:**
   ```c++
   area = (base * altura) / 2;
   ```

   Se calcula el área del triángulo utilizando la fórmula `área = (base * altura) / 2`.


6. **Muestra el resultado por pantalla:**
   ```c++
   cout << "El área del triángulo es: " << area << " unidades cuadradas." << endl;
   ```

   Esta línea muestra un mensaje en la consola con el resultado del área del triángulo.