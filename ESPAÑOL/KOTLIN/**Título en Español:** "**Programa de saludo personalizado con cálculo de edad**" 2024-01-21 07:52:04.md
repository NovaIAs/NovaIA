```kotlin
// **Importaciones:** Librerías y clases necesarias para el código.

import java.util.Scanner
import java.time.LocalDate
import java.time.format.DateTimeFormatter

// **Clase principal:** Contiene la lógica principal del programa.

class Principal {

    // **Función principal:** Punto de entrada del programa.

    fun main(args: Array<String>) {

        // **Variables:** Declaración de variables utilizadas en el programa.

        // Crear un objeto Scanner para leer la entrada de la consola.
        val scanner = Scanner(System.`in`)

        // Crear un objeto LocalDate para representar la fecha actual.
        val hoy = LocalDate.now()

        // **Entrada de datos:** Solicitamos datos al usuario.

        // Solicitar el nombre del usuario.
        print("Ingresa tu nombre: ")
        val nombre = scanner.nextLine()

        // Solicitar la fecha de nacimiento del usuario.
        print("Ingresa tu fecha de nacimiento (dd/mm/yyyy): ")
        val fechaNacimiento = LocalDate.parse(scanner.nextLine(), DateTimeFormatter.ofPattern("dd/MM/yyyy"))

        // **Procesamiento de datos:** Calculamos la edad del usuario y saludamos.

        // Calcular la edad del usuario.
        val edad = hoy.year - fechaNacimiento.year

        // **Salida de datos:** Mostramos los resultados al usuario.

        // Saludar al usuario.
        println("Hola, $nombre. ¡Bienvenido al programa!")

        // Informar la edad del usuario.
        println("Tienes $edad años.")

        //**Funciones adicionales:** Otro tipo de forma de representar este código.

        // **Función para calcular la edad:** Calcula la edad de una persona a partir de su fecha de nacimiento.

        fun calcularEdad(fechaNacimiento: LocalDate): Int {
            return hoy.year - fechaNacimiento.year
        }

        // **Función para saludar:** Saluda a una persona por su nombre.

        fun saludar(nombre: String) {
            println("Hola, $nombre. ¡Bienvenido al programa!")
        }

        // **Uso de las funciones adicionales:** Se muestran resultados de las funciones.

        // Calcular y mostrar la edad del usuario usando la función.
        val edadCalculada = calcularEdad(fechaNacimiento)
        println("Tu edad es $edadCalculada años.")

        // Saludar al usuario usando la función.
        saludar(nombre)
    }
}

```

**Explicación del código:**

1. **Importaciones:** Importamos las bibliotecas y clases necesarias para el programa.

2. **Clase principal:** Definimos la clase `Principal` que contiene la lógica principal del programa.

3. **Función principal:** Definimos la función `main` que es el punto de entrada del programa.

4. **Variables:** Declaramos las variables necesarias para el programa, incluyendo:
   * `scanner`: Objeto `Scanner` para leer la entrada de la consola.
   * `hoy`: Objeto `LocalDate` para representar la fecha actual.

5. **Entrada de datos:** Solicitamos al usuario que introduzca su nombre y fecha de nacimiento.

6. **Procesamiento de datos:**
   * Calculamos la edad del usuario utilizando la fecha actual y la fecha de nacimiento proporcionada.

7. **Salida de datos:**
   * Saludamos al usuario por su nombre.
   * Informamos la edad del usuario.

8. **Funciones adicionales:** Definimos funciones adicionales para calcular la edad de una persona y saludarla por su nombre.

9. **Uso de las funciones adicionales:**
   * Calculamos la edad del usuario utilizando la función `calcularEdad`.
   * Saludamos al usuario utilizando la función `saludar`.

10. **Ejecución del programa:** Al ejecutar el programa, se solicita al usuario que introduzca su nombre y fecha de nacimiento. A continuación, se calcula y muestra la edad del usuario y se le saluda por su nombre.