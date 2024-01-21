```kotlin
// Importación de las librerías necesarias
import java.util.Scanner
import java.lang.Math

// Declaración de la clase principal
class Main {
    // Método principal
    fun main(args: Array<String>) {
        // Creación de un objeto Scanner para leer la entrada del usuario
        val scanner = Scanner(System.`in`)

        // Solicitud del radio de la esfera al usuario
        print("Ingrese el radio de la esfera en centímetros: ")
        val radio = scanner.nextDouble()

        // Solicitud del tipo de material de la esfera al usuario
        print("Ingrese el tipo de material de la esfera (piedra, metal, madera): ")
        val material = scanner.next()

        // Cálculo del volumen de la esfera según el material
        val volumen = when (material) {
            "piedra" -> (4 / 3) * Math.PI * Math.pow(radio, 3.0) * 2.7
            "metal" -> (4 / 3) * Math.PI * Math.pow(radio, 3.0) * 7.8
            "madera" -> (4 / 3) * Math.PI * Math.pow(radio, 3.0) * 0.6
            else -> 0.0
        }

        // Impresión del volumen de la esfera
        println("El volumen de la esfera es $volumen centímetros cúbicos.")
    }
}
```

Explicación del código:

1. **Importación de las librerías necesarias**:

    * `java.util.Scanner`: Se importa la librería Scanner para poder leer la entrada del usuario.
    * `java.lang.Math`: Se importa la librería Math para utilizar funciones matemáticas como `Math.PI` y `Math.pow()`.

2. **Declaración de la clase principal `Main`**:

    * Se define la clase `Main` que contiene el método `main()` que será el punto de entrada del programa.

3. **Método principal `main()`**:

    * Se crea un objeto `Scanner` llamado `scanner` para leer la entrada del usuario.
    * Se solicita al usuario que ingrese el radio de la esfera en centímetros y se almacena en la variable `radio`.
    * Se solicita al usuario que ingrese el tipo de material de la esfera (piedra, metal o madera) y se almacena en la variable `material`.
    * Se calcula el volumen de la esfera según el material utilizando la fórmula `(4 / 3) * Math.PI * Math.pow(radio, 3.0) * densidad` donde `densidad` varía según el material.
    * Se imprime el volumen de la esfera en centímetros cúbicos.

4. **Uso de la sentencia `when`**:

    * La sentencia `when` se utiliza para calcular el volumen de la esfera según el material.
    * Se especifican los casos para cada tipo de material y se devuelve el volumen correspondiente.
    * Si el material ingresado por el usuario no es uno de los especificados, se devuelve un valor de 0.0.

5. **Uso de la función `Math.pow()`**:

    * La función `Math.pow()` se utiliza para elevar un número a una potencia.
    * En este caso, se utiliza para calcular el cubo del radio de la esfera (`Math.pow(radio, 3.0)`).

6. **Uso de la constante `Math.PI`**:

    * La constante `Math.PI` se utiliza para obtener el valor de π.
    * En este caso, se utiliza para calcular el volumen de la esfera.