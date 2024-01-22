```kotlin
// Importamos las clases necesarias para el programa.
import java.util.*

// Definimos una clase llamada "MiPrimeraClase" que contiene el código principal.
class MiPrimeraClase {

    // Definimos un método llamado "main" que será ejecutado cuando se ejecute el programa.
    fun main(args: Array<String>) {

        // Creamos un objeto de la clase "Scanner" para leer datos del teclado.
        val scanner = Scanner(System.`in`)

        // Imprimimos un mensaje en la consola pidiendo al usuario que introduzca un número.
        println("Introduce un número:")

        // Leemos el número introducido por el usuario utilizando el método "nextInt()".
        val numero = scanner.nextInt()

        // Comprobamos si el número introducido es mayor que 0.
        if (numero > 0) {

            // Si el número es mayor que 0, imprimimos un mensaje en la consola diciendo que es un número positivo.
            println("El número $numero es un número positivo.")

        } else if (numero == 0) {

            // Si el número es igual a 0, imprimimos un mensaje en la consola diciendo que es un número neutro.
            println("El número $numero es un número neutro.")

        } else {

            // Si el número es menor que 0, imprimimos un mensaje en la consola diciendo que es un número negativo.
            println("El número $numero es un número negativo.")

        }

    }

}
```

**Explicación del código:**

* El código comienza importando las clases necesarias para el programa. En este caso, importamos la clase "Scanner" de la biblioteca "java.util" para poder leer datos del teclado.
* A continuación, definimos una clase llamada "MiPrimeraClase" que contiene el código principal.
* Dentro de la clase "MiPrimeraClase", definimos un método llamado "main" que será ejecutado cuando se ejecute el programa.
* En el método "main", creamos un objeto de la clase "Scanner" para leer datos del teclado y luego imprimimos un mensaje en la consola pidiendo al usuario que introduzca un número.
* Después, leemos el número introducido por el usuario utilizando el método "nextInt()".
* Finalmente, comprobamos si el número introducido es mayor que 0, igual a 0 o menor que 0 y, según el caso, imprimimos un mensaje en la consola diciendo que el número es positivo, neutro o negativo.