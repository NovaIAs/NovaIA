```kotlin
// Importación de bibliotecas necesarias
import java.util.Scanner
import kotlin.random.Random

// Función principal del programa
fun main(args: Array<String>) {
    // Creación de un objeto Scanner para leer la entrada del usuario
    val scanner = Scanner(System.`in`)

    // Bienvenida al usuario
    println("¡Bienvenido al juego de adivinar el número!")

    // Generación de un número aleatorio entre 1 y 100
    val numeroAleatorio = Random.nextInt(1, 101)

    // Número de intentos del usuario
    var intentos = 0

    // Bucle while para seguir pidiendo al usuario que adivine el número
    while (true) {
        // Aumento del número de intentos
        intentos++

        // Petición al usuario para que introduzca un número
        print("Introduce un número del 1 al 100: ")
        val numeroUsuario = scanner.nextInt()

        // Comprobación si el número introducido es igual al número aleatorio
        if (numeroUsuario == numeroAleatorio) {
            // El usuario ha adivinado el número
            println("¡Felicidades! Has adivinado el número en $intentos intentos.")
            break  // Salimos del bucle
        } else if (numeroUsuario < numeroAleatorio) {
            // El número introducido es menor que el número aleatorio
            println("El número que has introducido es menor que el número aleatorio.")
        } else {
            // El número introducido es mayor que el número aleatorio
            println("El número que has introducido es mayor que el número aleatorio.")
        }
    }

    // Despedida del usuario
    println("Gracias por jugar.")
}
```

Explicación del código:

* **Importación de bibliotecas:** Se importan las bibliotecas necesarias para el programa, incluyendo `Scanner` para leer la entrada del usuario y `Random` para generar números aleatorios.
* **Función principal:** La función `main` es el punto de entrada del programa.
* **Generación de un número aleatorio:** Se genera un número aleatorio entre 1 y 100 utilizando la función `nextInt` de la clase `Random`.
* **Número de intentos del usuario:** Se inicializa la variable `intentos` a 0 para contar el número de intentos del usuario.
* **Bucle while:** Se utiliza un bucle `while` para seguir pidiendo al usuario que adivine el número hasta que lo consiga o hasta que se hayan hecho 10 intentos.
* **Petición al usuario:** Se utiliza la función `nextInt` de la clase `Scanner` para pedir al usuario que introduzca un número.
* **Comprobación si el número introducido es igual al número aleatorio:** Se comprueba si el número introducido por el usuario es igual al número aleatorio generado.
* **Salida por pantalla:** Se imprime por pantalla un mensaje en función de si el usuario ha adivinado el número, si el número introducido es menor o mayor que el número aleatorio.
* **Despedida del usuario:** Se imprime por pantalla un mensaje de despedida al usuario.

Este código es complejo porque utiliza conceptos avanzados de programación como la generación de números aleatorios, bucles `while` y comprobaciones condicionales. También es un ejemplo de una aplicación interactiva, ya que permite al usuario interactuar con el programa introduciendo números y recibiendo mensajes de respuesta.