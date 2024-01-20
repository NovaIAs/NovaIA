```kotlin
// Importamos las librerías y clases necesarias.
import kotlin.math.sqrt
import java.util.Scanner

// Definimos la clase `Polinomio` que representará un polinomio.
class Polinomio {
    // Atributos.
    private val coeficientes: DoubleArray
    private val grado: Int

    // Constructor.
    constructor(coeficientes: DoubleArray) {
        this.coeficientes = coeficientes
        this.grado = coeficientes.size - 1
    }

    // Métodos.

    // Método para evaluar el polinomio en un punto dado.
    fun evaluar(x: Double): Double {
        var resultado = 0.0
        for (i in 0..grado) {
            resultado += coeficientes[i] * Math.pow(x, i.toDouble())
        }
        return resultado
    }

    // Método para calcular las raíces del polinomio.
    fun raices(): DoubleArray {
        // Si el grado del polinomio es 0, entonces no tiene raíces.
        if (grado == 0) {
            return doubleArrayOf()
        }

        // Si el grado del polinomio es 1, entonces tiene una raíz.
        if (grado == 1) {
            return doubleArrayOf(-coeficientes[0] / coeficientes[1])
        }

        // Si el grado del polinomio es 2, entonces tiene dos raíces.
        if (grado == 2) {
            val discriminante = coeficientes[1] * coeficientes[1] - 4 * coeficientes[0] * coeficientes[2]
            if (discriminante < 0) {
                return doubleArrayOf()
            }
            val raiz1 = (-coeficientes[1] + Math.sqrt(discriminante)) / (2 * coeficientes[2])
            val raiz2 = (-coeficientes[1] - Math.sqrt(discriminante)) / (2 * coeficientes[2])
            return doubleArrayOf(raiz1, raiz2)
        }

        // Si el grado del polinomio es mayor que 2, no tiene una fórmula general para calcular las raíces.
        // En este caso, se debe utilizar un método numérico para aproximar las raíces.
        return doubleArrayOf()
    }

    // Método para imprimir el polinomio en formato estándar.
    fun imprimir() {
        for (i in grado downTo 0) {
            if (i == grado) {
                print("${coeficientes[i]}x^$i")
            } else if (i == 0) {
                print(" + ${coeficientes[i]}")
            } else {
                print(" + ${coeficientes[i]}x^$i")
            }
        }
        println()
    }
}

// Creamos una función para pedir al usuario que introduzca los coeficientes del polinomio.
fun pedirCoeficientes(): DoubleArray {
    val scanner = Scanner(System.`in`)
    print("Introduzca los coeficientes del polinomio separados por espacios: ")
    val coeficientes = scanner.nextLine().split(" ").map { it.toDouble() }.toDoubleArray()
    return coeficientes
}

// Creamos un objeto `Polinomio` con los coeficientes introducidos por el usuario.
val polinomio = Polinomio(pedirCoeficientes())

// Imprimimos el polinomio en formato estándar.
println("Polinomio:")
polinomio.imprimir()

// Evaluamos el polinomio en un punto dado.
println("Evaluación del polinomio en x = 2:")
println(polinomio.evaluar(2.0))

// Calculamos las raíces del polinomio.
println("Raíces del polinomio:")
polinomio.raices().forEach { println(it) }
```

**Explicación:**

El código anterior define una clase `Polinomio` que representa un polinomio. La clase tiene dos atributos: `coeficientes` que es un arreglo de dobles que contiene los coeficientes del polinomio y `grado` que es un entero que contiene el grado del polinomio.

La clase `Polinomio` tiene tres métodos:

* `evaluar`: Este método evalúa el polinomio en un punto dado.
* `raices`: Este método calcula las raíces del polinomio.
* `imprimir`: Este método imprime el polinomio en formato estándar.

El código también define una función `pedirCoeficientes()` que pide al usuario que introduzca los coeficientes del polinomio.

Por último, el código crea un objeto `Polinomio` con los coeficientes introducidos por el usuario, imprime el polinomio en formato estándar, evalúa el polinomio en un punto dado y calcula las raíces del polinomio.