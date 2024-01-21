```scala
object Complejo {

    def main(args: Array[String]): Unit = {

        // Definición de números complejos
        val c1 = Complejo(3, 4)
        val c2 = Complejo(5, -2)

        // Suma de números complejos
        val c3 = c1 + c2
        println("Suma: " + c3)

        // Resta de números complejos
        val c4 = c1 - c2
        println("Resta: " + c4)

        // Multiplicación de números complejos
        val c5 = c1 * c2
        println("Multiplicación: " + c5)

        // División de números complejos
        val c6 = c1 / c2
        println("División: " + c6)

        // Conjugado de un número complejo
        val c7 = c1.conjugado()
        println("Conjugado: " + c7)

        // Módulo de un número complejo
        val c8 = c1.modulo()
        println("Módulo: " + c8)

        // Fase de un número complejo
        val c9 = c1.fase()
        println("Fase: " + c9)
    }

    // Clase que representa un número complejo
    case class Complejo(real: Double, imaginario: Double) {

        // Suma de números complejos
        def +(otro: Complejo): Complejo = {
            Complejo(real + otro.real, imaginario + otro.imaginario)
        }

        // Resta de números complejos
        def -(otro: Complejo): Complejo = {
            Complejo(real - otro.real, imaginario - otro.imaginario)
        }

        // Multiplicación de números complejos
        def *(otro: Complejo): Complejo = {
            val real = this.real * otro.real - this.imaginario * otro.imaginario
            val imaginario = this.real * otro.imaginario + this.imaginario * otro.real
            Complejo(real, imaginario)
        }

        // División de números complejos
        def /(otro: Complejo): Complejo = {
            val denominador = otro.real * otro.real + otro.imaginario * otro.imaginario
            val real = (this.real * otro.real + this.imaginario * otro.imaginario) / denominador
            val imaginario = (this.imaginario * otro.real - this.real * otro.imaginario) / denominador
            Complejo(real, imaginario)
        }

        // Conjugado de un número complejo
        def conjugado(): Complejo = {
            Complejo(real, -imaginario)
        }

        // Módulo de un número complejo
        def modulo(): Double = {
            Math.sqrt(real * real + imaginario * imaginario)
        }

        // Fase de un número complejo
        def fase(): Double = {
            Math.atan2(imaginario, real)
        }

        // Sobrescritura del método toString() para mostrar el número complejo en formato "a + bi"
        override def toString: String = {
            real + " + " + imaginario + "i"
        }
    }
}
```

Explicación del código:

* La clase `Complejo` representa un número complejo con una parte real y una parte imaginaria.
* Los métodos `+`, `-`, `*`, `/`, `conjugado()`, `modulo()` y `fase()` implementan las operaciones aritméticas básicas y las funciones trigonométricas para números complejos.
* El método `toString()` reescribe el método por defecto para mostrar el número complejo en formato "a + bi".
* La clase `object` `Complejo` contiene el método `main()` que se ejecuta cuando se ejecuta el programa.
* En el método `main()`, se crean dos números complejos `c1` y `c2` y se realizan las operaciones aritméticas básicas y las funciones trigonométricas sobre ellos.