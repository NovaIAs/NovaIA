```kotlin
// Importación de librerías necesarias
import java.util.*

// Clase principal
class Complejo {

    // Atributos
    private var real: Double
    private var imaginario: Double

    // Constructor
    constructor(real: Double, imaginario: Double) {
        this.real = real
        this.imaginario = imaginario
    }

    // Métodos
    fun suma(complejo: Complejo): Complejo {
        // Crea un nuevo complejo con la suma de los valores reales e imaginarios
        return Complejo(this.real + complejo.real, this.imaginario + complejo.imaginario)
    }

    fun resta(complejo: Complejo): Complejo {
        // Crea un nuevo complejo con la resta de los valores reales e imaginarios
        return Complejo(this.real - complejo.real, this.imaginario - complejo.imaginario)
    }

    fun multiplicación(complejo: Complejo): Complejo {
        // Crea un nuevo complejo con la multiplicación de los valores reales e imaginarios
        return Complejo(this.real * complejo.real - this.imaginario * complejo.imaginario,
                this.real * complejo.imaginario + this.imaginario * complejo.real)
    }

    fun división(complejo: Complejo): Complejo? {
        // Verifica si el denominador es cero, en ese caso la división no está definida
        if (complejo.real == 0.0 && complejo.imaginario == 0.0) {
            return null // División por cero no está definida
        }

        // Calcula el conjugado del denominador
        val conjugado = Complejo(complejo.real, -complejo.imaginario)

        // Multiplica el numerador y el denominador por el conjugado del denominador
        val numerador = this.multiplicación(conjugado)
        val denominador = complejo.multiplicación(conjugado)

        // Crea un nuevo complejo con la división de los valores reales e imaginarios
        return Complejo(numerador.real / denominador.real, numerador.imaginario / denominador.real)
    }

    fun módulo(): Double {
        // Calcula el módulo del complejo
        return Math.sqrt(this.real * this.real + this.imaginario * this.imaginario)
    }

    fun argumento(): Double {
        // Calcula el argumento del complejo
        return Math.atan2(this.imaginario, this.real)
    }

    override fun toString(): String {
        // Representación en cadena del complejo
        return "Real: ${this.real}, Imaginario: ${this.imaginario}"
    }
}

// Función principal
fun main(args: Array<String>) {
    // Crea dos complejos
    val complejo1 = Complejo(1.0, 2.0)
    val complejo2 = Complejo(3.0, 4.0)

    // Imprime los complejos
    println("Complejo 1: $complejo1")
    println("Complejo 2: $complejo2")

    // Realiza operaciones aritméticas
    val suma = complejo1.suma(complejo2)
    val resta = complejo1.resta(complejo2)
    val multiplicación = complejo1.multiplicación(complejo2)
    val división = complejo1.división(complejo2)

    // Imprime los resultados
    println("Suma: $suma")
    println("Resta: $resta")
    println("Multiplicación: $multiplicación")
    println("División: $división")

    // Calcula módulo y argumento
    val módulo1 = complejo1.módulo()
    val argumento1 = complejo1.argumento()
    val módulo2 = complejo2.módulo()
    val argumento2 = complejo2.argumento()

    // Imprime módulo y argumento
    println("Módulo complejo 1: $módulo1")
    println("Argumento complejo 1: $argumento1")
    println("Módulo complejo 2: $módulo2")
    println("Argumento complejo 2: $argumento2")
}
```

**Explicación:**

- La clase `Complejo` representa un número complejo con dos atributos, la parte real y la parte imaginaria.
- El constructor crea un nuevo complejo con los valores especificados.
- Los métodos `suma`, `resta`, `multiplicación` y `división` realizan las operaciones aritméticas correspondientes entre dos complejos.
- Los métodos `módulo` y `argumento` calculan el módulo y el argumento del complejo.
- El método `toString` devuelve una representación en cadena del complejo.
- La función `main` crea dos complejos, realiza operaciones aritméticas y calcula el módulo y el argumento de cada uno.
- El código utiliza la librería `java.util` para importar la clase `Scanner`.