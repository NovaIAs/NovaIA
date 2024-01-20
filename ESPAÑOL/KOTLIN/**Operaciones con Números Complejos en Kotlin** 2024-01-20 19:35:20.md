```kotlin
// Creación de una clase para representar un número complejo en forma rectangular
class NumeroComplejo(real: Double, imaginario: Double) {
    private var real = real
    private var imaginario = imaginario

    // Métodos para obtener la parte real e imaginaria
    fun getReal(): Double {
        return real
    }

    fun getImaginario(): Double {
        return imaginario
    }

    // Métodos para suma, resta, multiplicación y división de números complejos
    fun sumar(complejo: NumeroComplejo): NumeroComplejo {
        val real = this.real + complejo.real
        val imaginario = this.imaginario + complejo.imaginario
        return NumeroComplejo(real, imaginario)
    }

    fun restar(complejo: NumeroComplejo): NumeroComplejo {
        val real = this.real - complejo.real
        val imaginario = this.imaginario - complejo.imaginario
        return NumeroComplejo(real, imaginario)
    }

    fun multiplicar(complejo: NumeroComplejo): NumeroComplejo {
        val real = this.real * complejo.real - this.imaginario * complejo.imaginario
        val imaginario = this.real * complejo.imaginario + this.imaginario * complejo.real
        return NumeroComplejo(real, imaginario)
    }

    fun dividir(complejo: NumeroComplejo): NumeroComplejo {
        val denominador = complejo.real * complejo.real + complejo.imaginario * complejo.imaginario
        val real = (this.real * complejo.real + this.imaginario * complejo.imaginario) / denominador
        val imaginario = (this.imaginario * complejo.real - this.real * complejo.imaginario) / denominador
        return NumeroComplejo(real, imaginario)
    }

    // Método para calcular el módulo del número complejo
    fun modulo(): Double {
        return Math.sqrt(real * real + imaginario * imaginario)
    }

    // Método para calcular el argumento del número complejo
    fun argumento(): Double {
        return Math.atan2(imaginario, real)
    }

    // Método para representar el número complejo en forma de cadena
    override fun toString(): String {
        return "$real + ${imaginario}i"
    }
}

// Clase para probar la clase NumeroComplejo
class Main {
    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            // Creamos dos números complejos
            val complejo1 = NumeroComplejo(3.0, 4.0)
            val complejo2 = NumeroComplejo(5.0, -2.0)

            // Sumamos, restamos, multiplicamos y dividimos los números complejos
            val suma = complejo1.sumar(complejo2)
            val resta = complejo1.restar(complejo2)
            val multiplicacion = complejo1.multiplicar(complejo2)
            val division = complejo1.dividir(complejo2)

            // Imprimimos los resultados
            println("Suma: $suma")
            println("Resta: $resta")
            println("Multiplicación: $multiplicacion")
            println("División: $division")

            // Calculamos el módulo y el argumento de los números complejos
            val modulo1 = complejo1.modulo()
            val modulo2 = complejo2.modulo()
            val argumento1 = complejo1.argumento()
            val argumento2 = complejo2.argumento()

            // Imprimimos los resultados
            println("Módulo del primer número complejo: $modulo1")
            println("Módulo del segundo número complejo: $modulo2")
            println("Argumento del primer número complejo: $argumento1")
            println("Argumento del segundo número complejo: $argumento2")
        }
    }
}
```

Este código define una clase `NumeroComplejo` para representar números complejos en forma rectangular, con métodos para realizar operaciones aritméticas básicas (suma, resta, multiplicación y división), calcular el módulo y el argumento del número complejo, y representarlo en forma de cadena.

La clase `Main` demuestra el uso de la clase `NumeroComplejo` creando dos números complejos, realizando operaciones aritméticas con ellos y calculando sus módulos y argumentos.