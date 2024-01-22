```kotlin
// Clase principal
class CalculadoraCompleja {

    // Función para sumar dos números complejos
    fun sumar(z1: Complejo, z2: Complejo): Complejo {
        return Complejo(z1.real + z2.real, z1.imaginario + z2.imaginario)
    }

    // Función para restar dos números complejos
    fun restar(z1: Complejo, z2: Complejo): Complejo {
        return Complejo(z1.real - z2.real, z1.imaginario - z2.imaginario)
    }

    // Función para multiplicar dos números complejos
    fun multiplicar(z1: Complejo, z2: Complejo): Complejo {
        return Complejo(
            z1.real * z2.real - z1.imaginario * z2.imaginario,
            z1.real * z2.imaginario + z1.imaginario * z2.real
        )
    }

    // Función para dividir dos números complejos
    fun dividir(z1: Complejo, z2: Complejo): Complejo {
        val denominador = z2.real * z2.real + z2.imaginario * z2.imaginario
        return Complejo(
            (z1.real * z2.real + z1.imaginario * z2.imaginario) / denominador,
            (z1.imaginario * z2.real - z1.real * z2.imaginario) / denominador
        )
    }

    // Función para calcular la magnitud de un número complejo
    fun magnitud(z: Complejo): Double {
        return Math.sqrt(z.real * z.real + z.imaginario * z.imaginario)
    }

    // Función para calcular el argumento de un número complejo
    fun argumento(z: Complejo): Double {
        return Math.atan2(z.imaginario, z.real)
    }

    // Función para calcular la raíz cuadrada de un número complejo
    fun raizCuadrada(z: Complejo): Complejo {
        val magnitud = magnitud(z)
        val argumento = argumento(z)
        return Complejo(
            Math.sqrt(magnitud / 2) * Math.cos(argumento / 2),
            Math.sqrt(magnitud / 2) * Math.sin(argumento / 2)
        )
    }
}

// Clase para representar un número complejo
data class Complejo(val real: Double, val imaginario: Double)

// Ejemplo de uso
val calculadora = CalculadoraCompleja()

// Crear dos números complejos
val z1 = Complejo(2.0, 3.0)
val z2 = Complejo(4.0, 5.0)

// Sumar los dos números complejos
val suma = calculadora.sumar(z1, z2)

// Restar los dos números complejos
val resta = calculadora.restar(z1, z2)

// Multiplicar los dos números complejos
val multiplicacion = calculadora.multiplicar(z1, z2)

// Dividir los dos números complejos
val division = calculadora.dividir(z1, z2)

// Calcular la magnitud de un número complejo
val magnitud = calculadora.magnitud(z1)

// Calcular el argumento de un número complejo
val argumento = calculadora.argumento(z1)

// Calcular la raíz cuadrada de un número complejo
val raizCuadrada = calculadora.raizCuadrada(z1)

// Imprimir los resultados de las operaciones
println("Suma: ${suma.real} + ${suma.imaginario}i")
println("Resta: ${resta.real} + ${resta.imaginario}i")
println("Multiplicación: ${multiplicacion.real} + ${multiplicacion.imaginario}i")
println("División: ${division.real} + ${division.imaginario}i")
println("Magnitud: ${magnitud}")
println("Argumento: ${argumento}")
println("Raíz cuadrada: ${raizCuadrada.real} + ${raizCuadrada.imaginario}i")
```

Explicación del código:

* La clase `CalculadoraCompleja` define las operaciones básicas para realizar cálculos con números complejos: suma, resta, multiplicación, división, magnitud, argumento y raíz cuadrada.
* La clase `Complejo` representa un número complejo con dos valores: la parte real y la parte imaginaria.
* La función `sumar` calcula la suma de dos números complejos.
* La función `restar` calcula la resta de dos números complejos.
* La función `multiplicar` calcula la multiplicación de dos números complejos.
* La función `dividir` calcula la división de dos números complejos.
* La función `magnitud` calcula la magnitud de un número complejo.
* La función `argumento` calcula el argumento de un número complejo.
* La función `raizCuadrada` calcula la raíz cuadrada de un número complejo.
* En el ejemplo de uso, se crean dos números complejos, se realizan las operaciones básicas y se imprimen los resultados.