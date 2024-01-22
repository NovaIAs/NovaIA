```kotlin
// IMPORTACIÓN DE BIBLIOTECAS
import java.util.*

// CLASE PRINCIPAL
class CalculadoraCompleja {

    // FUNCIÓN PRINCIPAL
    fun calcular() {
        // CREACIÓN DE VARIABLES
        val numerosComplejos = ArrayList<NumeroComplejo>()
        var opcion = 0

        // MENÚ DE OPCIONES
        do {
            println("-------------------------------------------------")
            println("CALCULADORA DE NÚMEROS COMPLEJOS")
            println("-------------------------------------------------")
            println("1. Ingresar número complejo")
            println("2. Sumar números complejos")
            println("3. Restar números complejos")
            println("4. Multiplicar números complejos")
            println("5. Dividir números complejos")
            println("6. Conjugado de un número complejo")
            println("7. Módulo de un número complejo")
            println("8. Argumento de un número complejo")
            println("9. Salir")
            println("-------------------------------------------------")
            print("Ingrese una opción: ")
            opcion = readLine()!!.toInt()

            // EJECUCIÓN DE LA OPCIÓN SELECCIONADA
            when (opcion) {
                1 -> ingresarNumeroComplejo(numerosComplejos)
                2 -> sumarNumerosComplejos(numerosComplejos)
                3 -> restarNumerosComplejos(numerosComplejos)
                4 -> multiplicarNumerosComplejos(numerosComplejos)
                5 -> dividirNumerosComplejos(numerosComplejos)
                6 -> conjugadoNumeroComplejo(numerosComplejos)
                7 -> moduloNumeroComplejo(numerosComplejos)
                8 -> argumentoNumeroComplejo(numerosComplejos)
            }
        } while (opcion != 9)
    }

    // FUNCIÓN PARA INGRESAR UN NÚMERO COMPLEJO
    private fun ingresarNumeroComplejo(numerosComplejos: ArrayList<NumeroComplejo>) {
        println("-------------------------------------------------")
        println("INGRESO DE NÚMERO COMPLEJO")
        println("-------------------------------------------------")
        print("Ingrese la parte real: ")
        val real = readLine()!!.toDouble()
        print("Ingrese la parte imaginaria: ")
        val imaginaria = readLine()!!.toDouble()
        numerosComplejos.add(NumeroComplejo(real, imaginaria))
        println("Número complejo ingresado: " + numerosComplejos[numerosComplejos.size - 1])
        println("-------------------------------------------------")
    }

    // FUNCIÓN PARA SUMAR NÚMEROS COMPLEJOS
    private fun sumarNumerosComplejos(numerosComplejos: ArrayList<NumeroComplejo>) {
        println("-------------------------------------------------")
        println("SUMA DE NÚMEROS COMPLEJOS")
        println("-------------------------------------------------")
        print("Ingrese el primer número complejo: ")
        val num1 = numerosComplejos[readLine()!!.toInt() - 1]
        print("Ingrese el segundo número complejo: ")
        val num2 = numerosComplejos[readLine()!!.toInt() - 1]
        println("Suma de números complejos: " + num1 + " + " + num2 + " = " + (num1 + num2))
        println("-------------------------------------------------")
    }

    // FUNCIÓN PARA RESTAR NÚMEROS COMPLEJOS
    private fun restarNumerosComplejos(numerosComplejos: ArrayList<NumeroComplejo>) {
        println("-------------------------------------------------")
        println("RESTA DE NÚMEROS COMPLEJOS")
        println("-------------------------------------------------")
        print("Ingrese el primer número complejo: ")
        val num1 = numerosComplejos[readLine()!!.toInt() - 1]
        print("Ingrese el segundo número complejo: ")
        val num2 = numerosComplejos[readLine()!!.toInt() - 1]
        println("Resta de números complejos: " + num1 + " - " + num2 + " = " + (num1 - num2))
        println("-------------------------------------------------")
    }

    // FUNCIÓN PARA MULTIPLICAR NÚMEROS COMPLEJOS
    private fun multiplicarNumerosComplejos(numerosComplejos: ArrayList<NumeroComplejo>) {
        println("-------------------------------------------------")
        println("MULTIPLICACIÓN DE NÚMEROS COMPLEJOS")
        println("-------------------------------------------------")
        print("Ingrese el primer número complejo: ")
        val num1 = numerosComplejos[readLine()!!.toInt() - 1]
        print("Ingrese el segundo número complejo: ")
        val num2 = numerosComplejos[readLine()!!.toInt() - 1]
        println("Multiplicación de números complejos: " + num1 + " * " + num2 + " = " + (num1 * num2))
        println("-------------------------------------------------")
    }

    // FUNCIÓN PARA DIVIDIR NÚMEROS COMPLEJOS
    private fun dividirNumerosComplejos(numerosComplejos: ArrayList<NumeroComplejo>) {
        println("-------------------------------------------------")
        println("DIVISIÓN DE NÚMEROS COMPLEJOS")
        println("-------------------------------------------------")
        print("Ingrese el primer número complejo: ")
        val num1 = numerosComplejos[readLine()!!.toInt() - 1]
        print("Ingrese el segundo número complejo: ")
        val num2 = numerosComplejos[readLine()!!.toInt() - 1]
        println("División de números complejos: " + num1 + " / " + num2 + " = " + (num1 / num2))
        println("-------------------------------------------------")
    }

    // FUNCIÓN PARA OBTENER EL CONJUGADO DE UN NÚMERO COMPLEJO
    private fun conjugadoNumeroComplejo(numerosComplejos: ArrayList<NumeroComplejo>) {
        println("-------------------------------------------------")
        println("CONJUGADO DE UN NÚMERO COMPLEJO")
        println("-------------------------------------------------")
        print("Ingrese el número complejo: ")
        val num = numerosComplejos[readLine()!!.toInt() - 1]
        println("Conjugado de número complejo: " + num + " = " + num.conjugado())
        println("-------------------------------------------------")
    }

    // FUNCIÓN PARA OBTENER EL MÓDULO DE UN NÚMERO COMPLEJO
    private fun moduloNumeroComplejo(numerosComplejos: ArrayList<NumeroComplejo>) {
        println("-------------------------------------------------")
        println("MÓDULO DE UN NÚMERO COMPLEJO")
        println("-------------------------------------------------")
        print("Ingrese el número complejo: ")
        val num = numerosComplejos[readLine()!!.toInt() - 1]
        println("Módulo de número complejo: " + num + " = " + num.modulo())
        println("-------------------------------------------------")
    }

    // FUNCIÓN PARA OBTENER EL ARGUMENTO DE UN NÚMERO COMPLEJO
    private fun argumentoNumeroComplejo(numerosComplejos: ArrayList<NumeroComplejo>) {
        println("-------------------------------------------------")
        println("ARGUMENTO DE UN NÚMERO COMPLEJO")
        println("-------------------------------------------------")
        print("Ingrese el número complejo: ")
        val num = numerosComplejos[readLine()!!.toInt() - 1]
        println("Argumento de número complejo: " + num + " = " + num.argumento())
        println("-------------------------------------------------")
    }

    // CLASE NÚMERO COMPLEJO
    class NumeroComplejo(var real: Double, var imaginaria: Double) {

        // FUNCIÓN PARA SUMAR DOS NÚMEROS COMPLEJOS
        operator fun plus(otro: NumeroComplejo): NumeroComplejo {
            return NumeroComplejo(real + otro.real, imaginaria + otro.imaginaria)
        }

        // FUNCIÓN PARA RESTAR DOS NÚMEROS COMPLEJOS
        operator fun minus(otro: NumeroComplejo): NumeroComplejo {
            return NumeroComplejo(real - otro.real, imaginaria - otro.imaginaria)
        }

        // FUNCIÓN PARA MULTIPLICAR DOS NÚMEROS COMPLEJOS
        operator fun times(otro: NumeroComplejo): NumeroComplejo {
            return NumeroComplejo((real * otro.real) - (imaginaria * otro.imaginaria), (real * otro.imaginaria) + (imaginaria * otro.real))
        }

        // FUNCIÓN PARA DIVIDIR DOS NÚMEROS COMPLEJOS
        operator fun div(otro: NumeroComplejo): NumeroComplejo {
            val denominador = (otro.real * otro.real) + (otro.imaginaria * otro.imaginaria)
            return NumeroComplejo(((real * otro.real) + (imaginaria * otro.imaginaria)) / denominador, ((imaginaria * otro.real) - (real * otro.imaginaria)) / denominador)
        }

        // FUNCIÓN PARA OBTENER EL CONJUGADO DE UN NÚMERO COMPLEJO
        fun conjugado(): NumeroComplejo {
            return NumeroComplejo(real, -imaginaria)
        }

        // FUNCIÓN PARA OBTENER EL MÓDULO DE UN NÚMERO COMPLEJO
        fun modulo(): Double {
            return Math.sqrt((real * real) + (imaginaria * imaginaria))
        }

        // FUNCIÓN PARA OBTENER EL ARGUMENTO DE UN NÚMERO COMPLEJO
        fun argumento(): Double {
            return Math.atan2(imaginaria, real)
        }

        // FUNCIÓN PARA OBTENER LA REPRESENTACIÓN EN STRING DE UN NÚMERO COMPLEJO
        override fun toString(): String {
            return "$real + ${imaginaria}i"
        }
    }
}