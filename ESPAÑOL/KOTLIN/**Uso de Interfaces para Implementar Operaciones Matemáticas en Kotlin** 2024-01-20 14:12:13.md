```kotlin
// Clase principal del programa
class ProgramaPrincipal {

    // Función principal del programa, que se ejecuta al iniciar la aplicación
    fun main(args: Array<String>) {

        // 1. Definimos una interfaz "Operacion"
        interface Operacion {
            fun operar(a: Int, b: Int): Int
        }

        // 2. Definimos una clase "Suma" que implementa la interfaz "Operacion"
        class Suma: Operacion {
            override fun operar(a: Int, b: Int): Int {
                return a + b
            }
        }

        // 3. Definimos una clase "Resta" que implementa la interfaz "Operacion"
        class Resta: Operacion {
            override fun operar(a: Int, b: Int): Int {
                return a - b
            }
        }

        // 4. Definimos una clase "Multiplicacion" que implementa la interfaz "Operacion"
        class Multiplicacion: Operacion {
            override fun operar(a: Int, b: Int): Int {
                return a * b
            }
        }

        // 5. Definimos una clase "Division" que implementa la interfaz "Operacion"
        class Division: Operacion {
            override fun operar(a: Int, b: Int): Int {
                return a / b
            }
        }

        // 6. Definimos una función "calcularOperacion" que recibe una operación y dos enteros, y devuelve el resultado de aplicar la operación a los enteros
        fun calcularOperacion(operacion: Operacion, a: Int, b: Int): Int {
            return operacion.operar(a, b)
        }

        // 7. Creamos instancias de las clases "Suma", "Resta", "Multiplicacion" y "Division"
        val suma = Suma()
        val resta = Resta()
        val multiplicacion = Multiplicacion()
        val division = Division()

        // 8. Probamos las operaciones con diferentes valores
        println("Suma: ${calcularOperacion(suma, 10, 5)}") // Resultado: 15
        println("Resta: ${calcularOperacion(resta, 10, 5)}") // Resultado: 5
        println("Multiplicacion: ${calcularOperacion(multiplicacion, 10, 5)}") // Resultado: 50
        println("Division: ${calcularOperacion(division, 10, 5)}") // Resultado: 2

        // 9. Creamos una lista de operaciones
        val operaciones = listOf(suma, resta, multiplicacion, division)

        // 10. Iteramos sobre la lista de operaciones y las aplicamos a diferentes valores
        for (operacion in operaciones) {
            println("Resultado de aplicar ${operacion.javaClass.simpleName} a 10 y 5: ${calcularOperacion(operacion, 10, 5)}")
        }

        // Resultado:
        // Suma: 15
        // Resta: 5
        // Multiplicacion: 50
        // Division: 2
        // Resultado de aplicar Suma a 10 y 5: 15
        // Resultado de aplicar Resta a 10 y 5: 5
        // Resultado de aplicar Multiplicacion a 10 y 5: 50
        // Resultado de aplicar Division a 10 y 5: 2
    }
}
```

Explicación del código:

1. Definimos una interfaz "Operacion" que define un método "operar" que recibe dos enteros y devuelve un entero. Esta interfaz representa una operación matemática.


2. Definimos cuatro clases que implementan la interfaz "Operacion": "Suma", "Resta", "Multiplicacion" y "Division". Estas clases representan las operaciones matemáticas de suma, resta, multiplicación y división.


3. Definimos una función "calcularOperacion" que recibe una operación y dos enteros, y devuelve el resultado de aplicar la operación a los enteros.


4. Creamos instancias de las clases "Suma", "Resta", "Multiplicacion" y "Division".


5. Probamos las operaciones con diferentes valores.


6. Creamos una lista de operaciones.


7. Iteramos sobre la lista de operaciones y las aplicamos a diferentes valores.


8. Imprimimos los resultados de las operaciones.