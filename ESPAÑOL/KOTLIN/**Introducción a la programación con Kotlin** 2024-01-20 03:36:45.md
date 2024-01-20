```kotlin
// Clase principal del programa
class ProgramaPrincipal {

    // Método principal del programa
    fun main(args: Array<String>) {

        // Creamos una lista de números
        val listaNumeros = listOf(1, 2, 3, 4, 5)

        // Creamos una lista de cadenas
        val listaCadenas = listOf("Hola", "Mundo", "!")

        // Creamos una lista de objetos de tipo Persona
        val listaPersonas = listOf(Persona("Juan", 20), Persona("María", 30))

        // Imprimimos la lista de números
        println("Lista de números:")
        for (numero in listaNumeros) {
            println(numero)
        }

        // Imprimimos la lista de cadenas
        println("Lista de cadenas:")
        for (cadena in listaCadenas) {
            println(cadena)
        }

        // Imprimimos la lista de personas
        println("Lista de personas:")
        for (persona in listaPersonas) {
            println("${persona.nombre} - ${persona.edad}")
        }

        // Creamos una función lambda para sumar dos números
        val sumaDosNumeros = { a: Int, b: Int -> a + b }

        // Llamamos a la función lambda
        val resultadoSuma = sumaDosNumeros(10, 20)

        // Imprimimos el resultado de la suma
        println("Resultado de la suma: $resultadoSuma")

        // Creamos una clase anónima para implementar la interfaz Comparable
        val comparable = object : Comparable<Persona> {
            override fun compareTo(other: Persona): Int {
                return this.edad - other.edad
            }
        }

        // Ordenamos la lista de personas por edad
        listaPersonas.sortedWith(comparable).forEach { println("${it.nombre} - ${it.edad}") }

    }

    // Clase Persona
    class Persona(val nombre: String, val edad: Int)

}
```

Explicación del código:

* La clase `ProgramaPrincipal` es la clase principal del programa y contiene el método `main()`, que es el punto de entrada del programa.
* La lista `listaNumeros` contiene una lista de números.
* La lista `listaCadenas` contiene una lista de cadenas.
* La lista `listaPersonas` contiene una lista de objetos de tipo `Persona`.
* La función lambda `sumaDosNumeros` es una función que recibe dos números y devuelve la suma de los mismos.
* La variable `comparable` es una clase anónima que implementa la interfaz `Comparable` y define el método `compareTo()` para comparar personas por edad.
* El método `sortedWith()` ordena la lista de personas por edad utilizando el comparador `comparable`.