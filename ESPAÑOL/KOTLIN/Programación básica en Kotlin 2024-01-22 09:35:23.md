```kotlin
// Importamos las librerías necesarias.
import java.util.*
import kotlin.collections.ArrayList

// Creamos una clase llamada "Main" que será el punto de entrada de nuestro programa.
class Main {

    // Creamos una función principal llamada "main" que será ejecutada cuando se ejecute el programa.
    fun main(args: Array<String>) {

        // Creamos una lista de números enteros.
        val listaNumeros = ArrayList<Int>()

        // Agregamos algunos números a la lista.
        listaNumeros.add(1)
        listaNumeros.add(2)
        listaNumeros.add(3)

        // Imprimimos la lista de números.
        println("Lista de números:")
        for (numero in listaNumeros) {
            println(numero)
        }

        // Creamos un mapa de claves y valores.
        val mapaClavesValores = HashMap<String, Int>()

        // Agregamos algunos pares de claves y valores al mapa.
        mapaClavesValores["Uno"] = 1
        mapaClavesValores["Dos"] = 2
        mapaClavesValores["Tres"] = 3

        // Imprimimos el mapa de claves y valores.
        println("Mapa de claves y valores:")
        for ((clave, valor) in mapaClavesValores) {
            println("$clave: $valor")
        }

        // Creamos una clase llamada "Persona" que tendrá un nombre y una edad.
        class Persona(val nombre: String, val edad: Int) {

            // Creamos una función para imprimir el nombre y la edad de la persona.
            fun imprimir() {
                println("Nombre: $nombre")
                println("Edad: $edad")
            }
        }

        // Creamos una lista de personas.
        val listaPersonas = ArrayList<Persona>()

        // Agregamos algunas personas a la lista.
        listaPersonas.add(Persona("Juan", 25))
        listaPersonas.add(Persona("María", 30))
        listaPersonas.add(Persona("Pedro", 35))

        // Imprimimos la lista de personas.
        println("Lista de personas:")
        for (persona in listaPersonas) {
            persona.imprimir()
        }

        // Creamos una función para calcular el promedio de una lista de números.
        fun promedio(listaNumeros: ArrayList<Int>): Double {
            var suma = 0
            for (numero in listaNumeros) {
                suma += numero
            }
            return suma / listaNumeros.size
        }

        // Calculamos el promedio de la lista de números.
        val promedioNumeros = promedio(listaNumeros)

        // Imprimimos el promedio de la lista de números.
        println("Promedio de la lista de números: $promedioNumeros")

    }
}
```

Explicación del código:

* Importamos las librerías necesarias.
* Creamos una clase llamada "Main" que será el punto de entrada de nuestro programa.
* Creamos una función principal llamada "main" que será ejecutada cuando se ejecute el programa.
* Creamos una lista de números enteros.
* Agregamos algunos números a la lista.
* Imprimimos la lista de números.
* Creamos un mapa de claves y valores.
* Agregamos algunos pares de claves y valores al mapa.
* Imprimimos el mapa de claves y valores.
* Creamos una clase llamada "Persona" que tendrá un nombre y una edad.
* Creamos una función para imprimir el nombre y la edad de la persona.
* Creamos una lista de personas.
* Agregamos algunas personas a la lista.
* Imprimimos la lista de personas.
* Creamos una función para calcular el promedio de una lista de números.
* Calculamos el promedio de la lista de números.
* Imprimimos el promedio de la lista de números.