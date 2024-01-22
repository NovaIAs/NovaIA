```kotlin
// Importación de librerías

import java.util.*
import kotlin.collections.ArrayList

// Función principal

fun main(args: Array<String>) {

    // Variables

    val nombres = ArrayList<String>()
    val edades = ArrayList<Int>()

    // Bucle para recopilar información

    while (true) {
        println("Ingresa un nombre:")
        val nombre = readLine() ?: break
        nombres.add(nombre)

        println("Ingresa la edad:")
        val edad = readLine()!!.toInt()
        edades.add(edad)

        println("¿Quieres continuar? (S/N)")
        val respuesta = readLine() ?: break
        if (respuesta.toLowerCase() == "n") break
    }

    // Procesamiento de la información

    val promedioEdad = edades.average()
    val mayorEdad = edades.max()
    val menorEdad = edades.min()
    val nombresMayoresEdad = ArrayList<String>()
    val nombresMenoresEdad = ArrayList<String>()

    for (i in 0 until nombres.size) {
        if (edades[i] > promedioEdad) {
            nombresMayoresEdad.add(nombres[i])
        } else {
            nombresMenoresEdad.add(nombres[i])
        }
    }

    // Impresión de resultados

    println("Promedio de edad: $promedioEdad años")
    println("Mayor edad: $mayorEdad años")
    println("Menor edad: $menorEdad años")
    println("Personas mayores de la edad promedio:")
    for (nombre in nombresMayoresEdad) {
        println(nombre)
    }
    println("Personas menores de la edad promedio:")
    for (nombre in nombresMenoresEdad) {
        println(nombre)
    }
}
```

Explicación del código:

1. Importación de librerías:

   - `java.util.*`: Importa todas las clases del paquete java.util, que contiene clases útiles para trabajar con estructuras de datos y colecciones.
   - `kotlin.collections.ArrayList`: Importa la clase `ArrayList`, que es una implementación de una lista mutable en Kotlin.

2. Función principal:

   - `main(args: Array<String>)`: Esta es la función principal del programa. Se ejecuta cuando se ejecuta el programa y recibe como argumento un arreglo de cadenas de caracteres que contiene los argumentos pasados al programa.

3. Variables:

   - `nombres`: Lista que almacenará los nombres de las personas.
   - `edades`: Lista que almacenará las edades de las personas.

4. Bucle para recopilar información:

   - Este bucle `while` se utiliza para recopilar información sobre las personas.
   - El usuario debe ingresar el nombre y la edad de cada persona.
   - El bucle continúa hasta que el usuario ingresa "N" cuando se le pregunta si desea continuar.

5. Procesamiento de la información:

   - Se calcula el promedio de edad, la edad mayor y la edad menor.
   - Se crean dos listas, `nombresMayoresEdad` y `nombresMenoresEdad`, para almacenar los nombres de las personas mayores y menores de la edad promedio, respectivamente.

6. Impresión de resultados:

   - Se imprimen el promedio de edad, la edad mayor y la edad menor.
   - Se imprimen los nombres de las personas mayores y menores de la edad promedio.