```kotlin
// Declaración de variables
val listaDeNumeros = arrayListOf(1, 2, 3, 4, 5)
val mapaDePares = mutableMapOf<Int, String>()
val funcionSuma: (Int, Int) -> Int = { a, b -> a + b }

// Bucle forEach para recorrer la lista de números
listaDeNumeros.forEach { numero ->
    // Si el número es par, se agrega al mapa de pares
    if (numero % 2 == 0) {
        mapaDePares[numero] = "Par"
    }
}

// Bucle for clásico para recorrer el mapa de pares
for (par in mapaDePares) {
    // Se imprime el par (clave y valor)
    println("${par.key} es ${par.value}")
}

// Uso de la función de orden superior map para crear una nueva lista de números
val listaDeNumerosCuadrados = listaDeNumeros.map { numero -> numero * numero }

// Uso de la función de orden superior filter para filtrar la lista de números cuadrados
val listaDeNumerosCuadradosPares = listaDeNumerosCuadrados.filter { numero -> numero % 2 == 0 }

// Uso de la función de orden superior reduce para sumar todos los elementos de la lista de números cuadrados pares
val sumaDeNumerosCuadradosPares = listaDeNumerosCuadradosPares.reduce(funcionSuma)

// Impresión del resultado
println("La suma de los números cuadrados pares es: $sumaDeNumerosCuadradosPares")

// Declaración de una clase
class Persona(nombre: String, edad: Int) {
    val nombre = nombre
    val edad = edad

    fun saludar() {
        println("Hola, mi nombre es $nombre y tengo $edad años")
    }
}

// Creación de un objeto de la clase Persona
val persona = Persona("Juan", 25)

// Llamada al método saludar del objeto persona
persona.saludar()


// Función principal
fun main(args: Array<String>) {
    // Código que se ejecuta al iniciar el programa
    println("¡Hola, Kotlin!")
}
```

Explicación del código:

* Se declaran varias variables, incluyendo una lista de números, un mapa de pares, una función de orden superior y una clase Persona.
* Se utiliza un bucle forEach para recorrer la lista de números y agregar los números pares al mapa de pares.
* Se utiliza un bucle for clásico para recorrer el mapa de pares e imprimir cada par (clave y valor).
* Se utilizan las funciones de orden superior map, filter y reduce para crear una nueva lista de números cuadrados pares y sumar todos los elementos de la lista.
* Se crea un objeto de la clase Persona y se llama al método saludar del objeto.
* La función principal se ejecuta al iniciar el programa y imprime un mensaje.