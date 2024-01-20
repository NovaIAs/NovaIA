```kotlin
// Este código crea un objeto de la clase `Persona`.

class Persona(val nombre: String, val edad: Int) {

    // Esta función devuelve el nombre de la persona.

    fun obtenerNombre(): String {
        return nombre
    }

    // Esta función devuelve la edad de la persona.

    fun obtenerEdad(): Int {
        return edad
    }

    // Esta función devuelve un saludo de la persona.

    fun saludar(): String {
        return "Hola, mi nombre es $nombre y tengo $edad años."
    }
}

// Este código crea una lista de objetos de la clase `Persona`.

val personas = listOf(
    Persona("Juan", 25),
    Persona("María", 30),
    Persona("Pedro", 35)
)

// Este código imprime en la consola el nombre de cada persona de la lista.

for (persona in personas) {
    println(persona.obtenerNombre())
}

// Este código imprime en la consola la edad de cada persona de la lista.

for (persona in personas) {
    println(persona.obtenerEdad())
}

// Este código imprime en la consola un saludo de cada persona de la lista.

for (persona in personas) {
    println(persona.saludar())
}
```

Explicación del código:

* La clase `Persona` define un objeto con dos propiedades: `nombre` y `edad`.
* La función `obtenerNombre()` devuelve el nombre de la persona.
* La función `obtenerEdad()` devuelve la edad de la persona.
* La función `saludar()` devuelve un saludo de la persona.
* La lista `personas` contiene tres objetos de la clase `Persona`.
* El bucle `for` recorre la lista `personas` e imprime en la consola el nombre de cada persona.
* El bucle `for` recorre la lista `personas` e imprime en la consola la edad de cada persona.
* El bucle `for` recorre la lista `personas` e imprime en la consola un saludo de cada persona.