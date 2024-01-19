```kotlin
// Definición de una clase abstracta con el nombre "Animal"
abstract class Animal {

    // Definimos una propiedad abstracta "nombre" de tipo String
    abstract var nombre: String

    // Definimos una función abstracta "comer" que no tiene parámetros y devuelve un String
    abstract fun comer(): String

    // Definimos una función concreta "dormir" que no tiene parámetros y devuelve un String
    open fun dormir(): String {
        return "ZZZZ..."
    }
}

// Definición de una clase concreta que hereda de la clase abstracta "Animal" con el nombre "Perro"
class Perro(override var nombre: String) : Animal() {

    // Sobreescribimos la función "comer" que devuelve el mensaje "Estoy comiendo comida para perros"
    override fun comer(): String {
        return "Estoy comiendo comida para perros"
    }

    // Sobreescribimos la función "dormir" que devuelve el mensaje "Estoy durmiendo como un perro"
    override fun dormir(): String {
        return "Estoy durmiendo como un perro"
    }

    // Definimos una función concreta "ladrar" que no tiene parámetros y devuelve un String
    fun ladrar(): String {
        return "Guau, guau!"
    }
}

// Definición de una clase concreta que hereda de la clase abstracta "Animal" con el nombre "Gato"
class Gato(override var nombre: String) : Animal() {

    // Sobreescribimos la función "comer" que devuelve el mensaje "Estoy comiendo comida para gatos"
    override fun comer(): String {
        return "Estoy comiendo comida para gatos"
    }

    // Sobreescribimos la función "dormir" que devuelve el mensaje "Estoy durmiendo como un gato"
    override fun dormir(): String {
        return "Estoy durmiendo como un gato"
    }

    // Definimos una función concreta "maullar" que no tiene parámetros y devuelve un String
    fun maullar(): String {
        return "Miau, miau!"
    }
}

// Definición de una función que recibe una lista de objetos de la clase "Animal" y devuelve un String
fun imprimirInformacionAnimales(animales: List<Animal>): String {

    // Creamos un StringBuilder para almacenar la información de los animales
    val builder = StringBuilder()

    // Iteramos sobre la lista de animales
    for (animal in animales) {

        // Agregamos el nombre del animal al StringBuilder
        builder.append("Nombre: ${animal.nombre}\n")

        // Agregamos el mensaje de comer del animal al StringBuilder
        builder.append("Comer: ${animal.comer()}\n")

        // Agregamos el mensaje de dormir del animal al StringBuilder
        builder.append("Dormir: ${animal.dormir()}\n")

        // Si el animal es un perro, agregamos el mensaje de ladrar al StringBuilder
        if (animal is Perro) {
            builder.append("Ladrar: ${animal.ladrar()}\n")
        }

        // Si el animal es un gato, agregamos el mensaje de maullar al StringBuilder
        if (animal is Gato) {
            builder.append("Maullar: ${animal.maullar()}\n")
        }

        // Agregamos una línea en blanco al StringBuilder
        builder.append("\n")
    }

    // Devolvemos el contenido del StringBuilder
    return builder.toString()
}

// Creamos una lista de objetos de la clase "Animal"
val animales = listOf(
    Perro("Firulais"),
    Gato("Michi"),
    Perro("Toby"),
    Gato("Luna")
)

// Imprimimos la información de los animales
println(imprimirInformacionAnimales(animales))
```

Este código define una clase abstracta llamada "Animal" que tiene una propiedad abstracta llamada "nombre" y dos funciones abstractas llamadas "comer()" y "dormir()". También define dos clases concretas llamadas "Perro" y "Gato" que heredan de la clase "Animal" y sobreescriben las funciones abstractas.

Además, define una función llamada "imprimirInformacionAnimales()" que recibe una lista de objetos de la clase "Animal" y devuelve un String con la información de los animales.

Por último, se crea una lista de objetos de la clase "Animal" y se llama a la función "imprimirInformacionAnimales()" para imprimir la información de los animales.

Este código es complejo porque utiliza conceptos de programación orientada a objetos como la herencia, el polimorfismo y la abstracción. También utiliza una función que recibe una lista de objetos como parámetro y devuelve un String con la información de los objetos.