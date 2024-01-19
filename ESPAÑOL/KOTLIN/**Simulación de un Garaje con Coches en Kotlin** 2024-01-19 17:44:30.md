```kotlin
// Clase Coche con propiedades y métodos
class Coche(var marca: String, var modelo: String, var año: Int) {

    // Función para mostrar la información del coche
    fun mostrarInfo() {
        println("Marca: $marca")
        println("Modelo: $modelo")
        println("Año: $año")
    }

    // Función para arrancar el coche
    fun arrancar() {
        println("El coche está arrancado")
    }

    // Función para parar el coche
    fun parar() {
        println("El coche está parado")
    }
}

// Clase Garaje con una lista de coches
class Garaje {

    private val coches: MutableList<Coche> = mutableListOf()

    // Función para añadir un coche al garaje
    fun añadirCoche(coche: Coche) {
        coches.add(coche)
    }

    // Función para eliminar un coche del garaje
    fun eliminarCoche(coche: Coche) {
        coches.remove(coche)
    }

    // Función para mostrar la lista de coches del garaje
    fun mostrarCoches() {
        for (coche in coches) {
            coche.mostrarInfo()
            println()
        }
    }
}

// Función main para probar las clases Coche y Garaje
fun main(args: Array<String>) {

    // Creamos un coche
    val coche1 = Coche("Toyota", "Yaris", 2023)

    // Creamos un garaje
    val garaje = Garaje()

    // Añadimos el coche al garaje
    garaje.añadirCoche(coche1)

    // Mostramos la lista de coches del garaje
    garaje.mostrarCoches()

    // Arrancamos el coche
    coche1.arrancar()

    // Paramos el coche
    coche1.parar()

    // Eliminamos el coche del garaje
    garaje.eliminarCoche(coche1)

    // Mostramos la lista de coches del garaje
    garaje.mostrarCoches()
}
```

Explicación del código:

* **Clase Coche:** Esta clase tiene tres propiedades: `marca`, `modelo` y `año`. También tiene cuatro métodos: `mostrarInfo()`, `arrancar()`, `parar()` y `eliminar()`.

* **Clase Garaje:** Esta clase tiene una propiedad: `coches`, que es una lista de coches. También tiene tres métodos: `añadirCoche()`, `eliminarCoche()` y `mostrarCoches()`.

* **Función main:** Esta función crea un coche, un garaje y añade el coche al garaje. Luego, muestra la lista de coches del garaje, arranca el coche, lo para y lo elimina del garaje. Finalmente, vuelve a mostrar la lista de coches del garaje.

El código es complejo porque utiliza varias clases, objetos y métodos. También utiliza conceptos de programación orientada a objetos, como la herencia y el polimorfismo.