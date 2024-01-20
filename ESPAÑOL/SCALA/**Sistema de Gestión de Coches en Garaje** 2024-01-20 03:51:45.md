```scala
// Importamos las librerías necesarias
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

// Definimos la clase Coche
class Coche(var marca: String, var modelo: String, var año: Int) {

    // Métodos para obtener y modificar los atributos
    def getMarca: String = marca
    def setMarca(marca: String): Unit = this.marca = marca

    def getModelo: String = modelo
    def setModelo(modelo: String): Unit = this.modelo = modelo

    def getAño: Int = año
    def setAño(año: Int): Unit = this.año = año

    // Método para imprimir la información del coche
    def mostrarInfo(): Unit = {
        println(s"Marca: $marca")
        println(s"Modelo: $modelo")
        println(s"Año: $año")
    }
}

// Definimos la clase Garaje
class Garaje {

    // Creamos un array para almacenar los coches
    private var coches = ArrayBuffer[Coche]()

    // Método para añadir un coche al garaje
    def añadirCoche(coche: Coche): Unit = {
        coches += coche
    }

    // Método para eliminar un coche del garaje
    def eliminarCoche(coche: Coche): Unit = {
        coches -= coche
    }

    // Método para mostrar los coches del garaje
    def mostrarCoches(): Unit = {
        for (coche <- coches) {
            coche.mostrarInfo()
            println()
        }
    }
}

// Creamos un objeto de la clase Garaje
val garaje = new Garaje()

// Añadimos algunos coches al garaje
garaje.añadirCoche(new Coche("Audi", "A3", 2019))
garaje.añadirCoche(new Coche("BMW", "Serie 3", 2020))
garaje.añadirCoche(new Coche("Mercedes-Benz", "Clase C", 2021))

// Mostramos los coches del garaje
garaje.mostrarCoches()

// Eliminamos un coche del garaje
garaje.eliminarCoche(garaje.coches(0))

// Volvemos a mostrar los coches del garaje
garaje.mostrarCoches()
```

Este código crea una clase `Coche` que representa un coche con sus atributos `marca`, `modelo` y `año`. También incluye métodos para obtener y modificar estos atributos y un método para imprimir la información del coche.

Luego crea una clase `Garaje` que tiene un array de coches y métodos para añadir, eliminar y mostrar los coches del garaje.

Finalmente, crea un objeto de la clase `Garaje` y añade algunos coches al garaje. Luego muestra los coches del garaje, elimina uno de ellos y vuelve a mostrar los coches del garaje.