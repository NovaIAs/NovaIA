```kotlin
// Clase Abstracta Animal
abstract class Animal {
    abstract fun comer()
    abstract fun dormir()
}

// Clase Perro que hereda de Animal
class Perro : Animal() {
    override fun comer() {
        println("El perro come croquetas.")
    }

    override fun dormir() {
        println("El perro duerme en su caseta.")
    }

    fun ladrar() {
        println("El perro ladra.")
    }
}

// Clase Gato que hereda de Animal
class Gato : Animal() {
    override fun comer() {
        println("El gato come pienso.")
    }

    override fun dormir() {
        println("El gato duerme en una cesta.")
    }

    fun maullar() {
        println("El gato maúlla.")
    }
}

// Clase Pájaro que hereda de Animal
class Pájaro : Animal() {
    override fun comer() {
        println("El pájaro come semillas.")
    }

    override fun dormir() {
        println("El pájaro duerme en un nido.")
    }

    fun cantar() {
        println("El pájaro canta.")
    }
}

// Clase Main
fun main(args: Array<String>) {
    // Creamos un objeto de la clase Perro
    val perro = Perro()

    // Llamamos a los métodos comer(), dormir() y ladrar() del objeto perro
    perro.comer()
    perro.dormir()
    perro.ladrar()

    // Creamos un objeto de la clase Gato
    val gato = Gato()

    // Llamamos a los métodos comer(), dormir() y maullar() del objeto gato
    gato.comer()
    gato.dormir()
    gato.maullar()

    // Creamos un objeto de la clase Pájaro
    val pájaro = Pájaro()

    // Llamamos a los métodos comer(), dormir() y cantar() del objeto pájaro
    pájaro.comer()
    pájaro.dormir()
    pájaro.cantar()
}
```

Este código crea tres clases abstractas: Animal, Perro, Gato y Pájaro. Cada clase tiene tres métodos abstractos: comer(), dormir() y un método específico para cada clase: ladrar(), maullar() y cantar().

La clase Main crea objetos de cada una de las clases y llama a los métodos correspondientes para demostrar el polimorfismo en Kotlin.

El polimorfismo es una característica del lenguaje de programación que permite que objetos de diferentes clases respondan al mismo mensaje de manera diferente. Esto se logra mediante el uso de la herencia y la sobrecarga de métodos.

En el código anterior, las clases Perro, Gato y Pájaro heredan de la clase Animal. Esto significa que los objetos de estas clases tienen acceso a los métodos comer() y dormir() definidos en la clase Animal.

Además, las clases Perro, Gato y Pájaro definen sus propios métodos específicos: ladrar(), maullar() y cantar(). Esto significa que los objetos de estas clases pueden responder al mensaje "hacer ruido" de manera diferente.

Cuando el código llama al método hacerRuido() en los objetos perro, gato y pájaro, el compilador determina el tipo específico del objeto y llama al método correspondiente. Esto demuestra el polimorfismo en Kotlin.