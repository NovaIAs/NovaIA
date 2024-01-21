```scala
// IMPORTACIÓN DE LIBRERÍAS
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

// CLASE PRINCIPAL
object CodigoComplejo {

    // FUNCIÓN PRINCIPAL
    def main(args: Array[String]): Unit = {

        // CREACIÓN DE UN MAPA MUTABLE
        val mapa: Map[String, Int] = Map()

        // AÑADIR ELEMENTOS AL MAPA
        mapa += ("Uno" -> 1)
        mapa += ("Dos" -> 2)
        mapa += ("Tres" -> 3)

        // CREACIÓN DE UNA LISTA MUTABLE
        val lista: ListBuffer[String] = ListBuffer()

        // AÑADIR ELEMENTOS A LA LISTA
        lista += "Uno"
        lista += "Dos"
        lista += "Tres"

        // CREACIÓN DE UN ARRAY MUTABLE
        val array: ArrayBuffer[Int] = ArrayBuffer()

        // AÑADIR ELEMENTOS AL ARRAY
        array += 1
        array += 2
        array += 3

        // IMPRIMIR EL MAPA
        println("Mapa:")
        for ((clave, valor) <- mapa) {
            println(s"$clave -> $valor")
        }

        // IMPRIMIR LA LISTA
        println("Lista:")
        for (elemento <- lista) {
            println(elemento)
        }

        // IMPRIMIR EL ARRAY
        println("Array:")
        for (elemento <- array) {
            println(elemento)
        }

        // FUNCIÓN ANÓNIMA
        val funcionAnonima: (Int, Int) => Int = (x, y) => x + y

        // LLAMADA A LA FUNCIÓN ANÓNIMA
        val resultado = funcionAnonima(1, 2)

        // IMPRIMIR EL RESULTADO
        println("Resultado de la función anónima:")
        println(resultado)

        // CLASE ANIDADA
        class ClaseAnidada {
            def metodoAnidado(): String = {
                return "Método anidado"
            }
        }

        // CREACIÓN DE UNA INSTANCIA DE LA CLASE ANIDADA
        val instanciaAnidada = new ClaseAnidada()

        // LLAMADA AL MÉTODO ANIDADADO
        val resultadoAnidado = instanciaAnidada.metodoAnidado()

        // IMPRIMIR EL RESULTADO
        println("Resultado del método anidado:")
        println(resultadoAnidado)
    }
}
```

**EXPLICACIÓN DEL CÓDIGO:**

* La primera línea importa las librerías necesarias para el código.
* La clase principal es `CodigoComplejo`.
* La función principal es `main`.
* Se crea un mapa mutable llamado `mapa` y se le añaden tres pares clave-valor.
* Se crea una lista mutable llamada `lista` y se le añaden tres elementos.
* Se crea un array mutable llamado `array` y se le añaden tres elementos.
* Se imprime el mapa, la lista y el array.
* Se define una función anónima que suma dos números y se llama a la función.
* Se imprime el resultado de la función anónima.
* Se define una clase anidada llamada `ClaseAnidada` que tiene un método llamado `metodoAnidado()`.
* Se crea una instancia de la clase anidada y se llama al método anidado.
* Se imprime el resultado del método anidado.