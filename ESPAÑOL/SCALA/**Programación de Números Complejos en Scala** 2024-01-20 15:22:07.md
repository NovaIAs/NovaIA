```scala
// Importamos las librerías que necesitamos
import scala.collection.mutable.ArrayBuffer // Para crear arrays mutables
import scala.util.control.Breaks._ // Para usar el bucle "break"

// Creamos una clase llamada "Complejo" que representa un número complejo
class Complejo(val real: Double, val imaginario: Double) {

  // Sobrecargamos el operador "+" para sumar dos números complejos
  def +(otro: Complejo): Complejo = {
    new Complejo(real + otro.real, imaginario + otro.imaginario)
  }

  // Sobrecargamos el operador "-" para restar dos números complejos
  def -(otro: Complejo): Complejo = {
    new Complejo(real - otro.real, imaginario - otro.imaginario)
  }

  // Sobrecargamos el operador "*" para multiplicar dos números complejos
  def *(otro: Complejo): Complejo = {
    new Complejo(real * otro.real - imaginario * otro.imaginario,
      real * otro.imaginario + imaginario * otro.real)
  }

  // Sobrecargamos el operador "/" para dividir dos números complejos
  def /(otro: Complejo): Complejo = {
    val denominador = otro.real * otro.real + otro.imaginario * otro.imaginario
    new Complejo((real * otro.real + imaginario * otro.imaginario) / denominador,
      (imaginario * otro.real - real * otro.imaginario) / denominador)
  }

  // Sobrecargamos el método "toString" para imprimir un número complejo en formato "(real, imaginario)"
  override def toString: String = {
    "(" + real + ", " + imaginario + ")"
  }
}

// Creamos un objeto "main" para ejecutar el programa
object Main {
  def main(args: Array[String]): Unit = {

    // Creamos algunos números complejos
    val c1 = new Complejo(3, 4)
    val c2 = new Complejo(5, -2)

    // Los sumamos, restamos, multiplicamos y dividimos
    val c3 = c1 + c2
    val c4 = c1 - c2
    val c5 = c1 * c2
    val c6 = c1 / c2

    // Imprimimos los resultados
    println("c1 = " + c1)
    println("c2 = " + c2)
    println("c3 = " + c3)
    println("c4 = " + c4)
    println("c5 = " + c5)
    println("c6 = " + c6)

    // Comprobamos si dos números complejos son iguales
    if (c1 == c2) {
      println("c1 y c2 son iguales")
    } else {
      println("c1 y c2 son diferentes")
    }

    // Creamos un array de números complejos
    val array = ArrayBuffer[Complejo]()
    array += c1
    array += c2
    array += c3
    array += c4
    array += c5
    array += c6

    // Recorremos el array y lo imprimimos
    for (c <- array) {
      println(c)
    }

    // Usamos un bucle "break" para salir del bucle cuando se cumple una condición
    for (i <- 0 until 10) {
      if (i == 5) {
        break
      }
      println(i)
    }
  }
}
```

Este código está escrito en Scala y crea una clase llamada "Complejo" que representa un número complejo. La clase tiene dos propiedades: "real" e "imaginario".

El código también sobrecarga los operadores "+", "-", "*", "/" y "==" para que se puedan usar con números complejos.

El código también crea un objeto "main" que ejecuta el programa. El objeto "main" crea algunos números complejos y luego los suma, resta, multiplica, divide, compara e imprime.

El código también crea un array de números complejos y lo recorre e imprime.

Finalmente, el código usa un bucle "break" para salir del bucle cuando se cumple una condición.