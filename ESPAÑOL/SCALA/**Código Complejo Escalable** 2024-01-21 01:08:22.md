```scala
package aplicacion

import scala.util.{Random, Try}

object CodigoComplejo {

  // Definición de una clase con constructor y métodos
  class Persona(nombre: String, edad: Int) {
    def saludar(): String = s"Hola, soy $nombre y tengo $edad años."
  }

  // Función que devuelve un número aleatorio entre dos enteros
  def generarNumeroAleatorio(min: Int, max: Int): Int = Random.nextInt(max - min + 1) + min

  // Función que devuelve el máximo común divisor de dos enteros
  def maximoComunDivisor(a: Int, b: Int): Int = {
    if (b == 0) a else maximoComunDivisor(b, a % b)
  }

  // Función que intenta parsear una cadena a un entero, o devuelve un valor por defecto en caso de error
  def parsearEntero(cadena: String, valorPorDefecto: Int): Int = {
    Try(cadena.toInt).getOrElse(valorPorDefecto)
  }

  // Función que devuelve la lista de los factores primos de un entero
  def factoresPrimos(n: Int): List[Int] = {
    def factoresPrimosAux(n: Int, factores: List[Int]): List[Int] = {
      if (n == 1) factores
      else {
        val factor = generarNumeroAleatorio(2, n)
        if (n % factor == 0) factoresPrimosAux(n / factor, factor :: factores)
        else factoresPrimosAux(n, factores)
      }
    }

    factoresPrimosAux(n, Nil)
  }

  // Ejemplo de uso de las funciones definidas
  def main(args: Array[String]): Unit = {
    // Crear una persona
    val persona = new Persona("Juan", 25)

    // Obtener un número aleatorio
    val numeroAleatorio = generarNumeroAleatorio(1, 100)

    // Obtener el máximo común divisor de dos números
    val mcd = maximoComunDivisor(12345, 67890)

    // Parsear una cadena a un entero
    val entero = parsearEntero("123", 0)

    // Obtener los factores primos de un número
    val factoresPrimosNumero = factoresPrimos(12345)

    // Imprimir los resultados
    println(persona.saludar())
    println(numeroAleatorio)
    println(mcd)
    println(entero)
    println(factoresPrimosNumero)
  }
}
```

Este código define una clase `Persona` con constructor y métodos, una función `generarNumeroAleatorio` que devuelve un número aleatorio entre dos enteros, una función `maximoComunDivisor` que devuelve el máximo común divisor de dos enteros, una función `parsearEntero` que intenta parsear una cadena a un entero, y una función `factoresPrimos` que devuelve la lista de los factores primos de un entero.

El código también incluye un ejemplo de uso de las funciones definidas, que crea una persona, obtiene un número aleatorio, obtiene el máximo común divisor de dos números, parsea una cadena a un entero, y obtiene los factores primos de un número.