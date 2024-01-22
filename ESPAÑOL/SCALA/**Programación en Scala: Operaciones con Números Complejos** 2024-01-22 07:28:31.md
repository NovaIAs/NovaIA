```scala
import scala.util.Random

object ComplejoScala {

  // Definimos una clase compleja con atributos reales e imaginarios.
  case class Complejo(real: Double, imaginario: Double) {

    // Definimos los métodos básicos de suma, resta, multiplicación y división.
    def +(otro: Complejo): Complejo = Complejo(real + otro.real, imaginario + otro.imaginario)
    def -(otro: Complejo): Complejo = Complejo(real - otro.real, imaginario - otro.imaginario)
    def *(otro: Complejo): Complejo = Complejo(real * otro.real - imaginario * otro.imaginario, real * otro.imaginario + imaginario * otro.real)
    def /(otro: Complejo): Complejo = {
      val denominador = otro.real * otro.real + otro.imaginario * otro.imaginario
      Complejo((real * otro.real + imaginario * otro.imaginario) / denominador, (imaginario * otro.real - real * otro.imaginario) / denominador)
    }

    // Definimos el método para obtener el módulo del complejo.
    def modulo: Double = Math.sqrt(real * real + imaginario * imaginario)

    // Definimos el método para obtener el ángulo del complejo.
    def angulo: Double = Math.atan2(imaginario, real)

    // Definimos el método para obtener la representación en cadena del complejo.
    override def toString: String = s"($real + ${imaginario}i)"
  }

  // Definimos una función para generar números complejos aleatorios.
  def generarComplejoAleatorio(minimo: Double, maximo: Double): Complejo = Complejo(Random.nextDouble() * (maximo - minimo) + minimo, Random.nextDouble() * (maximo - minimo) + minimo)

  // Definimos una función para calcular la raíz cuadrada de un número complejo.
  def raizCuadrada(complejo: Complejo): Complejo = {
    val modulo = complejo.modulo
    val angulo = complejo.angulo / 2
    Complejo(Math.sqrt(modulo) * Math.cos(angulo), Math.sqrt(modulo) * Math.sin(angulo))
  }

  // Definimos una función para calcular la potencia de un número complejo.
  def potencia(complejo: Complejo, exponente: Int): Complejo = {
    if (exponente == 0) {
      Complejo(1, 0)
    } else if (exponente == 1) {
      complejo
    } else if (exponente > 0) {
      complejo * potencia(complejo, exponente - 1)
    } else {
      1 / potencia(complejo, -exponente)
    }
  }

  // Definimos una función para calcular el seno de un número complejo.
  def seno(complejo: Complejo): Complejo = {
    val exponente = Complejo(0, 1)
    (potencia(exponente, 3) * complejo - potencia(exponente, 5) * potencia(complejo, 3) / 3! + potencia(exponente, 7) * potencia(complejo, 5) / 5! - ...) / exponente
  }

  // Definimos una función para calcular el coseno de un número complejo.
  def coseno(complejo: Complejo): Complejo = {
    val exponente = Complejo(0, 1)
    (potencia(exponente, 2) * complejo^2 - potencia(exponente, 4) * potencia(complejo, 4) / 2! + potencia(exponente, 6) * potencia(complejo, 6) / 4! - ...)
  }

  // Definimos una función para calcular la tangente de un número complejo.
  def tangente(complejo: Complejo): Complejo = seno(complejo) / coseno(complejo)

  // Definimos una función para calcular el arcoseno de un número complejo.
  def arcoseno(complejo: Complejo): Complejo = -1i * logaritmoNatural(complejo + raizCuadrada(complejo^2 - 1))

  // Definimos una función para calcular el arcocoseno de un número complejo.
  def arcocoseno(complejo: Complejo): Complejo = -1i * logaritmoNatural(complejo + raizCuadrada(complejo^2 - 1))

  // Definimos una función para calcular la arcotangente de un número complejo.
  def arcotangente(complejo: Complejo): Complejo = -1i * logaritmoNatural((complejo - 1i) / (complejo + 1i))

  // Definimos una función para calcular el logaritmo natural de un número complejo.
  def logaritmoNatural(complejo: Complejo): Complejo = Complejo(Math.log(complejo.modulo), complejo.angulo)

  // Creamos algunos números complejos aleatorios.
  val complejo1 = generarComplejoAleatorio(-10, 10)
  val complejo2 = generarComplejoAleatorio(-10, 10)

  // Mostramos los números complejos aleatorios.
  println(s"Complejo 1: $complejo1")
  println(s"Complejo 2: $complejo2")

  // Sumamos los números complejos.
  val complejoSuma = complejo1 + complejo2

  // Mostramos el resultado de la suma.
  println(s"Suma: $complejoSuma")

  // Restamos los números complejos.
  val complejoResta = complejo1 - complejo2

  // Mostramos el resultado de la resta.
  println(s"Resta: $complejoResta")

  // Multiplicamos los números complejos.
  val complejoMultiplicacion = complejo1 * complejo2

  // Mostramos el resultado de la multiplicación.
  println(s"Multiplicación: $complejoMultiplicacion")

  // Dividimos los números complejos.
  val complejoDivision = complejo1 / complejo2

  // Mostramos el resultado de la división.
  println(s"División: $complejoDivision")

  // Calculamos el módulo de los números complejos.
  val moduloComplejo1 = complejo1.modulo
  val moduloComplejo2 = complejo2.modulo

  // Mostramos los módulos de los números complejos.
  println(s"Módulo Complejo 1: $moduloComplejo1")
  println(s"Módulo Complejo 2: $moduloComplejo2")

  // Calculamos el ángulo de los números complejos.
  val anguloComplejo1 = complejo1.angulo
  val anguloComplejo2 = complejo2.angulo

  // Mostramos los ángulos de los números complejos.
  println(s"Ángulo Complejo 1: $anguloComplejo1")
  println(s"Ángulo Complejo 2: $anguloComplejo2")

  // Calculamos la raíz cuadrada de los números complejos.
  val raizCuadradaComplejo1 = raizCuadrada(complejo1)
  val raizCuadradaComplejo2 = raizCuadrada(complejo2)

  // Mostramos las raíces cuadradas de los números complejos.
  println(s"Raíz Cuadrada Complejo 1: $raizCuadradaComplejo1")
  println(s"Raíz Cuadrada Complejo 2: $raizCuadradaComplejo2")

  // Calculamos la potencia de los números complejos.
  val potenciaComplejo1 = potencia(complejo1, 2)
  val potenciaComplejo2 = potencia(complejo2, 2)

  // Mostramos las potencias de los números complejos.
  println(s"Potencia Complejo 1: $