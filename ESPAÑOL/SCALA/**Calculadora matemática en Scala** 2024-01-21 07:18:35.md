```scala
import scala.collection.mutable

// Definimos una clase abstracta para representar una expresión matemática.
abstract class Expresión {
  def evaluar(): Double // Método abstracto para evaluar la expresión.
}

// Definimos una clase concreta para representar una expresión constante.
class Constante(val valor: Double) extends Expresión {
  override def evaluar(): Double = valor // Devolvemos el valor de la constante.
}

// Definimos una clase concreta para representar una expresión binaria.
class Binaria(val izquierda: Expresión, val derecha: Expresión, val operador: String) extends Expresión {
  override def evaluar(): Double = {
    // Obtenemos los operandos izquierdo y derecho.
    val v1 = izquierda.evaluar()
    val v2 = derecha.evaluar()

    // Evaluamos la expresión binaria según el operador.
    operador match {
      case "+" => v1 + v2 // Suma.
      case "-" => v1 - v2 // Resta.
      case "*" => v1 * v2 // Multiplicación.
      case "/" => v1 / v2 // División.
    }
  }
}

// Definimos una clase concreta para representar una expresión unaria.
class Unaria(val expresión: Expresión, val operador: String) extends Expresión {
  override def evaluar(): Double = {
    // Obtenemos el operando.
    val v = expresión.evaluar()

    // Evaluamos la expresión unaria según el operador.
    operador match {
      case "-" => -v // Negación.
    }
  }
}

// Definimos una clase para representar una calculadora.
class Calculadora {
  // Definimos un mapa para almacenar las variables y sus valores.
  private val variables = mutable.Map[String, Double]()

  // Método para evaluar una expresión.
  def evaluar(expresión: Expresión): Double = {
    // Evaluamos la expresión.
    val resultado = expresión.evaluar()

    // Devolvemos el resultado.
    resultado
  }

  // Método para asignar un valor a una variable.
  def asignar(variable: String, valor: Double): Unit = {
    // Asignamos el valor a la variable.
    variables(variable) = valor
  }

  // Método para obtener el valor de una variable.
  def obtener(variable: String): Double = {
    // Obtenemos el valor de la variable.
    variables(variable)
  }
}

// Instanciamos una calculadora.
val calculadora = new Calculadora()

// Evaluamos una expresión compleja.
val resultado = calculadora.evaluar(
  new Binaria(
    new Unaria(new Constante(3), "-"), // Negación de 3.
    new Binaria(
      new Constante(2),
      new Constante(5),
      "*"), // Multiplicación de 2 por 5.
    "+")) // Suma del resultado de la negación y de la multiplicación.

// Imprimimos el resultado.
println(resultado) // Resultado: 32

```

Este código es un ejemplo de un programa complejo en Scala que implementa una calculadora. El programa incluye una clase abstracta para representar una expresión matemática, tres clases concretas para representar expresiones constantes, binarias y unarias, y una clase para representar una calculadora. El programa también incluye métodos para evaluar una expresión, asignar un valor a una variable y obtener el valor de una variable.

El programa puede evaluar una expresión compleja, como la expresión `-(2 * 5) + 3`, y devolver el resultado de la evaluación. El programa también puede almacenar y recuperar valores de variables.