```
// Define una clase de objeto abstracta llamada `Operaciones`
// Esta clase incluye una función abstracta llamada `calcular`.
abstract class Operaciones {
  def calcular(a: Double, b: Double): Double
}

// Define una clase concreta que extiende la clase `Operaciones`.
// Esta clase implementa la función `calcular` para realizar la suma.
class Suma extends Operaciones {
  override def calcular(a: Double, b: Double): Double = a + b
}

// Define una clase concreta que extiende la clase `Operaciones`.
// Esta clase implementa la función `calcular` para realizar la resta.
class Resta extends Operaciones {
  override def calcular(a: Double, b: Double): Double = a - b
}

// Define una clase concreta que extiende la clase `Operaciones`.
// Esta clase implementa la función `calcular` para realizar la multiplicación.
class Multiplicacion extends Operaciones {
  override def calcular(a: Double, b: Double): Double = a * b
}

// Define una clase concreta que extiende la clase `Operaciones`.
// Esta clase implementa la función `calcular` para realizar la división.
class Division extends Operaciones {
  override def calcular(a: Double, b: Double): Double = a / b
}

// Define una función genérica llamada `realizarOperacion`.
// Esta función acepta un objeto de tipo `Operaciones` y dos números dobles.
// Ejecuta la operación especificada por el objeto `Operaciones` en los dos números.
def realizarOperacion(operacion: Operaciones, a: Double, b: Double): Double = {
  operacion.calcular(a, b)
}

// Define una función principal.
def main(args: Array[String]) {
  println("Bienvenidos a la calculadora")

  // Crea objetos de las diferentes operaciones.
  val suma = new Suma
  val resta = new Resta
  val multiplicacion = new Multiplicacion
  val division = new Division

  // Realiza la calculadora con los diferentes métodos.
  val resultadoSuma = realizarOperacion(suma, 10, 5)
  val resultadoResta = realizarOperacion(resta, 10, 5)
  val resultadoMultiplicacion = realizarOperacion(multiplicacion, 10, 5)
  val resultadoDivision = realizarOperacion(division, 10, 5)

  // Imprime los resultados de las operaciones.
  println("El resultado de la suma es: " + resultadoSuma)
  println("El resultado de la resta es: " + resultadoResta)
  println("El resultado de la multiplicación es: " + resultadoMultiplicacion)
  println("El resultado de la división es: " + resultadoDivision)
}
```

Explicación del código:

* El código define una clase abstracta llamada `Operaciones` que representa una operación matemática.
* Se definen cuatro clases concretas que extienden la clase `Operaciones` para implementar cada una de las operaciones matemáticas básicas: suma, resta, multiplicación y división.
* Se define una función genérica llamada `realizarOperacion` que acepta un objeto de tipo `Operaciones` y dos números dobles. Esta función ejecuta la operación especificada por el objeto `Operaciones` en los dos números.
* La función principal `main` crea objetos de las diferentes operaciones y llama a la función `realizarOperacion` para realizar las operaciones y mostrar los resultados.