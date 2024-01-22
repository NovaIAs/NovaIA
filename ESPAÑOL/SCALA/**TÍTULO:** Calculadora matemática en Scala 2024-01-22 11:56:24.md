```scala
// Interface para operaciones matemáticas básicas
trait OperacionesMatematicas {
  def suma(a: Double, b: Double): Double
  def resta(a: Double, b: Double): Double
  def multiplicacion(a: Double, b: Double): Double
  def division(a: Double, b: Double): Double
}

// Clase que implementa la interfaz OperacionesMatematicas
class Calculadora implements OperacionesMatematicas {

  def suma(a: Double, b: Double): Double = {
    a + b
  }

  def resta(a: Double, b: Double): Double = {
    a - b
  }

  def multiplicacion(a: Double, b: Double): Double = {
    a * b
  }

  def division(a: Double, b: Double): Double = {
    a / b
  }
}

// Clase que utiliza la interfaz OperacionesMatematicas para realizar cálculos
class CalculadoraApp {

  def main(args: Array[String]): Unit = {
    val calculadora = new Calculadora()

    val resultadoSuma = calculadora.suma(10, 20)
    val resultadoResta = calculadora.resta(20, 10)
    val resultadoMultiplicacion = calculadora.multiplicacion(10, 20)
    val resultadoDivision = calculadora.division(20, 10)

    println("Resultado de la suma: " + resultadoSuma)
    println("Resultado de la resta: " + resultadoResta)
    println("Resultado de la multiplicación: " + resultadoMultiplicacion)
    println("Resultado de la división: " + resultadoDivision)
  }
}
```
**Explicación del código:**

1. **Definición de la interfaz OperacionesMatematicas:** Esta interfaz define los métodos necesarios para realizar operaciones matemáticas básicas como suma, resta, multiplicación y división.

2. **Definición de la clase Calculadora que implementa la interfaz OperacionesMatematicas:** Esta clase implementa la interfaz OperacionesMatematicas y proporciona su implementación para cada uno de los métodos definidos en la interfaz.

3. **Definición de la clase CalculadoraApp que utiliza la interfaz OperacionesMatematicas:** Esta clase utiliza la interfaz OperacionesMatematicas para realizar cálculos matemáticos. Crea una instancia de la clase Calculadora y utiliza sus métodos para realizar cálculos de suma, resta, multiplicación y división.

4. **Método main:** Este método es el punto de entrada del programa. Crea una instancia de la clase CalculadoraApp y llama a su método main para realizar los cálculos matemáticos.

5. **Métodos de la clase CalculadoraApp:** El método main de la clase CalculadoraApp crea una instancia de la clase Calculadora, realiza los cálculos matemáticos utilizando los métodos de la clase Calculadora y muestra los resultados en la consola.