```scala
// DEFINICIÓN DE ALGUNOS TIPOS
sealed trait Figura
case class Circulo(radio: Double) extends Figura
case class Rectangulo(ancho: Double, alto: Double) extends Figura
case class Triangulo(lado1: Double, lado2: Double, lado3: Double) extends Figura

// DEFINICIÓN DE UNA CLASE QUE CALCULULA EL ÁREA DE UNA FIGURA
class CalculadorArea {
  def calcularArea(figura: Figura): Double = figura match {
    case Circulo(radio) => Math.PI * radio * radio
    case Rectangulo(ancho, alto) => ancho * alto
    case Triangulo(lado1, lado2, lado3) => {
      val semiperimetro = (lado1 + lado2 + lado3) / 2
      Math.sqrt(semiperimetro * (semiperimetro - lado1) * (semiperimetro - lado2) * (semiperimetro - lado3))
    }
  }
}

// DEFINICIÓN DE UNA FUNCIÓN QUE IMPRIME EL ÁREA DE UNA FIGURA
def imprimirArea(figura: Figura): Unit = {
  val calculadorArea = new CalculadorArea
  val area = calculadorArea.calcularArea(figura)
  println(s"El área de la figura es: $area")
}

// PROGRAMA PRINCIPAL
object Main {
  def main(args: Array[String]): Unit = {
    val circulo = Circulo(5.0)
    val rectangulo = Rectangulo(10.0, 20.0)
    val triangulo = Triangulo(3.0, 4.0, 5.0)

    imprimirArea(circulo)
    imprimirArea(rectangulo)
    imprimirArea(triangulo)
  }
}
```

Explicación del código:

1. Definición de tipos:

* Se definen tres clases selladas `Figura`, `Circulo`, `Rectangulo` y `Triangulo`.
* La clase `Figura` es una clase abstracta que representa una figura geométrica.
* Las clases `Circulo`, `Rectangulo` y `Triangulo` son clases concretas que heredan de la clase `Figura`.

2. Definición de una clase que calcula el área de una figura:

* Se define la clase `CalculadorArea` que tiene un método `calcularArea` que calcula el área de una figura.
* El método `calcularArea` recibe como parámetro una variable de tipo `Figura` y devuelve el área de la figura.
* El método `calcularArea` utiliza el patrón de coincidencia para calcular el área de la figura.

3. Definición de una función que imprime el área de una figura:

* Se define la función `imprimirArea` que recibe como parámetro una variable de tipo `Figura`.
* La función `imprimirArea` crea un objeto de la clase `CalculadorArea` y llama al método `calcularArea` para calcular el área de la figura.
* La función `imprimirArea` imprime el área de la figura en la consola.

4. Programa principal:

* Se definen tres variables de tipo `Figura`: `circulo`, `rectangulo` y `triangulo`.
* Se llama a la función `imprimirArea` para imprimir el área de cada una de las figuras.