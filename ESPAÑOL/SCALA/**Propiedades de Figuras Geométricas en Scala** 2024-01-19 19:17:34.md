```scala
// Definición de la clase "Circulo"
class Circulo(val radio: Double) {

  // Método para calcular el área del círculo
  def area: Double = {
    Math.PI * radio * radio
  }

  // Método para calcular la circunferencia del círculo
  def circunferencia: Double = {
    2 * Math.PI * radio
  }

  // Método para imprimir las propiedades del círculo
  def imprimirPropiedades(): Unit = {
    println(s"Radio: $radio")
    println(s"Área: $area")
    println(s"Circunferencia: $circunferencia")
  }
}

// Definición de la clase "Rectangulo"
class Rectangulo(val ancho: Double, val alto: Double) {

  // Método para calcular el área del rectángulo
  def area: Double = {
    ancho * alto
  }

  // Método para calcular el perímetro del rectángulo
  def perimetro: Double = {
    2 * (ancho + alto)
  }

  // Método para imprimir las propiedades del rectángulo
  def imprimirPropiedades(): Unit = {
    println(s"Ancho: $ancho")
    println(s"Alto: $alto")
    println(s"Área: $area")
    println(s"Perímetro: $perimetro")
  }
}

// Definición de la clase "Triangulo"
class Triangulo(val base: Double, val altura: Double) {

  // Método para calcular el área del triángulo
  def area: Double = {
    0.5 * base * altura
  }

  // Método para calcular el perímetro del triángulo
  def perimetro: Double = {
    base + altura + Math.sqrt(base * base + altura * altura)
  }

  // Método para imprimir las propiedades del triángulo
  def imprimirPropiedades(): Unit = {
    println(s"Base: $base")
    println(s"Altura: $altura")
    println(s"Área: $area")
    println(s"Perímetro: $perimetro")
  }
}

// Función principal
object Main {

  // Método para crear una lista de formas geométricas
  def crearFormas(): List[FormaGeometrica] = {
    List(
      new Circulo(5.0),
      new Rectangulo(10.0, 20.0),
      new Triangulo(15.0, 25.0)
    )
  }

  // Método para imprimir las propiedades de una lista de formas geométricas
  def imprimirFormas(formas: List[FormaGeometrica]): Unit = {
    for (forma <- formas) {
      forma.imprimirPropiedades()
      println()
    }
  }

  // Punto de entrada del programa
  def main(args: Array[String]): Unit = {

    // Crear una lista de formas geométricas
    val formas = crearFormas()

    // Imprimir las propiedades de la lista de formas geométricas
    imprimirFormas(formas)
  }
}
```

Explicación:

* El código define tres clases: `Circulo`, `Rectangulo` y `Triangulo`, que representan formas geométricas básicas. Cada clase tiene propiedades y métodos específicos para calcular el área, la circunferencia o el perímetro de la forma geométrica correspondiente.
* La clase `FormaGeometrica` es una clase base abstracta que define una interfaz común para todas las formas geométricas. Esta clase tiene un método abstracto `area` y un método abstracto `perimetro`.
* La clase `Circulo` hereda de la clase `FormaGeometrica` e implementa los métodos `area` y `perimetro` para calcular el área y la circunferencia de un círculo, respectivamente.
* La clase `Rectangulo` hereda de la clase `FormaGeometrica` e implementa los métodos `area` y `perimetro` para calcular el área y el perímetro de un rectángulo, respectivamente.
* La clase `Triangulo` hereda de la clase `FormaGeometrica` e implementa los métodos `area` y `perimetro` para calcular el área y el perímetro de un triángulo, respectivamente.
* La clase `Main` contiene el punto de entrada del programa. Esta clase define un método `crearFormas()` para crear una lista de formas geométricas y un método `imprimirFormas()` para imprimir las propiedades de una lista de formas geométricas.
* El método `main()` de la clase `Main` crea una lista de formas geométricas utilizando el método `crearFormas()`, e imprime las propiedades de la lista de formas geométricas utilizando el método `imprimirFormas()`.