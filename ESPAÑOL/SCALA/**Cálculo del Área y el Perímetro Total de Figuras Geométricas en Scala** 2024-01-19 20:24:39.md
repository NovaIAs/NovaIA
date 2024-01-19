```scala
// Definimos una interfaz llamada "Figura" que representa una figura geométrica.
trait Figura {
  def area: Double
  def perimetro: Double
}

// Definimos una clase llamada "Circulo" que implementa la interfaz "Figura".
class Circulo(radio: Double) extends Figura {
  override def area: Double = math.Pi * radio * radio
  override def perimetro: Double = 2 * math.Pi * radio
}

// Definimos una clase llamada "Cuadrado" que implementa la interfaz "Figura".
class Cuadrado(lado: Double) extends Figura {
  override def area: Double = lado * lado
  override def perimetro: Double = 4 * lado
}

// Definimos una clase llamada "Rectangulo" que implementa la interfaz "Figura".
class Rectangulo(ancho: Double, alto: Double) extends Figura {
  override def area: Double = ancho * alto
  override def perimetro: Double = 2 * (ancho + alto)
}

// Definimos una clase llamada "Triangulo" que implementa la interfaz "Figura".
class Triangulo(base: Double, altura: Double) extends Figura {
  override def area: Double = (base * altura) / 2
  override def perimetro: Double = base + altura + math.sqrt(base * base + altura * altura)
}

// Definimos una función llamada "calcularArea" que recibe una lista de figuras y devuelve el área total de las figuras.
def calcularArea(figuras: List[Figura]): Double = {
  figuras.map(_.area).sum
}

// Definimos una función llamada "calcularPerimetro" que recibe una lista de figuras y devuelve el perímetro total de las figuras.
def calcularPerimetro(figuras: List[Figura]): Double = {
  figuras.map(_.perimetro).sum
}

// Creamos una lista de figuras.
val figuras = List(
  new Circulo(5.0),
  new Cuadrado(3.0),
  new Rectangulo(4.0, 6.0),
  new Triangulo(2.0, 4.0)
)

// Calculamos el área total de las figuras.
val areaTotal = calcularArea(figuras)

// Calculamos el perímetro total de las figuras.
val perimetroTotal = calcularPerimetro(figuras)

// Imprimimos el área total y el perímetro total de las figuras.
println(s"El área total de las figuras es: $areaTotal")
println(s"El perímetro total de las figuras es: $perimetroTotal")
```

Este código es una implementación en Scala de un programa que calcula el área total y el perímetro total de una lista de figuras geométricas. El programa define una interfaz llamada "Figura" que representa una figura geométrica y cuatro clases que implementan la interfaz: "Circulo", "Cuadrado", "Rectangulo" y "Triangulo".

El programa también define dos funciones: "calcularArea" y "calcularPerimetro". La función "calcularArea" recibe una lista de figuras y devuelve el área total de las figuras. La función "calcularPerimetro" recibe una lista de figuras y devuelve el perímetro total de las figuras.

El programa crea una lista de figuras y luego utiliza las funciones "calcularArea" y "calcularPerimetro" para calcular el área total y el perímetro total de las figuras. Finalmente, el programa imprime el área total y el perímetro total de las figuras.