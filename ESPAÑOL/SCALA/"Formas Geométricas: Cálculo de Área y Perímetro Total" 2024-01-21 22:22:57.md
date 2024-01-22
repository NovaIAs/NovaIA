```scala
// Definimos una clase abstracta "Forma" que sirve como base para otras formas geométricas.
abstract class Forma {
  // Método abstracto para calcular el perímetro de la forma.
  def perimetro: Double

  // Método abstracto para calcular el área de la forma.
  def area: Double
}

// Definimos una clase "Rectángulo" que hereda de "Forma".
class Rectángulo(val ancho: Double, val alto: Double) extends Forma {
  // Implementamos el método "perímetro" para un rectángulo.
  override def perimetro: Double = 2 * (ancho + alto)

  // Implementamos el método "área" para un rectángulo.
  override def area: Double = ancho * alto
}

// Definimos una clase "Círculo" que hereda de "Forma".
class Círculo(val radio: Double) extends Forma {
  // Implementamos el método "perímetro" para un círculo.
  override def perimetro: Double = 2 * Math.PI * radio

  // Implementamos el método "área" para un círculo.
  override def area: Double = Math.PI * radio * radio
}

// Definimos una función para calcular el área total de una lista de formas.
def areaTotal(formas: List[Forma]): Double = {
  // Utilizamos la función "foldLeft" para sumar el área de cada forma en la lista.
  formas.foldLeft(0.0)((acc, forma) => acc + forma.area)
}

// Definimos una función para calcular el perímetro total de una lista de formas.
def perimetroTotal(formas: List[Forma]): Double = {
  // Utilizamos la función "foldLeft" para sumar el perímetro de cada forma en la lista.
  formas.foldLeft(0.0)((acc, forma) => acc + forma.perímetro)
}

// Definimos una lista de formas para probar nuestras funciones.
val formas = List(
  new Rectángulo(5.0, 10.0),
  new Círculo(3.0),
  new Rectángulo(2.0, 4.0)
)

// Imprimimos el área total y el perímetro total de la lista de formas.
println(s"Área total: ${areaTotal(formas)}")
println(s"Perímetro total: ${perímetroTotal(formas)}")
```

Explicación del código:

* Definimos una clase abstracta `Forma` que sirve como base para otras formas geométricas.
* Definimos dos clases concretas `Rectángulo` y `Círculo` que heredan de `Forma`.
* Implementamos los métodos `perímetro` y `área` para cada clase concreta.
* Definimos dos funciones `areaTotal` y `perímetroTotal` para calcular el área total y el perímetro total de una lista de formas.
* Creamos una lista de formas para probar nuestras funciones.
* Imprimimos el área total y el perímetro total de la lista de formas.