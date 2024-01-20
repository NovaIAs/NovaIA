```scala
// Clase que representa un punto en un espacio bidimensional.
class Punto(val x: Double, val y: Double) {
  // Método para obtener la distancia entre dos puntos.
  def distancia(otroPunto: Punto): Double = {
    val dx = x - otroPunto.x
    val dy = y - otroPunto.y
    Math.sqrt(dx * dx + dy * dy)
  }

  // Método para obtener un nuevo punto que es la suma de este punto y otro punto.
  def +(otroPunto: Punto): Punto = {
    new Punto(x + otroPunto.x, y + otroPunto.y)
  }

  // Método para obtener un nuevo punto que es la resta de este punto y otro punto.
  def -(otroPunto: Punto): Punto = {
    new Punto(x - otroPunto.x, y - otroPunto.y)
  }

  // Método para obtener la representación en cadena de este punto.
  override def toString: String = {
    "(" + x + ", " + y + ")"
  }
}

// Clase que representa una línea en un espacio bidimensional.
class Linea(val punto1: Punto, val punto2: Punto) {
  // Método para obtener la longitud de la línea.
  def longitud: Double = {
    punto1.distancia(punto2)
  }

  // Método para obtener un nuevo punto que es el punto medio de la línea.
  def puntoMedio: Punto = {
    (punto1 + punto2) / 2
  }

  // Método para obtener la pendiente de la línea.
  def pendiente: Double = {
    (punto2.y - punto1.y) / (punto2.x - punto1.x)
  }

  // Método para obtener la ordenada en el origen de la línea.
  def ordenadaEnOrigen: Double = {
    punto1.y - pendiente * punto1.x
  }

  // Método para obtener la representación en cadena de esta línea.
  override def toString: String = {
    "Línea de " + punto1 + " a " + punto2
  }
}

// Clase que representa un círculo en un espacio bidimensional.
class Circulo(val centro: Punto, val radio: Double) {
  // Método para obtener el área del círculo.
  def area: Double = {
    Math.PI * radio * radio
  }

  // Método para obtener el perímetro del círculo.
  def perimetro: Double = {
    2 * Math.PI * radio
  }

  // Método para obtener un nuevo círculo que es la unión de este círculo y otro círculo.
  def union(otroCirculo: Circulo): Circulo = {
    val nuevoCentro = (centro + otroCirculo.centro) / 2
    val nuevoRadio = centro.distancia(nuevoCentro) + radio
    new Circulo(nuevoCentro, nuevoRadio)
  }

  // Método para obtener la representación en cadena de este círculo.
  override def toString: String = {
    "Círculo con centro en " + centro + " y radio " + radio
  }
}

// Función principal.
object Main {
  def main(args: Array[String]): Unit = {
    // Se crean dos puntos.
    val punto1 = new Punto(1, 2)
    val punto2 = new Punto(3, 4)

    // Se crea una línea a partir de los dos puntos.
    val linea = new Linea(punto1, punto2)

    // Se imprimen la longitud, el punto medio, la pendiente y la ordenada en el origen de la línea.
    println("Longitud de la línea: " + linea.longitud)
    println("Punto medio de la línea: " + linea.puntoMedio)
    println("Pendiente de la línea: " + linea.pendiente)
    println("Ordenada en el origen de la línea: " + linea.ordenadaEnOrigen)

    // Se crea un círculo a partir de un punto y un radio.
    val circulo = new Circulo(punto1, 5)

    // Se imprimen el área, el perímetro y el centro del círculo.
    println("Área del círculo: " + circulo.area)
    println("Perímetro del círculo: " + circulo.perimetro)
    println("Centro del círculo: " + circulo.centro)

    // Se crean dos círculos.
    val circulo1 = new Circulo(punto1, 3)
    val circulo2 = new Circulo(punto2, 4)

    // Se crea un nuevo círculo que es la unión de los dos círculos anteriores.
    val circuloUnion = circulo1.union(circulo2)

    // Se imprime el centro y el radio del nuevo círculo.
    println("Centro del círculo unión: " + circuloUnion.centro)
    println("Radio del círculo unión: " + circuloUnion.radio)
  }
}
```

Este código crea dos clases, `Punto`, `Linea` y `Circulo`, que representan puntos, líneas y círculos en un espacio bidimensional. La clase `Punto` tiene dos propiedades: `x` e `y`, que representan las coordenadas del punto. La clase `Linea` tiene dos propiedades: `punto1` y `punto2`, que representan los dos puntos que definen la línea. La clase `Circulo` tiene dos propiedades: `centro` y `radio`, que representan el centro y el radio del círculo.

El código también define una función principal, `main`, que crea una serie de puntos, líneas y círculos. Luego, imprime la información de estos objetos, como su longitud, punto medio, pendiente, ordenada en el origen, área, perímetro y centro.

Por último, el código define una función `union`, que devuelve un nuevo círculo que es la unión de dos círculos dados. Esta función se utiliza para crear un nuevo círculo que es la unión de los dos círculos creados en la función principal.