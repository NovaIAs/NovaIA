```scala
object ProgramaComplejo {

  // Clase que representa un punto en el espacio 3D
  case class Punto3D(x: Double, y: Double, z: Double) {
    def +(otro: Punto3D): Punto3D = Punto3D(x + otro.x, y + otro.y, z + otro.z)
    def -(otro: Punto3D): Punto3D = Punto3D(x - otro.x, y - otro.y, z - otro.z)
    def *(escalar: Double): Punto3D = Punto3D(x * escalar, y * escalar, z * escalar)
    def /(escalar: Double): Punto3D = Punto3D(x / escalar, y / escalar, z / escalar)

    override def toString: String = s"($x, $y, $z)"
  }

  // Clase que representa una línea en el espacio 3D
  case class Linea3D(punto1: Punto3D, punto2: Punto3D) {
    def longitud: Double = math.sqrt(math.pow(punto2.x - punto1.x, 2) + math.pow(punto2.y - punto1.y, 2) + math.pow(punto2.z - punto1.z, 2))

    override def toString: String = s"$punto1 -> $punto2"
  }

  // Clase que representa un triángulo en el espacio 3D
  case class Triangulo3D(punto1: Punto3D, punto2: Punto3D, punto3: Punto3D) {
    def area: Double = {
      val lado1 = Linea3D(punto1, punto2).longitud
      val lado2 = Linea3D(punto2, punto3).longitud
      val lado3 = Linea3D(punto3, punto1).longitud
      val semiperimetro = (lado1 + lado2 + lado3) / 2
      math.sqrt(semiperimetro * (semiperimetro - lado1) * (semiperimetro - lado2) * (semiperimetro - lado3))
    }

    override def toString: String = s"$punto1 -> $punto2 -> $punto3"
  }

  // Función principal
  def main(args: Array[String]): Unit = {
    // Creamos algunos puntos en el espacio 3D
    val punto1 = Punto3D(1, 2, 3)
    val punto2 = Punto3D(4, 5, 6)
    val punto3 = Punto3D(7, 8, 9)

    // Imprimimos los puntos
    println("Puntos:")
    println(punto1)
    println(punto2)
    println(punto3)

    // Creamos una línea en el espacio 3D
    val linea1 = Linea3D(punto1, punto2)

    // Imprimimos la línea
    println("Línea:")
    println(linea1)

    // Calculamos la longitud de la línea
    val longitudLinea1 = linea1.longitud

    // Imprimimos la longitud de la línea
    println("Longitud de la línea:")
    println(longitudLinea1)

    // Creamos un triángulo en el espacio 3D
    val triangulo1 = Triangulo3D(punto1, punto2, punto3)

    // Imprimimos el triángulo
    println("Triángulo:")
    println(triangulo1)

    // Calculamos el área del triángulo
    val areaTriangulo1 = triangulo1.area

    // Imprimimos el área del triángulo
    println("Área del triángulo:")
    println(areaTriangulo1)
  }
}
```

Explicación del código:

* La clase `Punto3D` representa un punto en el espacio 3D. Tiene tres atributos: `x`, `y` y `z`, que representan las coordenadas del punto en cada eje. También tiene varios métodos, como `+`, `-`, `*` y `/`, que permiten realizar operaciones aritméticas con puntos.
* La clase `Linea3D` representa una línea en el espacio 3D. Tiene dos atributos: `punto1` y `punto2`, que representan los dos puntos que forman la línea. También tiene un método llamado `longitud`, que devuelve la longitud de la línea.
* La clase `Triangulo3D` representa un triángulo en el espacio 3D. Tiene tres atributos: `punto1`, `punto2` y `punto3`, que representan los tres puntos que forman el triángulo. También tiene un método llamado `area`, que devuelve el área del triángulo.
* La función `main` es la función principal del programa. Crea algunos puntos, líneas y triángulos en el espacio 3D, y luego imprime sus propiedades, como la longitud de las líneas y el área de los triángulos.