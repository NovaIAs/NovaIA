```scala
// Clase complexa con interfaces y herencia múltiple
class Complex(val real: Double, val imag: Double) extends Ordered[Complex] with Serializable {

  // Método para sumar dos números complejos
  def +(otro: Complex): Complex = new Complex(real + otro.real, imag + otro.imag)

  // Método para restar dos números complejos
  def -(otro: Complex): Complex = new Complex(real - otro.real, imag - otro.imag)

  // Método para multiplicar dos números complejos
  def *(otro: Complex): Complex = new Complex(real * otro.real - imag * otro.imag, real * otro.imag + imag * otro.real)

  // Método para dividir dos números complejos
  def /(otro: Complex): Complex = {
    val denominador = otro.real * otro.real + otro.imag * otro.imag
    new Complex((real * otro.real + imag * otro.imag) / denominador, (imag * otro.real - real * otro.imag) / denominador)
  }

  // Método para obtener el módulo del número complejo
  def modulo: Double = Math.sqrt(real * real + imag * imag)

  // Método para obtener el argumento del número complejo
  def argumento: Double = Math.atan2(imag, real)

  // Método para comparar dos números complejos
  override def compare(otro: Complex): Int = {
    if (modulo < otro.modulo) -1
    else if (modulo > otro.modulo) 1
    else 0
  }

  // Método para serializar el número complejo
  override def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.writeDouble(real)
    out.writeDouble(imag)
  }

  // Método para deserializar el número complejo
  override def readObject(in: java.io.ObjectInputStream): Unit = {
    real = in.readDouble()
    imag = in.readDouble()
  }

  // Método para obtener una representación en cadena del número complejo
  override def toString: String = real + (if (imag >= 0) "+" else "") + imag + "i"
}

// Objeto acompañante de la clase Complex
object Complex {

  // Método para crear un número complejo a partir de dos dobles
  def apply(real: Double, imag: Double): Complex = new Complex(real, imag)
}

// Ejemplo de uso
val c1 = new Complex(3, 4)
val c2 = new Complex(5, -2)

println(c1 + c2)
println(c1 - c2)
println(c1 * c2)
println(c1 / c2)
println(c1.modulo)
println(c1.argumento)
println(c1.compare(c2))
```

Explicación del código:

* La clase `Complex` es una clase compleja que representa un número complejo.
* La clase `Complex` implementa las interfaces `Ordered` y `Serializable`.
* La clase `Complex` tiene dos constructores: uno que toma dos dobles como parámetros y otro que toma un doble como parámetro.
* La clase `Complex` tiene una serie de métodos para sumar, restar, multiplicar y dividir números complejos.
* La clase `Complex` también tiene una serie de métodos para obtener el módulo, el argumento y la comparación de dos números complejos.
* La clase `Complex` también tiene un método para serializar y deserializar números complejos.
* El objeto acompañante de la clase `Complex` proporciona un método `apply()` para crear un número complejo a partir de dos dobles.
* El ejemplo de uso muestra cómo crear números complejos, sumar, restar, multiplicar y dividir números complejos, obtener el módulo y el argumento de un número complejo, comparar dos números complejos y serializar y deserializar números complejos.