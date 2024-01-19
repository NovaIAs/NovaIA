```scala
// Definición de la clase Complejo
class Complejo(real: Double, imaginario: Double) {

  // Propiedades
  def realParte = real
  def imaginarioParte = imaginario

  // Suma de dos complejos
  def +(otro: Complejo): Complejo = {
    new Complejo(real + otro.real, imaginario + otro.imaginario)
  }

  // Resta de dos complejos
  def -(otro: Complejo): Complejo = {
    new Complejo(real - otro.real, imaginario - otro.imaginario)
  }

  // Multiplicación de dos complejos
  def *(otro: Complejo): Complejo = {
    new Complejo(
      real * otro.real - imaginario * otro.imaginario,
      real * otro.imaginario + imaginario * otro.real
    )
  }

  // División de dos complejos
  def /(otro: Complejo): Complejo = {
    val denominador = otro.real * otro.real + otro.imaginario * otro.imaginario
    new Complejo(
      (real * otro.real + imaginario * otro.imaginario) / denominador,
      (imaginario * otro.real - real * otro.imaginario) / denominador
    )
  }

  // Módulo del complejo
  def modulo: Double = {
    Math.sqrt(real * real + imaginario * imaginario)
  }

  // Argumento del complejo
  def argumento: Double = {
    Math.atan2(imaginario, real)
  }

  // Conjugado del complejo
  def conjugado: Complejo = {
    new Complejo(real, -imaginario)
  }

  // Representación en cadena del complejo
  override def toString: String = {
    s"$real + ($imaginario)i"
  }
}

// Ejemplo de uso
val complejo1 = new Complejo(3, 4)
val complejo2 = new Complejo(5, -2)

println(s"Complejo 1: $complejo1")
println(s"Complejo 2: $complejo2")

println(s"Suma: ${complejo1 + complejo2}")
println(s"Resta: ${complejo1 - complejo2}")
println(s"Multiplicación: ${complejo1 * complejo2}")
println(s"División: ${complejo1 / complejo2}")

println(s"Módulo complejo 1: ${complejo1.modulo}")
println(s"Argumento complejo 1: ${complejo1.argumento}")

println(s"Conjugado complejo 1: ${complejo1.conjugado}")
```

Explicación del código:

* Definimos la clase `Complejo` con dos propiedades, `realParte` e `imaginarioParte`, que representan la parte real e imaginaria del complejo respectivamente.

* Definimos cuatro métodos para realizar las operaciones básicas de suma, resta, multiplicación y división de dos complejos.

* Definimos el método `modulo` para calcular el módulo del complejo, que es la raíz cuadrada de la suma de los cuadrados de la parte real y la parte imaginaria.

* Definimos el método `argumento` para calcular el argumento del complejo, que es el ángulo que forma el complejo con el eje real.

* Definimos el método `conjugado` para calcular el conjugado del complejo, que es el complejo que tiene la misma parte real y la parte imaginaria opuesta.

* Por último, definimos el método `toString` para obtener una representación en cadena del complejo en el formato "parte real + (parte imaginaria)i".

* En la parte final del código, creamos dos objetos de la clase `Complejo` y realizamos varias operaciones con ellos, mostrando los resultados en la consola.