```scala
// Definición de tipos personalizados

// Clase Persona con propiedades nombre, edad y género.
class Persona(nombre: String, edad: Int, genero: String) {
  // Métodos para obtener y establecer el valor de las propiedades.
  def getNombre = nombre
  def setNombre(nuevoNombre: String) = { nombre = nuevoNombre }

  def getEdad = edad
  def setEdad(nuevaEdad: Int) = { edad = nuevaEdad }

  def getGenero = genero
  def setGenero(nuevoGenero: String) = { genero = nuevoGenero }

  // Método para imprimir información de la persona.
  def imprimirInformacion() = {
    println(f"Nombre: ${getNombre}%s")
    println(f"Edad: ${getEdad}%d")
    println(f"Género: ${getGenero}%s")
  }
}

// Clase Rectangulo con propiedades longitud y anchura.
class Rectangulo(longitud: Double, anchura: Double) {
  // Métodos para obtener y establecer el valor de las propiedades.
  def getLongitud = longitud
  def setLongitud(nuevaLongitud: Double) = { longitud = nuevaLongitud }

  def getAnchura = anchura
  def setAnchura(nuevaAnchura: Double) = { anchura = nuevaAnchura }

  // Método para calcular el área del rectángulo.
  def calcularArea(): Double = {
    longitud * anchura
  }
}

// Clase Círculo con propiedad radio.
class Circulo(radio: Double) {
  // Métodos para obtener y establecer el valor de la propiedad.
  def getRadio = radio
  def setRadio(nuevoRadio: Double) = { radio = nuevoRadio }

  // Método para calcular el área del círculo.
  def calcularArea(): Double = {
    Math.PI * radio * radio
  }
}

// Definición de funciones

// Función para imprimir un saludo.
def saludar(nombre: String) = {
  println(f"Hola, ${nombre}%s!")
}

// Función para calcular el máximo de dos números.
def max(x: Int, y: Int): Int = {
  if (x > y) x else y
}

// Función para calcular el factorial de un número.
def factorial(n: Int): Int = {
  if (n == 0) 1 else n * factorial(n - 1)
}

// Programa principal

// Crear objetos de las clases Persona, Rectangulo y Circulo.
val persona1 = new Persona("Juan", 30, "Masculino")
val rectangulo1 = new Rectangulo(5.0, 10.0)
val circulo1 = new Circulo(3.0)

// Imprimir información de los objetos creados.
persona1.imprimirInformacion()
println(f"Área del rectángulo: ${rectangulo1.calcularArea()}%f")
println(f"Área del círculo: ${circulo1.calcularArea()}%f")

// Llamar a las funciones definidas anteriormente.
saludar("María")
println(f"Máximo entre 5 y 10: ${max(5, 10)}")
println(f"Factorial de 5: ${factorial(5)}")
```

Explicación del código:

* Se definen tres clases personalizadas: `Persona`, `Rectangulo` y `Circulo`. Cada clase tiene sus propias propiedades y métodos.
* Se definen tres funciones: `saludar`, `max` y `factorial`. Cada función tiene su propio propósito y toma diferentes parámetros.
* En el programa principal, se crean objetos de las clases `Persona`, `Rectangulo` y `Circulo`.
* Se llama a los métodos de los objetos creados para imprimir información sobre ellos.
* Se llaman a las funciones definidas anteriormente para realizar cálculos y mostrar resultados.

Este código es un ejemplo de cómo se pueden definir y utilizar tipos personalizados y funciones en Scala. Es un código complejo que demuestra diferentes características del lenguaje.