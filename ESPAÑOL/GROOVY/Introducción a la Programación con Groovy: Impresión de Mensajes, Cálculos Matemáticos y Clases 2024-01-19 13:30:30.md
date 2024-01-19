```groovy
// Definición de una función para imprimir un mensaje
def imprimirMensaje(String mensaje) {
  println "El mensaje es: ${mensaje}"
}

// Definición de una clase Persona con propiedades nombre y edad
class Persona {
  String nombre
  int edad

  // Constructor de la clase Persona
  Persona(String nombre, int edad) {
    this.nombre = nombre
    this.edad = edad
  }

  // Método de la clase Persona para imprimir el nombre y la edad
  def imprimirNombreYEdad() {
    println "Nombre: ${nombre}, Edad: ${edad}"
  }
}

// Definición de una lista de personas
def personas = [
  new Persona("Juan", 20),
  new Persona("María", 25),
  new Persona("Pedro", 30),
  new Persona("Ana", 35)
]

// Iteración sobre la lista de personas e impresión del nombre y la edad de cada una
personas.each { persona ->
  persona.imprimirNombreYEdad()
}

// Definición de una función para calcular el área de un círculo
def calcularAreaCirculo(double radio) {
  Math.PI * radio ** 2
}

// Impresión del área de un círculo con radio 5
println "Área del círculo con radio 5: ${calcularAreaCirculo(5)}"

// Definición de una función para convertir una temperatura de Celsius a Fahrenheit
def convertirCelsiusAFahrenheit(double temperaturaCelsius) {
  temperaturaCelsius * 9 / 5 + 32
}

// Impresión de la temperatura convertida de 20 Celsius a Fahrenheit
println "20 Celsius en Fahrenheit: ${convertirCelsiusAFahrenheit(20)}"

// Definición de una clase Animal con propiedades nombre y tipo
class Animal {
  String nombre
  String tipo

  // Constructor de la clase Animal
  Animal(String nombre, String tipo) {
    this.nombre = nombre
    this.tipo = tipo
  }

  // Método de la clase Animal para imprimir el nombre y el tipo
  def imprimirNombreYTipo() {
    println "Nombre: ${nombre}, Tipo: ${tipo}"
  }
}

// Definición de una lista de animales
def animales = [
  new Animal("Perro", "Mamífero"),
  new Animal("Gato", "Mamífero"),
  new Animal("Pájaro", "Ave"),
  new Animal("Pez", "Pez")
]

// Iteración sobre la lista de animales e impresión del nombre y el tipo de cada uno
animales.each { animal ->
  animal.imprimirNombreYTipo()
}

// Definición de una clase Coche con propiedades marca, modelo y año
class Coche {
  String marca
  String modelo
  int año

  // Constructor de la clase Coche
  Coche(String marca, String modelo, int año) {
    this.marca = marca
    this.modelo = modelo
    this.año = año
  }

  // Método de la clase Coche para imprimir la marca, el modelo y el año
  def imprimirMarcaModeloAño() {
    println "Marca: ${marca}, Modelo: ${modelo}, Año: ${año}"
  }
}

// Definición de una lista de coches
def coches = [
  new Coche("Toyota", "Corolla", 2020),
  new Coche("Honda", "Civic", 2021),
  new Coche("Tesla", "Model 3", 2022),
  new Coche("Ford", "Mustang", 2023)
]

// Iteración sobre la lista de coches e impresión de la marca, el modelo y el año de cada uno
coches.each { coche ->
  coche.imprimirMarcaModeloAño()
}

```
Explicación del código:

* Se define una función `imprimirMensaje` que recibe un mensaje como parámetro y lo imprime en la consola.
* Se define una clase `Persona` con las propiedades `nombre` y `edad`.
* Se crea una lista de personas con algunas instancias de la clase `Persona`.
* Se itera sobre la lista de personas e imprime el nombre y la edad de cada persona.
* Se define una función `calcularAreaCirculo` que calcula el área de un círculo dado su radio.
* Se imprime el área de un círculo con radio 5.
* Se define una función `convertirCelsiusAFahrenheit` que convierte una temperatura de Celsius a Fahrenheit.
* Se imprime la temperatura convertida de 20 Celsius a Fahrenheit.
* Se define una clase `Animal` con las propiedades `nombre` y `tipo`.
* Se crea una lista de animales con algunas instancias de la clase `Animal`.
* Se itera sobre la lista de animales e imprime el nombre y el tipo de cada animal.
* Se define una clase `Coche` con las propiedades `marca`, `modelo` y `año`.
* Se crea una lista de coches con algunas instancias de la clase `Coche`.
* Se itera sobre la lista de coches e imprime la marca, el modelo y el año de cada coche.