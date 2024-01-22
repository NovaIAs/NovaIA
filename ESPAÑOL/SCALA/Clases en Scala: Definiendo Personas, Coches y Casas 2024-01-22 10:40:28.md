```
// Definir la clase Persona
class Persona(nombre: String, edad: Int) {
  var nombre: String = nombre
  var edad: Int = edad

  // Definir un método para obtener el nombre de la persona
  def getNombre(): String = {
    nombre
  }

  // Definir un método para obtener la edad de la persona
  def getEdad(): Int = {
    edad
  }

  // Definir un método para saludar
  def saludar(): Unit = {
    println("Hola, mi nombre es " + nombre + " y tengo " + edad + " años.")
  }
}

// Crear una instancia de la clase Persona
val persona1 = new Persona("Juan", 25)

// Obtener el nombre de la persona
val nombre = persona1.getNombre()

// Obtener la edad de la persona
val edad = persona1.getEdad()

// Saludar a la persona
persona1.saludar()

// Definir la clase Coche
class Coche(marca: String, modelo: String, matricula: String) {
  var marca: String = marca
  var modelo: String = modelo
  var matricula: String = matricula

  // Definir un método para obtener la marca del coche
  def getMarca(): String = {
    marca
  }

  // Definir un método para obtener el modelo del coche
  def getModelo(): String = {
    modelo
  }

  // Definir un método para obtener la matricula del coche
  def getMatricula(): String = {
    matricula
  }

  // Definir un método para conducir el coche
  def conducir(): Unit = {
    println("Estoy conduciendo el " + marca + " " + modelo + " con matricula " + matricula + ".")
  }
}

// Crear una instancia de la clase Coche
val coche1 = new Coche("Toyota", "Yaris", "ABC123")

// Obtener la marca del coche
val marca = coche1.getMarca()

// Obtener el modelo del coche
val modelo = coche1.getModelo()

// Obtener la matricula del coche
val matricula = coche1.getMatricula()

// Conducir el coche
coche1.conducir()

// Definir la clase Casa
class Casa(direccion: String, numeroHabitaciones: Int, numeroBaños: Int) {
  var direccion: String = direccion
  var numeroHabitaciones: Int = numeroHabitaciones
  var numeroBaños: Int = numeroBaños

  // Definir un método para obtener la direccion de la casa
  def getDireccion(): String = {
    direccion
  }

  // Definir un método para obtener el numero de habitaciones de la casa
  def getNumeroHabitaciones(): Int = {
    numeroHabitaciones
  }

  // Definir un método para obtener el numero de baños de la casa
  def getNumeroBaños(): Int = {
    numeroBaños
  }

  // Definir un método para vivir en la casa
  def vivir(): Unit = {
    println("Estoy viviendo en la casa en " + direccion + " que tiene " + numeroHabitaciones + " habitaciones y " + numeroBaños + " baños.")
  }
}

// Crear una instancia de la clase Casa
val casa1 = new Casa("Calle Mayor, 123", 3, 2)

// Obtener la direccion de la casa
val direccion = casa1.getDireccion()

// Obtener el numero de habitaciones de la casa
val numeroHabitaciones = casa1.getNumeroHabitaciones()

// Obtener el numero de baños de la casa
val numeroBaños = casa1.getNumeroBaños()

// Vivir en la casa
casa1.vivir()

```

**Explicación del código:**

El código define tres clases: `Persona`, `Coche` y `Casa`. Cada clase tiene sus propios atributos y métodos.

La clase `Persona` tiene dos atributos: `nombre` y `edad`. También tiene tres métodos: `getNombre()`, `getEdad()` y `saludar()`. El método `getNombre()` devuelve el nombre de la persona, el método `getEdad()` devuelve la edad de la persona y el método `saludar()` saluda a la persona.

La clase `Coche` tiene tres atributos: `marca`, `modelo` y `matricula`. También tiene tres métodos: `getMarca()`, `getModelo()` y `getMatricula()`. El método `getMarca()` devuelve la marca del coche, el método `getModelo()` devuelve el modelo del coche y el método `getMatricula()` devuelve la matricula del coche.

La clase `Casa` tiene tres atributos: `direccion`, `numeroHabitaciones` y `numeroBaños`. También tiene tres métodos: `getDireccion()`, `getNumeroHabitaciones()` y `getNumeroBaños()`. El método `getDireccion()` devuelve la direccion de la casa, el método `getNumeroHabitaciones()` devuelve el numero de habitaciones de la casa y el método `getNumeroBaños()` devuelve el numero de baños de la casa.

El código también crea una instancia de cada clase y llama a los métodos de cada instancia.