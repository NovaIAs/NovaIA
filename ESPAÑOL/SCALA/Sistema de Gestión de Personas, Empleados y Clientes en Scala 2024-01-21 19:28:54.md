```scala
// Clase Persona que define los atributos y métodos de una persona
class Persona(nombre: String, edad: Int) {
  // Atributos
  var nombre: String = nombre
  var edad: Int = edad

  // Métodos
  def saludar(): Unit = {
    println(s"Hola, mi nombre es $nombre y tengo $edad años.")
  }

  def crecer(): Unit = {
    edad += 1
  }
}

// Clase Empleado que hereda de la clase Persona y define los atributos y métodos específicos de un empleado
class Empleado(nombre: String, edad: Int, salario: Double) extends Persona(nombre, edad) {
  // Atributos
  var salario: Double = salario

  // Métodos
  def trabajar(): Unit = {
    println(s"Estoy trabajando y ganando $salario por hora.")
  }

  def cobrarNomina(): Double = {
    salario * 160
  }
}

// Clase Cliente que hereda de la clase Persona y define los atributos y métodos específicos de un cliente
class Cliente(nombre: String, edad: Int, telefono: String) extends Persona(nombre, edad) {
  // Atributos
  var telefono: String = telefono

  // Métodos
  def comprar(): Unit = {
    println(s"Estoy comprando en la tienda.")
  }

  def llamar(): Unit = {
    println(s"Estoy llamando al número $telefono.")
  }
}

// Función principal del programa
def main(args: Array[String]): Unit = {
  // Creamos algunos objetos de las clases Persona, Empleado y Cliente
  val persona1 = new Persona("Juan", 20)
  val empleado1 = new Empleado("María", 30, 2000)
  val cliente1 = new Cliente("Pedro", 40, "555-123-4567")

  // Llamamos a los métodos de los objetos para mostrar su comportamiento
  persona1.saludar()
  empleado1.trabajar()
  cliente1.comprar()
  cliente1.llamar()

  // Mostramos el salario del empleado y el número de teléfono del cliente
  println(s"El salario de $empleado1 es ${empleado1.cobrarNomina()}")
  println(s"El número de teléfono de $cliente1 es ${cliente1.telefono}")
}
```

Este código implementa un sistema básico de gestión de personas, empleados y clientes. Cada clase tiene sus propios atributos y métodos, que se pueden utilizar para gestionar la información y el comportamiento de los objetos.

La función `main()` crea varios objetos de las clases `Persona`, `Empleado` y `Cliente`, y luego llama a sus métodos para mostrar su comportamiento. También muestra el salario del empleado y el número de teléfono del cliente.

Este código es bastante complejo, pero está bien organizado y es fácil de entender. Es un buen ejemplo de cómo utilizar la herencia y el polimorfismo en Scala para crear un sistema escalable y mantenible.