```scala
// Definimos una clase llamada "Persona" que representa a una persona.
class Persona(nombre: String, apellido: String, edad: Int) {
  // Definimos los atributos de la clase "Persona".
  def nombre = nombre
  def apellido = apellido
  def edad = edad

  // Definimos un método llamado "saludar()" que devuelve un mensaje de saludo.
  def saludar(): String = {
    "Hola, mi nombre es " + nombre + " " + apellido + " y tengo " + edad + " años."
  }
}

// Definimos una clase llamada "Empleado" que hereda de la clase "Persona".
class Empleado(nombre: String, apellido: String, edad: Int, cargo: String, salario: Double) extends Persona(nombre, apellido, edad) {
  // Definimos los atributos de la clase "Empleado".
  def cargo = cargo
  def salario = salario

  // Definimos un método llamado "trabajar()" que devuelve un mensaje de trabajo.
  def trabajar(): String = {
    "Estoy trabajando como " + cargo + " y gano " + salario + " euros al mes."
  }
}

// Definimos una clase llamada "Estudiante" que hereda de la clase "Persona".
class Estudiante(nombre: String, apellido: String, edad: Int, carrera: String, universidad: String) extends Persona(nombre, apellido, edad) {
  // Definimos los atributos de la clase "Estudiante".
  def carrera = carrera
  def universidad = universidad

  // Definimos un método llamado "estudiar()" que devuelve un mensaje de estudio.
  def estudiar(): String = {
    "Estoy estudiando " + carrera + " en la universidad " + universidad + "."
  }
}

// Definimos una función principal para ejecutar el programa.
def main(args: Array[String]): Unit = {
  // Creamos un objeto de la clase "Persona" llamado "persona1".
  val persona1 = new Persona("Juan", "García", 20)

  // Creamos un objeto de la clase "Empleado" llamado "empleado1".
  val empleado1 = new Empleado("María", "López", 30, "Ingeniero", 2000.0)

  // Creamos un objeto de la clase "Estudiante" llamado "estudiante1".
  val estudiante1 = new Estudiante("Pedro", "Sánchez", 18, "Informática", "Universidad de Málaga")

  // Imprimimos los mensajes de saludo, trabajo y estudio de los objetos creados.
  println(persona1.saludar())
  println(empleado1.trabajar())
  println(estudiante1.estudiar())
}
```

Explicación del código:

1. Definimos una clase llamada "Persona" que representa a una persona. Esta clase tiene tres atributos: nombre, apellido y edad. También definimos un método llamado "saludar()" que devuelve un mensaje de saludo.
2. Definimos una clase llamada "Empleado" que hereda de la clase "Persona". Esta clase tiene dos atributos adicionales: cargo y salario. También definimos un método llamado "trabajar()" que devuelve un mensaje de trabajo.
3. Definimos una clase llamada "Estudiante" que hereda de la clase "Persona". Esta clase tiene dos atributos adicionales: carrera y universidad. También definimos un método llamado "estudiar()" que devuelve un mensaje de estudio.
4. Definimos una función principal para ejecutar el programa. En esta función creamos objetos de las clases "Persona", "Empleado" y "Estudiante" y luego imprimimos los mensajes de saludo, trabajo y estudio de estos objetos.