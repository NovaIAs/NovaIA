```scala
// Definimos una clase llamada "Persona" con propiedades "nombre", "edad" y "ocupación".
class Persona(val nombre: String, val edad: Int, val ocupacion: String) {

  // Definimos un método llamado "saludar()" que devuelve un saludo personalizado.
  def saludar(): String = {
    s"Hola, me llamo $nombre, tengo $edad años y soy $ocupacion."
  }
}

// Definimos una clase llamada "Estudiante" que hereda de la clase "Persona".
class Estudiante(nombre: String, edad: Int, ocupacion: String, val curso: String) extends Persona(nombre, edad, ocupacion) {

  // Definimos un método llamado "estudiar()" que devuelve un mensaje indicando que el estudiante está estudiando.
  def estudiar(): String = {
    s"$nombre está estudiando $curso."
  }
}

// Definimos una clase llamada "Profesor" que hereda de la clase "Persona".
class Profesor(nombre: String, edad: Int, ocupacion: String, val asignatura: String) extends Persona(nombre, edad, ocupacion) {

  // Definimos un método llamado "enseñar()" que devuelve un mensaje indicando que el profesor está enseñando.
  def enseñar(): String = {
    s"$nombre está enseñando $asignatura."
  }
}

// Creamos una lista de personas.
val personas = List(
  new Persona("Juan", 20, "Estudiante"),
  new Estudiante("María", 22, "Estudiante", "Ingeniería Informática"),
  new Profesor("Pedro", 30, "Profesor", "Matemáticas")
)

// Recorremos la lista de personas e imprimimos el saludo de cada una.
personas.foreach(persona => println(persona.saludar()))

// Recorremos la lista de personas e imprimimos el mensaje de estudio o enseñanza de cada una.
personas.foreach {
  case estudiante: Estudiante => println(estudiante.estudiar())
  case profesor: Profesor => println(profesor.enseñar())
}
```

Este código define una clase base llamada "Persona" con propiedades "nombre", "edad" y "ocupación", y un método llamado "saludar()" que devuelve un saludo personalizado.

A continuación, define dos clases derivadas de "Persona": "Estudiante" y "Profesor". La clase "Estudiante" tiene una propiedad adicional llamada "curso", y la clase "Profesor" tiene una propiedad adicional llamada "asignatura".

También define dos métodos adicionales: "estudiar()" y "enseñar()", que devuelven mensajes indicando que el estudiante está estudiando o que el profesor está enseñando, respectivamente.

Finalmente, crea una lista de personas e imprime el saludo de cada una, así como el mensaje de estudio o enseñanza de cada una.

Este código es complejo porque utiliza clases base y derivadas, así como métodos polimórficos, para representar una jerarquía de objetos con diferentes propiedades y comportamientos. También utiliza una lista de objetos para almacenar y procesar una colección de datos.