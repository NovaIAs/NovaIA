```scala
// Importamos las librerías necesarias
import scala.util.Random
import scala.collection.mutable

// Creamos una clase que representa a una persona
class Persona(nombre: String, apellidos: String, edad: Int) {
  var nombre = nombre
  var apellidos = apellidos
  var edad = edad

  // Método que devuelve la edad de la persona
  def getEdad: Int = {
    edad
  }

  // Método que devuelve el nombre completo de la persona
  def getNombreCompleto: String = {
    nombre + " " + apellidos
  }
}

// Creamos una lista de personas
val personas = mutable.ListBuffer[Persona]()

// Añadimos 10 personas a la lista
for (i <- 1 to 10) {
  val nombre = Random.nextString(10)
  val apellidos = Random.nextString(10)
  val edad = Random.nextInt(100)

  val persona = new Persona(nombre, apellidos, edad)
  personas += persona
}

// Imprimimos la lista de personas
println("Lista de personas:")
for (persona <- personas) {
  println(persona.getNombreCompleto + " (" + persona.getEdad + ")")
}

// Ordenamos la lista de personas por edad
val personasOrdenadasPorEdad = personas.sortWith((p1, p2) => p1.getEdad < p2.getEdad)

// Imprimimos la lista de personas ordenadas por edad
println("Lista de personas ordenadas por edad:")
for (persona <- personasOrdenadasPorEdad) {
  println(persona.getNombreCompleto + " (" + persona.getEdad + ")")
}

// Creamos un mapa que asocia el nombre de la persona con su edad
val mapaPersonas = mutable.Map[String, Int]()

// Añadimos las personas al mapa
for (persona <- personas) {
  mapaPersonas += (persona.getNombreCompleto -> persona.getEdad)
}

// Imprimimos el mapa de personas
println("Mapa de personas:")
for ((nombre, edad) <- mapaPersonas) {
  println(nombre + " -> " + edad)
}
```

Este código crea una lista de 10 personas, ordena la lista por edad y crea un mapa que asocia el nombre de la persona con su edad.

El código está explicado paso a paso:

1. Importamos las librerías necesarias.
2. Creamos una clase que representa a una persona.
3. Creamos una lista de personas.
4. Añadimos 10 personas a la lista.
5. Imprimimos la lista de personas.
6. Ordenamos la lista de personas por edad.
7. Imprimimos la lista de personas ordenadas por edad.
8. Creamos un mapa que asocia el nombre de la persona con su edad.
9. Añadimos las personas al mapa.
10. Imprimimos el mapa de personas.