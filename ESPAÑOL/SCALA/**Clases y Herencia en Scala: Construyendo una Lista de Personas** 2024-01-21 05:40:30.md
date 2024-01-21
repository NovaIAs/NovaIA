```scala
// Definición de una clase Persona con propiedades nombre, apellido y edad
class Persona(val nombre: String, val apellido: String, val edad: Int) {

  // Método para obtener el nombre completo de la persona
  def getNombreCompleto(): String = {
    nombre + " " + apellido
  }

  // Método para obtener la edad de la persona
  def getEdad(): Int = edad

}

// Definición de una clase Estudiante que hereda de la clase Persona
class Estudiante(nombre: String, apellido: String, edad: Int, val notaMedia: Double) extends Persona(nombre, apellido, edad) {

  // Método para obtener la nota media del estudiante
  def getNotaMedia(): Double = notaMedia

}

// Definición de una clase Profesor que hereda de la clase Persona
class Profesor(nombre: String, apellido: String, edad: Int, val asignatura: String) extends Persona(nombre, apellido, edad) {

  // Método para obtener la asignatura que imparte el profesor
  def getAsignatura(): String = asignatura

}

// Definición de una función para crear una lista de personas
def crearListaPersonas(): List[Persona] = {
  List(
    new Estudiante("Juan", "Pérez", 20, 8.5),
    new Estudiante("María", "García", 22, 9.0),
    new Profesor("José", "López", 40, "Matemáticas"),
    new Profesor("Ana", "Sánchez", 35, "Lengua")
  )
}

// Definición de una función para imprimir la lista de personas
def imprimirListaPersonas(lista: List[Persona]): Unit = {
  println("Lista de personas:")
  for (persona <- lista) {
    println(persona.getNombreCompleto() + " - " + persona.getEdad())
  }
}

// Función principal del programa
def main(args: Array[String]): Unit = {
  val listaPersonas = crearListaPersonas()
  imprimirListaPersonas(listaPersonas)
}
```

Este código es una implementación en Scala de un programa que define clases para representar personas, estudiantes y profesores. El programa también define funciones para crear una lista de personas e imprimirla.

El código se puede explicar de la siguiente manera:

1. Se definen las clases `Persona`, `Estudiante` y `Profesor`. La clase `Persona` tiene tres propiedades: `nombre`, `apellido` y `edad`. La clase `Estudiante` hereda de la clase `Persona` y tiene una propiedad adicional: `notaMedia`. La clase `Profesor` también hereda de la clase `Persona` y tiene una propiedad adicional: `asignatura`.
2. Se define una función `crearListaPersonas()` que crea una lista de personas. La lista incluye dos estudiantes y dos profesores.
3. Se define una función `imprimirListaPersonas()` que imprime la lista de personas. La función imprime el nombre completo y la edad de cada persona.
4. La función `main` del programa llama a la función `crearListaPersonas()` para crear una lista de personas y luego llama a la función `imprimirListaPersonas()` para imprimir la lista.

El programa imprimirá la siguiente salida:

```
Lista de personas:
Juan Pérez - 20
María García - 22
José López - 40
Ana Sánchez - 35
```