```scala
// Definición de la clase Persona con propiedades nombre y edad
class Persona(var nombre: String, var edad: Int) {

  // Método para obtener el nombre de la persona
  def getNombre(): String = nombre

  // Método para obtener la edad de la persona
  def getEdad(): Int = edad

  // Método para establecer el nombre de la persona
  def setNombre(nombre: String): Unit = {
    this.nombre = nombre
  }

  // Método para establecer la edad de la persona
  def setEdad(edad: Int): Unit = {
    this.edad = edad
  }

  // Método para imprimir los datos de la persona
  def imprimirDatos(): Unit = {
    println(s"Nombre: $nombre")
    println(s"Edad: $edad")
  }
}

// Definición de la clase ListaPersonas, que contiene una lista de objetos de tipo Persona
class ListaPersonas {

  // Variable privada para almacenar la lista de personas
  private var listaPersonas: List[Persona] = List()

  // Método para agregar una persona a la lista
  def agregarPersona(persona: Persona): Unit = {
    listaPersonas = listaPersonas :+ persona
  }

  // Método para obtener la lista de personas
  def getListaPersonas(): List[Persona] = listaPersonas

  // Método para imprimir los datos de todas las personas de la lista
  def imprimirDatosLista(): Unit = {
    for (persona <- listaPersonas) {
      persona.imprimirDatos()
      println()
    }
  }
}

// Definición de la clase Main, que contiene el método main para ejecutar el programa
object Main {

  // Método main para ejecutar el programa
  def main(args: Array[String]): Unit = {

    // Creación de una lista de personas
    val listaPersonas = new ListaPersonas()

    // Creación de una persona
    val persona1 = new Persona("Juan", 25)

    // Agregamos la persona a la lista
    listaPersonas.agregarPersona(persona1)

    // Creación de otra persona
    val persona2 = new Persona("María", 30)

    // Agregamos la persona a la lista
    listaPersonas.agregarPersona(persona2)

    // Imprimimos los datos de todas las personas de la lista
    listaPersonas.imprimirDatosLista()
  }
}
```

Explicación del código:

* La clase `Persona` define un objeto de tipo persona con dos propiedades: `nombre` y `edad`. La clase también define métodos para obtener y establecer los valores de estas propiedades, así como un método para imprimir los datos de la persona.

* La clase `ListaPersonas` define una lista de objetos de tipo `Persona`. La clase también define métodos para agregar personas a la lista, obtener la lista de personas e imprimir los datos de todas las personas de la lista.

* La clase `Main` define el método `main` para ejecutar el programa. En el método `main`, creamos una lista de personas y agregamos dos personas a la lista. Luego, imprimimos los datos de todas las personas de la lista.