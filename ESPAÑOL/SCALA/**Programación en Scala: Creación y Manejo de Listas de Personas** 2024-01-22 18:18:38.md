```scala
import scala.util.Random

// Clase Persona que representa a una persona con un nombre y una edad
class Persona(val nombre: String, val edad: Int) {

  // Método para obtener el nombre de la persona
  def getNombre(): String = {
    nombre
  }

  // Método para obtener la edad de la persona
  def getEdad(): Int = {
    edad
  }

  // Método para generar una persona aleatoria
  def generarPersonaAleatoria(): Persona = {
    // Generar un nombre aleatorio
    val nombres = Array("Juan", "María", "Pedro", "Ana", "José")
    val nombreAleatorio = nombres(Random.nextInt(nombres.length))

    // Generar una edad aleatoria entre 1 y 100
    val edadAleatoria = Random.nextInt(100) + 1

    // Crear una nueva persona con el nombre y la edad aleatorios
    new Persona(nombreAleatorio, edadAleatoria)
  }
}

// Clase ListaPersonas que representa una lista de personas
class ListaPersonas() {

  // Colección mutable de personas
  private var personas: List[Persona] = List()

  // Método para añadir una persona a la lista
  def añadirPersona(persona: Persona): Unit = {
    personas = personas :+ persona
  }

  // Método para obtener la lista de personas
  def getPersonas(): List[Persona] = {
    personas
  }

  // Método para obtener el número de personas en la lista
  def getNumeroPersonas(): Int = {
    personas.length
  }

  // Método para generar una lista de personas aleatorias de un tamaño determinado
  def generarListaPersonasAleatorias(tamaño: Int): List[Persona] = {
    // Crear una lista vacía
    var listaPersonas = List[Persona]()

    // Añadir personas aleatorias a la lista hasta alcanzar el tamaño deseado
    for (i <- 1 to tamaño) {
      listaPersonas = listaPersonas :+ new Persona("", 0).generarPersonaAleatoria()
    }

    // Devolver la lista de personas aleatorias generada
    listaPersonas
  }
}

// Crear una lista de personas
val listaPersonas = new ListaPersonas()

// Añadir algunas personas a la lista
listaPersonas.añadirPersona(new Persona("Juan", 25))
listaPersonas.añadirPersona(new Persona("María", 30))
listaPersonas.añadirPersona(new Persona("Pedro", 35))

// Generar una lista de 10 personas aleatorias y añadirla a la lista
listaPersonas.añadirPersona(new ListaPersonas().generarListaPersonasAleatorias(10))

// Obtener la lista de personas
val personas = listaPersonas.getPersonas()

// Imprimir la lista de personas
for (persona <- personas) {
  println(persona.getNombre() + " tiene " + persona.getEdad() + " años")
}
```

Explicación del código:

* Se definen dos clases, `Persona` y `ListaPersonas`.
* La clase `Persona` representa a una persona con un nombre y una edad.
* La clase `ListaPersonas` representa una lista de personas.
* Se crean algunas personas e se añaden a la lista de personas.
* Se genera una lista de 10 personas aleatorias y se añade a la lista de personas.
* Se obtiene la lista de personas.
* Se imprime la lista de personas.

Este código es complejo porque utiliza una serie de conceptos diferentes de Scala, como clases, objetos, listas, bucles y funciones. También utiliza el generador aleatorio de Scala para generar personas aleatorias.