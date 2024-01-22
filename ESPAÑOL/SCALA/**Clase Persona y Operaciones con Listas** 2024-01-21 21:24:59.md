```scala
// Definición de una clase en Scala
class Persona(nombre: String, edad: Int) {

  // Propiedades de la clase
  val nombre: String = nombre
  val edad: Int = edad

  // Método para obtener el nombre de la persona
  def getNombre(): String = {
    return nombre
  }

  // Método para obtener la edad de la persona
  def getEdad(): Int = {
    return edad
  }

  // Método para saludar a la persona
  def saludar(): Unit = {
    println(s"Hola, $nombre! Tienes $edad años.")
  }
}

// Definición de una función para crear una lista de personas
def crearListaPersonas(): List[Persona] = {
  val personas = List(
    new Persona("Juan", 20),
    new Persona("María", 25),
    new Persona("Pedro", 30),
    new Persona("Ana", 35),
    new Persona("José", 40)
  )

  return personas
}

// Definición de una función para filtrar la lista de personas por edad
def filtrarPersonasPorEdad(personas: List[Persona], edadMinima: Int, edadMaxima: Int): List[Persona] = {
  val personasFiltradas = personas.filter(persona => persona.getEdad() >= edadMinima && persona.getEdad() <= edadMaxima)

  return personasFiltradas
}

// Definición de una función para obtener el promedio de edad de una lista de personas
def calcularPromedioEdad(personas: List[Persona]): Double = {
  val sumaEdades = personas.map(persona => persona.getEdad()).sum
  val promedioEdad = sumaEdades / personas.length

  return promedioEdad
}

// Definición de la función principal
def main(args: Array[String]): Unit = {

  // Creación de la lista de personas
  val personas = crearListaPersonas()

  // Filtrado de la lista de personas por edad
  val personasFiltradas = filtrarPersonasPorEdad(personas, 25, 35)

  // Obtención del promedio de edad de la lista de personas filtradas
  val promedioEdad = calcularPromedioEdad(personasFiltradas)

  // Impresión de los resultados
  println(s"Lista de personas:")
  personas.foreach(persona => println(s"${persona.getNombre()} (${persona.getEdad()} años)"))

  println()

  println(s"Lista de personas filtradas por edad:")
  personasFiltradas.foreach(persona => println(s"${persona.getNombre()} (${persona.getEdad()} años)"))

  println()

  println(s"Promedio de edad de las personas filtradas: $promedioEdad años")
}
```

Explicación del código:

- La clase `Persona` define las propiedades `nombre` y `edad`, y los métodos `getNombre()`, `getEdad()` y `saludar()`.

- La función `crearListaPersonas()` crea una lista de objetos `Persona`.

- La función `filtrarPersonasPorEdad()` filtra la lista de personas por edad, devolviendo una nueva lista con las personas que cumplen con los criterios de edad especificados.

- La función `calcularPromedioEdad()` calcula el promedio de edad de una lista de personas.

- La función `main()` crea una lista de personas, filtra la lista por edad y calcula el promedio de edad de las personas filtradas. Finalmente, imprime los resultados por consola.