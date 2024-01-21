```scala
object CodigoComplejoScala {

  // Definir una clase de datos para representar una persona
  case class Persona(nombre: String, edad: Int, ocupacion: String)

  // Definir una clase de compañía para representar una empresa
  case class Compania(nombre: String, direccion: String, numeroEmpleados: Int)

  // Definir una clase de ciudad para representar una ciudad
  case class Ciudad(nombre: String, poblacion: Int, alcalde: String)

  // Definir una clase de país para representar un país
  case class Pais(nombre: String, capital: String, presidente: String)

  // Definir una lista de personas
  val personas = List(
    Persona("Juan", 25, "Ingeniero"),
    Persona("María", 30, "Doctora"),
    Persona("Pedro", 35, "Abogado"),
    Persona("Ana", 40, "Profesora"),
    Persona("Carlos", 45, "Contador")
  )

  // Definir una lista de compañías
  val companias = List(
    Compania("Google", "1600 Amphitheatre Parkway, Mountain View, CA 94043", 10000),
    Compania("Apple", "One Apple Park Way, Cupertino, CA 95014", 12000),
    Compania("Microsoft", "One Microsoft Way, Redmond, WA 98052", 13000),
    Compania("Amazon", "410 Terry Avenue North, Seattle, WA 98109", 14000),
    Compania("Tesla", "3500 Deer Creek Road, Palo Alto, CA 94304", 15000)
  )

  // Definir una lista de ciudades
  val ciudades = List(
    Ciudad("Madrid", 3200000, "José Luis Martínez-Almeida"),
    Ciudad("Barcelona", 1600000, "Ada Colau"),
    Ciudad("Valencia", 800000, "Joan Ribó"),
    Ciudad("Sevilla", 700000, "Antonio Muñoz Martínez"),
    Ciudad("Zaragoza", 650000, "Jorge Azcón")
  )

  // Definir una lista de países
  val paises = List(
    Pais("España", "Madrid", "Pedro Sánchez"),
    Pais("Francia", "París", "Emmanuel Macron"),
    Pais("Alemania", "Berlín", "Olaf Scholz"),
    Pais("Reino Unido", "Londres", "Rishi Sunak"),
    Pais("Italia", "Roma", "Giorgia Meloni")
  )

  // Función para imprimir una lista de elementos
  def imprimirLista[T](lista: List[T]): Unit = {
    lista.foreach(println)
  }

  // Función para imprimir una lista de personas
  def imprimirPersonas(personas: List[Persona]): Unit = {
    println("Personas:")
    imprimirLista(personas)
  }

  // Función para imprimir una lista de compañías
  def imprimirCompanias(companias: List[Compania]): Unit = {
    println("Compañías:")
    imprimirLista(companias)
  }

  // Función para imprimir una lista de ciudades
  def imprimirCiudades(ciudades: List[Ciudad]): Unit = {
    println("Ciudades:")
    imprimirLista(ciudades)
  }

  // Función para imprimir una lista de países
  def imprimirPaises(paises: List[Pais]): Unit = {
    println("Países:")
    imprimirLista(paises)
  }

  // Función principal
  def main(args: Array[String]): Unit = {
    imprimirPersonas(personas)
    imprimirCompanias(companias)
    imprimirCiudades(ciudades)
    imprimirPaises(paises)
  }
}
```

Explicación del código:

* Se definen cuatro clases de datos: `Persona`, `Compania`, `Ciudad` y `Pais`. Cada clase de datos representa un tipo de objeto diferente.
* Se definen cuatro listas: `personas`, `companias`, `ciudades` y `paises`. Cada lista contiene objetos del tipo correspondiente.
* Se definen cuatro funciones para imprimir una lista de elementos: `imprimirLista`, `imprimirPersonas`, `imprimirCompanias`, `imprimirCiudades` y `imprimirPaises`. Cada función imprime una lista de objetos del tipo correspondiente.
* La función principal `main` llama a las funciones de impresión para imprimir las listas de personas, compañías, ciudades y países.