```scala
// Definimos una clase llamada "Persona" que representa a una persona con su nombre y edad.
class Persona(val nombre: String, val edad: Int) {

  // Definimos un método llamado "saludar" que imprime un mensaje de saludo con el nombre de la persona.
  def saludar(): Unit = {
    println(s"¡Hola, mi nombre es $nombre!")
  }

  // Definimos un método llamado "cumplirAños" que incrementa la edad de la persona en uno.
  def cumplirAños(): Unit = {
    edad += 1
  }
}

// Definimos una clase llamada "Familia" que representa a una familia con una lista de miembros y un apellido.
class Familia(val miembros: List[Persona], val apellido: String) {

  // Definimos un método llamado "imprimirMiembros" que imprime el nombre y la edad de cada miembro de la familia.
  def imprimirMiembros(): Unit = {
    miembros.foreach { miembro =>
      println(s"${miembro.nombre} (${miembro.edad} años)")
    }
  }

  // Definimos un método llamado "añadirMiembro" que añade un nuevo miembro a la familia.
  def añadirMiembro(nuevoMiembro: Persona): Unit = {
    miembros :+= nuevoMiembro
  }
}

// Creamos una instancia de la clase "Persona" llamada "Juan".
val juan = new Persona("Juan", 25)

// Creamos una instancia de la clase "Familia" llamada "Familia Pérez".
val familiaPerez = new Familia(List(juan), "Pérez")

// Añadimos un nuevo miembro a la familia, llamado "Ana".
familiaPerez.añadirMiembro(new Persona("Ana", 23))

// Imprimimos los miembros de la familia.
familiaPerez.imprimirMiembros()
```

Este código crea una clase llamada "Persona" que representa a una persona con su nombre y edad, y una clase llamada "Familia" que representa a una familia con una lista de miembros y un apellido.

A continuación, creamos una instancia de la clase "Persona" llamada "Juan" y una instancia de la clase "Familia" llamada "Familia Pérez".

Luego, añadimos un nuevo miembro a la familia, llamado "Ana", e imprimimos los miembros de la familia.

Este código es complejo y diferenciado porque combina dos clases diferentes, "Persona" y "Familia", y utiliza métodos y propiedades para representar y manipular los datos de las personas y las familias. Además, utiliza la sintaxis de Scala para definir clases, métodos y propiedades, que es diferente de la sintaxis de otros lenguajes de programación.