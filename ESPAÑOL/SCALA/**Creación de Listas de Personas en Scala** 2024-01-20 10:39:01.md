```scala
// Definición de la clase Persona
class Persona(val nombre: String, val apellido: String, val edad: Int) {

  // Método para obtener el nombre completo de la persona
  def getNombreCompleto(): String = {
    nombre + " " + apellido
  }

  // Método para obtener la edad de la persona
  def getEdad(): Int = {
    edad
  }

  // Método para obtener una representación en cadena de la persona
  override def toString(): String = {
    "Nombre: " + getNombreCompleto() + ", Edad: " + getEdad()
  }
}

// Definición de la clase ListaPersonas
class ListaPersonas {

  // Lista de personas
  private var personas: List[Persona] = List()

  // Método para añadir una persona a la lista
  def añadirPersona(persona: Persona): Unit = {
    personas = persona :: personas
  }

  // Método para obtener la lista de personas
  def getPersonas(): List[Persona] = {
    personas
  }

  // Método para obtener una representación en cadena de la lista de personas
  override def toString(): String = {
    "Lista de personas:\n" + personas.mkString("\n")
  }
}

// Definición de la clase Principal
object Principal {

  // Método principal
  def main(args: Array[String]): Unit = {

    // Creación de una lista de personas
    val listaPersonas = new ListaPersonas()

    // Añadir personas a la lista
    listaPersonas.añadirPersona(new Persona("Juan", "Pérez", 25))
    listaPersonas.añadirPersona(new Persona("Ana", "López", 30))
    listaPersonas.añadirPersona(new Persona("Pedro", "García", 35))

    // Imprimir la lista de personas
    println(listaPersonas)
  }
}
```

Explicación del código:

1. Definición de la clase `Persona`:

   - La clase `Persona` representa a una persona con un nombre, un apellido y una edad.
   - La clase tiene tres métodos: `getNombreCompleto()`, `getEdad()` y `toString()`.
   - El método `getNombreCompleto()` devuelve el nombre completo de la persona.
   - El método `getEdad()` devuelve la edad de la persona.
   - El método `toString()` devuelve una representación en cadena de la persona.

2. Definición de la clase `ListaPersonas`:

   - La clase `ListaPersonas` representa una lista de personas.
   - La clase tiene tres métodos: `añadirPersona()`, `getPersonas()` y `toString()`.
   - El método `añadirPersona()` añade una persona a la lista.
   - El método `getPersonas()` devuelve la lista de personas.
   - El método `toString()` devuelve una representación en cadena de la lista de personas.

3. Definición de la clase `Principal`:

   - La clase `Principal` es la clase principal del programa.
   - La clase tiene un método principal `main()`, que es el punto de entrada del programa.

4. Método `main()`:

   - El método `main()` crea una lista de personas, añade personas a la lista e imprime la lista de personas.

5. Uso del método `mkString()`:

   - El método `mkString()` se utiliza para convertir una lista en una cadena.
   - El método `mkString()` toma un separador como argumento y devuelve una cadena con los elementos de la lista separados por el separador.

Este código es complejo porque:

1. Utiliza varias clases y objetos.
2. Utiliza herencia y polimorfismo.
3. Utiliza el método `mkString()` para convertir una lista en una cadena.
4. El código está bien comentado y es fácil de entender.