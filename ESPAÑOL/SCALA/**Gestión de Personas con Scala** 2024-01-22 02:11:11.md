```scala
// Definimos una clase llamada "Persona" con dos propiedades: "nombre" y "edad".
class Persona(val nombre: String, val edad: Int) {

  // Definimos un método llamado "saludar" que imprime un mensaje en la consola.
  def saludar(): Unit = {
    println(s"Hola, mi nombre es ${nombre} y tengo ${edad} años.")
  }
}

// Definimos una clase llamada "ListaPersonas" que es una colección de objetos de tipo "Persona".
class ListaPersonas extends scala.collection.mutable.Buffer[Persona] {

  // Definimos un método llamado "añadirPersona" que añade una persona a la lista.
  def añadirPersona(persona: Persona): Unit = {
    this += persona
  }

  // Definimos un método llamado "eliminarPersona" que elimina una persona de la lista.
  def eliminarPersona(persona: Persona): Unit = {
    this -= persona
  }

  // Definimos un método llamado "buscarPersonaPorNombre" que busca una persona en la lista por su nombre.
  def buscarPersonaPorNombre(nombre: String): Persona = {
    this.find(persona => persona.nombre == nombre).get
  }

  // Definimos un método llamado "ordenarPorNombre" que ordena la lista de personas por su nombre.
  def ordenarPorNombre(): Unit = {
    this.sortBy(_.nombre)
  }

  // Definimos un método llamado "imprimirLista" que imprime la lista de personas en la consola.
  def imprimirLista(): Unit = {
    this.foreach(persona => println(s"${persona.nombre} - ${persona.edad}"))
  }
}

// Creamos una lista de personas.
val listaPersonas = new ListaPersonas

// Añadimos algunas personas a la lista.
listaPersonas.añadirPersona(new Persona("Juan", 20))
listaPersonas.añadirPersona(new Persona("María", 25))
listaPersonas.añadirPersona(new Persona("Pedro", 30))

// Imprimimos la lista de personas en la consola.
listaPersonas.imprimirLista()

// Buscamos una persona en la lista por su nombre.
val personaBuscada = listaPersonas.buscarPersonaPorNombre("María")

// Imprimimos la persona encontrada en la consola.
println(s"Persona encontrada: ${personaBuscada.nombre} - ${personaBuscada.edad}")

// Ordenamos la lista de personas por su nombre.
listaPersonas.ordenarPorNombre()

// Imprimimos la lista de personas ordenada en la consola.
listaPersonas.imprimirLista()

// Eliminamos una persona de la lista.
listaPersonas.eliminarPersona(new Persona("Pedro", 30))

// Imprimimos la lista de personas después de eliminar a Pedro en la consola.
listaPersonas.imprimirLista()
```

**Explicación del código:**

1. Definimos una clase llamada "Persona" con dos propiedades: "nombre" y "edad", y un método llamado "saludar" que imprime un mensaje en la consola.
2. Definimos una clase llamada "ListaPersonas" que es una colección de objetos de tipo "Persona", y varios métodos para añadir, eliminar, buscar y ordenar personas en la lista.
3. Creamos una lista de personas y añadimos algunas personas a ella.
4. Imprimimos la lista de personas en la consola.
5. Buscamos una persona en la lista por su nombre.
6. Imprimimos la persona encontrada en la consola.
7. Ordenamos la lista de personas por su nombre.
8. Imprimimos la lista de personas ordenada en la consola.
9. Eliminamos una persona de la lista.
10. Imprimimos la lista de personas después de eliminar a Pedro en la consola.