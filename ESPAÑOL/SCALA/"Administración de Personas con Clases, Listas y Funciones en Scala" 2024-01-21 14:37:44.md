```scala
// Definimos una clase llamada "Persona" con atributos "nombre" y "edad".
class Persona(val nombre: String, val edad: Int) {

    // Definimos un método para imprimir los atributos de la persona.
    def imprimir(): Unit = {
        println(s"Nombre: $nombre, Edad: $edad")
    }
}

// Definimos una clase llamada "ListaPersonas" que contiene una lista de personas.
class ListaPersonas {

    // Definimos una lista mutable de personas.
    private var personas: List[Persona] = List()

    // Definimos un método para añadir una persona a la lista.
    def añadirPersona(persona: Persona): Unit = {
        personas = personas :+ persona
    }

    // Definimos un método para eliminar una persona de la lista.
    def eliminarPersona(persona: Persona): Unit = {
        personas = personas.filterNot(_ == persona)
    }

    // Definimos un método para imprimir la lista de personas.
    def imprimir(): Unit = {
        for (persona <- personas) {
            persona.imprimir()
        }
    }
}

// Definimos una función para crear una lista de personas.
def crearListaPersonas(): ListaPersonas = {
    val listaPersonas = new ListaPersonas()

    listaPersonas.añadirPersona(new Persona("Juan", 20))
    listaPersonas.añadirPersona(new Persona("María", 25))
    listaPersonas.añadirPersona(new Persona("Pedro", 30))

    return listaPersonas
}

// Definimos la función principal.
def main(args: Array[String]): Unit = {

    // Creamos una lista de personas.
    val listaPersonas = crearListaPersonas()

    // Imprimimos la lista de personas.
    listaPersonas.imprimir()

    // Eliminamos a Pedro de la lista.
    listaPersonas.eliminarPersona(new Persona("Pedro", 30))

    // Volvemos a imprimir la lista de personas.
    listaPersonas.imprimir()
}
```

Explicación del código:

* Definimos una clase llamada "Persona" con atributos "nombre" y "edad". También definimos un método para imprimir los atributos de la persona.
* Definimos una clase llamada "ListaPersonas" que contiene una lista de personas. Definimos métodos para añadir, eliminar e imprimir personas de la lista.
* Definimos una función para crear una lista de personas.
* Definimos la función principal, que crea una lista de personas, la imprime, elimina a una persona de la lista y la vuelve a imprimir.

Este código es complejo porque utiliza clases, métodos, funciones, listas y bucles. También es diferenciado porque no se repite fácilmente.