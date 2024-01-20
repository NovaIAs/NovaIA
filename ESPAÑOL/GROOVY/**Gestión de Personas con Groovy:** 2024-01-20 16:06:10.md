```groovy
// Definimos una clase llamada "Persona" que representa a una persona con un nombre y una edad.
class Persona {
    String nombre
    int edad

    // Definimos el constructor de la clase "Persona".
    Persona(String nombre, int edad) {
        this.nombre = nombre
        this.edad = edad
    }

    // Definimos un método llamado "saludar" que imprime un saludo personalizado para la persona.
    def saludar() {
        println("Hola, mi nombre es ${nombre} y tengo ${edad} años.")
    }
}

// Definimos una clase llamada "ListaPersonas" que representa una lista de personas.
class ListaPersonas {
    List<Persona> personas = new ArrayList<>()

    // Definimos el método "agregarPersona" que añade una nueva persona a la lista.
    def agregarPersona(Persona persona) {
        personas.add(persona)
    }

    // Definimos el método "eliminarPersona" que elimina una persona de la lista.
    def eliminarPersona(Persona persona) {
        personas.remove(persona)
    }

    // Definimos el método "buscarPersonaPorNombre" que busca una persona en la lista por su nombre.
    def buscarPersonaPorNombre(String nombre) {
        personas.find { it.nombre == nombre }
    }

    // Definimos el método "imprimirListaPersonas" que imprime todas las personas de la lista.
    def imprimirListaPersonas() {
        personas.each { println(it) }
    }
}

// Creamos una lista de personas.
def listaPersonas = new ListaPersonas()

// Añadimos algunas personas a la lista.
listaPersonas.agregarPersona(new Persona("Juan", 20))
listaPersonas.agregarPersona(new Persona("María", 25))
listaPersonas.agregarPersona(new Persona("Pedro", 30))

// Imprimimos la lista de personas.
listaPersonas.imprimirListaPersonas()

// Buscamos una persona en la lista por su nombre.
def personaBuscada = listaPersonas.buscarPersonaPorNombre("Pedro")

// Imprimimos la persona encontrada.
println("Persona encontrada: ${personaBuscada}")

// Eliminamos la persona encontrada de la lista.
listaPersonas.eliminarPersona(personaBuscada)

// Imprimimos la lista de personas después de eliminar a la persona.
listaPersonas.imprimirListaPersonas()
```

Explicación del código:

* Creamos una clase llamada "Persona" que representa a una persona con un nombre y una edad.
* Creamos una clase llamada "ListaPersonas" que representa una lista de personas.
* Añadimos algunas personas a la lista.
* Imprimimos la lista de personas.
* Buscamos una persona en la lista por su nombre.
* Imprimimos la persona encontrada.
* Eliminamos la persona encontrada de la lista.
* Imprimimos la lista de personas después de eliminar a la persona.