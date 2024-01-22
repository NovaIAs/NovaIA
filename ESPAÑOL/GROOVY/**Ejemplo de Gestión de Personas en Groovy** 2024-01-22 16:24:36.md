```groovy
// Clase para representar objetos de tipo Persona
class Persona {
    String nombre
    String apellido
    int edad
}

// Clase para gestionar la lista de personas
class GestorPersonas {
    List<Persona> personas = []

    // Función para añadir una persona a la lista
    void añadirPersona(Persona persona) {
        personas.add(persona)
    }

    // Función para eliminar una persona de la lista
    void eliminarPersona(Persona persona) {
        personas.remove(persona)
    }

    // Función para obtener la lista de personas
    List<Persona> obtenerPersonas() {
        return personas
    }

    // Función para obtener la lista de nombres de las personas
    List<String> obtenerNombresPersonas() {
        return personas.collect { it.nombre }
    }
}

// Clase principal del programa
class Programa {
    static void main(String[] args) {
        // Creamos un gestor de personas
        GestorPersonas gestorPersonas = new GestorPersonas()

        // Creamos algunas personas
        Persona persona1 = new Persona(nombre: "Juan", apellido: "García", edad: 25)
        Persona persona2 = new Persona(nombre: "María", apellido: "Pérez", edad: 30)
        Persona persona3 = new Persona(nombre: "Pedro", apellido: "López", edad: 35)

        // Añadimos las personas al gestor
        gestorPersonas.añadirPersona(persona1)
        gestorPersonas.añadirPersona(persona2)
        gestorPersonas.añadirPersona(persona3)

        // Obtenemos la lista de personas
        List<Persona> personas = gestorPersonas.obtenerPersonas()

        // Imprimimos la lista de personas
        personas.each { Persona persona ->
            println "${persona.nombre} ${persona.apellido} (${persona.edad})"
        }

        // Obtenemos la lista de nombres de las personas
        List<String> nombresPersonas = gestorPersonas.obtenerNombresPersonas()

        // Imprimimos la lista de nombres de las personas
        println "Nombres de las personas:"
        nombresPersonas.each { String nombre ->
            println nombre
        }

        // Eliminamos una persona de la lista
        gestorPersonas.eliminarPersona(persona2)

        // Obtenemos la lista de personas
        personas = gestorPersonas.obtenerPersonas()

        // Imprimimos la lista de personas
        println "Lista de personas después de eliminar a María Pérez:"
        personas.each { Persona persona ->
            println "${persona.nombre} ${persona.apellido} (${persona.edad})"
        }
    }
}
```

Explicación del código:

* La clase `Persona` representa objetos de tipo persona, con atributos como el nombre, el apellido y la edad.
* La clase `GestorPersonas` gestiona la lista de personas, permitiendo añadir, eliminar y obtener la lista de personas.
* La clase `Programa` es la clase principal del programa, que crea un gestor de personas, añade algunas personas al gestor, obtiene y imprime la lista de personas y los nombres de las personas, y elimina una persona de la lista.

Este código es un ejemplo de un programa complejo en Groovy que utiliza clases, objetos, listas y funciones para gestionar una lista de personas. El código es bastante completo y cubre varios aspectos de la programación en Groovy.