```groovy
import groovy.transform.CompileStatic

// Clase que representa una persona
@CompileStatic
class Persona {
    String nombre
    String apellido
    int edad

    // Constructor con parámetros
    Persona(String nombre, String apellido, int edad) {
        this.nombre = nombre
        this.apellido = apellido
        this.edad = edad
    }

    // Método que devuelve el nombre completo de la persona
    String getNombreCompleto() {
        "$nombre $apellido"
    }

    // Método que devuelve la edad de la persona
    int getEdad() {
        edad
    }
}

// Clase que representa una lista de personas
@CompileStatic
class ListaPersonas {
    List<Persona> personas

    // Constructor con parámetros
    ListaPersonas(List<Persona> personas) {
        this.personas = personas
    }

    // Método que devuelve la lista de personas
    List<Persona> getPersonas() {
        personas
    }

    // Método que añade una persona a la lista
    void añadirPersona(Persona persona) {
        personas.add(persona)
    }

    // Método que elimina una persona de la lista
    void eliminarPersona(Persona persona) {
        personas.remove(persona)
    }
}

// Clase principal
def main() {
    // Creamos una lista de personas
    def listaPersonas = new ListaPersonas([
        new Persona("Juan", "García", 20),
        new Persona("María", "Pérez", 25),
        new Persona("José", "López", 30)
    ])

    // Añadimos una persona a la lista
    listaPersonas.añadirPersona(new Persona("Ana", "Fernández", 35))

    // Eliminamos una persona de la lista
    listaPersonas.eliminarPersona(new Persona("José", "López", 30))

    // Imprimimos la lista de personas
    listaPersonas.getPersonas().each { persona ->
        println "Nombre: ${persona.getNombreCompleto()}, Edad: ${persona.getEdad()}"
    }
}

// Llamamos al método main() para ejecutar el programa
main()
```

Explicación del código:

* La clase `Persona` representa una persona con sus atributos: `nombre`, `apellido` y `edad`.
* La clase `ListaPersonas` representa una lista de personas con sus métodos: `getPersonas()`, `añadirPersona()` y `eliminarPersona()`.
* La clase principal `main()` crea una lista de personas, añade y elimina una persona, y finalmente imprime la lista de personas.
* El código utiliza cierres (bloques de código que pueden ser pasados como argumentos a otros métodos) para recorrer la lista de personas e imprimir sus nombres y edades.