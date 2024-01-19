```groovy
// Definición de la clase Person
class Persona {
    int id
    String nombre
    String apellido
    int edad
    Persona padre
    Persona madre
    List<Persona> hijos

    // Constructor de la clase Person
    Persona(int id, String nombre, String apellido, int edad) {
        this.id = id
        this.nombre = nombre
        this.apellido = apellido
        this.edad = edad
    }

    // Método para obtener el nombre completo de la persona
    String nombreCompleto() {
        "$nombre $apellido"
    }

    // Método para obtener la edad de la persona
    int edad() {
        edad
    }

    // Método para obtener los hijos de la persona
    List<Persona> hijos() {
        hijos
    }

    // Método para agregar un hijo a la persona
    void agregarHijo(Persona hijo) {
        hijos.add(hijo)
    }

    // Método para eliminar un hijo de la persona
    void eliminarHijo(Persona hijo) {
        hijos.remove(hijo)
    }

    // Método para obtener el padre de la persona
    Persona padre() {
        padre
    }

    // Método para establecer el padre de la persona
    void establecerPadre(Persona padre) {
        this.padre = padre
    }

    // Método para obtener la madre de la persona
    Persona madre() {
        madre
    }

    // Método para establecer la madre de la persona
    void establecerMadre(Persona madre) {
        this.madre = madre
    }

    // Método para imprimir la información de la persona
    void mostrarInformacion() {
        println "ID: $id"
        println "Nombre: $nombreCompleto()"
        println "Edad: $edad"
        println "Padre: ${padre?.nombreCompleto()}"
        println "Madre: ${madre?.nombreCompleto()}"
        println "Hijos:"
        hijos.each { hijo ->
            println "  - ${hijo.nombreCompleto()}"
        }
    }
}

// Definición de la clase Familia
class Familia {
    List<Persona> personas

    // Constructor de la clase Familia
    Familia(List<Persona> personas) {
        this.personas = personas
    }

    // Método para obtener la lista de personas de la familia
    List<Persona> personas() {
        personas
    }

    // Método para agregar una persona a la familia
    void agregarPersona(Persona persona) {
        personas.add(persona)
    }

    // Método para eliminar una persona de la familia
    void eliminarPersona(Persona persona) {
        personas.remove(persona)
    }

    // Método para obtener el padre de la familia
    Persona padre() {
        personas.find { it.id == 1 } // Assumption: The father is always the first person in the list
    }

    // Método para obtener la madre de la familia
    Persona madre() {
        personas.find { it.id == 2 } // Assumption: The mother is always the second person in the list
    }

    // Método para obtener los hijos de la familia
    List<Persona> hijos() {
        personas.findAll { it.id > 2 } // Assumption: The children are always the remaining people in the list
    }

    // Método para imprimir la información de la familia
    void mostrarInformacion() {
        println "Familia:"
        personas.each { persona ->
            persona.mostrarInformacion()
            println ""
        }
    }
}

// Creación de las instancias de las personas
Persona padre = new Persona(1, "Juan", "García", 45)
Persona madre = new Persona(2, "María", "López", 40)
Persona hijo1 = new Persona(3, "Pedro", "García", 15)
Persona hijo2 = new Persona(4, "Ana", "García", 12)

// Creación de la instancia de la familia
Familia familia = new Familia([padre, madre, hijo1, hijo2])

// Establecer las relaciones de parentesco entre las personas
padre.establecerHijo(hijo1)
padre.establecerHijo(hijo2)
madre.establecerHijo(hijo1)
madre.establecerHijo(hijo2)
hijo1.establecerPadre(padre)
hijo1.establecerMadre(madre)
hijo2.establecerPadre(padre)
hijo2.establecerMadre(madre)

// Imprimir la información de la familia
familia.mostrarInformacion()
```

Este código define dos clases, `Persona` y `Familia`, que representan a las personas y a las familias, respectivamente. Cada persona tiene un nombre, un apellido, una edad, un padre, una madre y una lista de hijos. Cada familia tiene una lista de personas.

El código crea instancias de las personas y de la familia, y establece las relaciones de parentesco entre ellas.

Finalmente, el código imprime la información de la familia, que incluye el nombre completo, la edad, el padre, la madre y los hijos de cada persona.