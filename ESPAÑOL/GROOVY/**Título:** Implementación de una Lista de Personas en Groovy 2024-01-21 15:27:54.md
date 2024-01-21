```groovy
// Definición de la clase Persona
class Persona {
    String nombre;
    int edad;
    String genero;

    // Constructor de la clase Persona
    Persona(String nombre, int edad, String genero) {
        this.nombre = nombre
        this.edad = edad
        this.genero = genero
    }

    // Método para obtener el nombre de la persona
    String obtenerNombre() {
        return nombre
    }

    // Método para obtener la edad de la persona
    int obtenerEdad() {
        return edad
    }

    // Método para obtener el genero de la persona
    String obtenerGenero() {
        return genero
    }

    // Método para imprimir la información de la persona
    void imprimirInformacion() {
        println("Nombre: ${nombre}")
        println("Edad: ${edad}")
        println("Genero: ${genero}")
    }
}

// Definición de la clase ListaPersonas
class ListaPersonas {
    List<Persona> listaPersonas = []

    // Método para añadir una persona a la lista
    void añadirPersona(Persona persona) {
        listaPersonas.add(persona)
    }

    // Método para obtener la lista de personas
    List<Persona> obtenerListaPersonas() {
        return listaPersonas
    }

    // Método para imprimir la información de todas las personas de la lista
    void imprimirInformacionTodasLasPersonas() {
        listaPersonas.each { persona ->
            persona.imprimirInformacion()
            println()
        }
    }
}

// Creación de una lista de personas
ListaPersonas listaPersonas = new ListaPersonas()

// Añadir personas a la lista
listaPersonas.añadirPersona(new Persona("Juan", 20, "Masculino"))
listaPersonas.añadirPersona(new Persona("María", 25, "Femenino"))
listaPersonas.añadirPersona(new Persona("Pedro", 30, "Masculino"))

// Imprimir la información de todas las personas de la lista
listaPersonas.imprimirInformacionTodasLasPersonas()
```

**Explicación:**

* Se define la clase Persona con sus atributos y métodos.
* Se define la clase ListaPersonas que contiene una lista de personas.
* Se crean objetos de la clase Persona y se añaden a la lista.
* Se imprime la información de todas las personas de la lista.