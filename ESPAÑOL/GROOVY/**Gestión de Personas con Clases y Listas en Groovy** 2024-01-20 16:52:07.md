```groovy
// Definición de una clase Persona con propiedades y métodos
class Persona {
    String nombre
    int edad
    String ciudad

    // Constructor de la clase Persona
    Persona(String nombre, int edad, String ciudad) {
        this.nombre = nombre
        this.edad = edad
        this.ciudad = ciudad
    }

    // Método para obtener el nombre de la persona
    String getNombre() {
        return nombre
    }

    // Método para obtener la edad de la persona
    int getEdad() {
        return edad
    }

    // Método para obtener la ciudad de la persona
    String getCiudad() {
        return ciudad
    }

    // Método para imprimir los datos de la persona
    void imprimirDatos() {
        println "Nombre: $nombre"
        println "Edad: $edad"
        println "Ciudad: $ciudad"
    }
}

// Definición de una clase ListaPersonas para gestionar una lista de personas
class ListaPersonas {
    List<Persona> personas = []

    // Método para añadir una persona a la lista
    void añadirPersona(Persona persona) {
        personas << persona
    }

    // Método para eliminar una persona de la lista
    void eliminarPersona(Persona persona) {
        personas.remove(persona)
    }

    // Método para obtener la lista de personas
    List<Persona> getPersonas() {
        return personas
    }

    // Método para imprimir los datos de todas las personas de la lista
    void imprimirDatos() {
        for (Persona persona in personas) {
            persona.imprimirDatos()
        }
    }
}

// Creación de una instancia de la clase ListaPersonas
ListaPersonas listaPersonas = new ListaPersonas()

// Añadir personas a la lista
listaPersonas.añadirPersona(new Persona("Juan", 25, "Madrid"))
listaPersonas.añadirPersona(new Persona("María", 30, "Barcelona"))
listaPersonas.añadirPersona(new Persona("Pedro", 35, "Sevilla"))

// Imprimir los datos de todas las personas de la lista
listaPersonas.imprimirDatos()

// Eliminar una persona de la lista
listaPersonas.eliminarPersona(new Persona("Pedro", 35, "Sevilla"))

// Imprimir los datos de todas las personas de la lista
listaPersonas.imprimirDatos()
```

Explicación del código:

* Se definen dos clases, `Persona` y `ListaPersonas`. La clase `Persona` representa a una persona con propiedades como el nombre, la edad y la ciudad. La clase `ListaPersonas` representa una lista de personas y proporciona métodos para añadir, eliminar y obtener personas de la lista.
* Se crea una instancia de la clase `ListaPersonas` y se añaden tres personas a la lista.
* Se imprimen los datos de todas las personas de la lista.
* Se elimina una persona de la lista.
* Se imprimen los datos de todas las personas de la lista.