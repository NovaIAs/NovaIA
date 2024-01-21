```groovy
// Clase que representa una persona
class Persona {
    String nombre
    String apellido
    int edad

    // Constructor de la clase
    Persona(String nombre, String apellido, int edad) {
        this.nombre = nombre
        this.apellido = apellido
        this.edad = edad
    }

    // Método para obtener el nombre completo de la persona
    String obtenerNombreCompleto() {
        return "$nombre $apellido"
    }

    // Método para obtener la edad de la persona
    int obtenerEdad() {
        return edad
    }
}

// Clase que representa una lista de personas
class ListaDePersonas {
    List<Persona> personas

    // Constructor de la clase
    ListaDePersonas() {
        personas = []
    }

    // Método para agregar una persona a la lista
    void agregarPersona(Persona persona) {
        personas.add(persona)
    }

    // Método para obtener la lista de personas
    List<Persona> obtenerPersonas() {
        return personas
    }

    // Método para obtener la cantidad de personas en la lista
    int obtenerCantidadDePersonas() {
        return personas.size()
    }
}

// Clase que representa una aplicación
class Aplicacion {
    // Método principal de la aplicación
    static void main(String[] args) {
        // Crear una lista de personas
        ListaDePersonas listaDePersonas = new ListaDePersonas()

        // Agregar personas a la lista
        listaDePersonas.agregarPersona(new Persona("Juan", "Perez", 25))
        listaDePersonas.agregarPersona(new Persona("Maria", "Gonzalez", 30))
        listaDePersonas.agregarPersona(new Persona("Pedro", "Rodriguez", 35))

        // Obtener la lista de personas
        List<Persona> personas = listaDePersonas.obtenerPersonas()

        // Recorrer la lista de personas e imprimir sus nombres
        for (Persona persona in personas) {
            println "Nombre: $persona.obtenerNombreCompleto() Edad: $persona.obtenerEdad()"
        }

        // Obtener la cantidad de personas en la lista
        int cantidadDePersonas = listaDePersonas.obtenerCantidadDePersonas()

        // Imprimir la cantidad de personas en la lista
        println "Cantidad de personas: $cantidadDePersonas"
    }
}
```

Explicación del código:

* La clase `Persona` representa a una persona y tiene las propiedades `nombre`, `apellido` y `edad`.

* La clase `ListaDePersonas` representa a una lista de personas y tiene las propiedades `personas` y `cantidadDePersonas`.

* La clase `Aplicacion` es la clase principal de la aplicación y contiene el método `main`, que se ejecuta cuando se ejecuta la aplicación.

* En el método `main` se crea una instancia de la clase `ListaDePersonas`, se agregan tres personas a la lista, se obtiene la lista de personas de la lista, se recorre la lista de personas e se imprimen sus nombres y edades, y se imprime la cantidad de personas en la lista.