```groovy
// Clase Persona
class Persona {
    String nombre
    String apellido
    int edad

    // Constructor
    Persona(String nombre, String apellido, int edad) {
        this.nombre = nombre
        this.apellido = apellido
        this.edad = edad
    }

    // Método para obtener el nombre completo de la persona
    String obtenerNombreCompleto() {
        "$nombre $apellido"
    }

    // Método para obtener la edad de la persona
    int obtenerEdad() {
        edad
    }

    // Método para obtener la persona en formato JSON
    String obtenerJson() {
        "{\"nombre\": \"$nombre\", \"apellido\": \"$apellido\", \"edad\": $edad}"
    }
}

// Clase ListaPersonas
class ListaPersonas {
    List<Persona> personas = []

    // Método para agregar una persona a la lista
    void agregarPersona(Persona persona) {
        personas.add(persona)
    }

    // Método para obtener la lista de personas en formato JSON
    String obtenerJson() {
        "{\"personas\": [${personas.collect { it.obtenerJson() }.join(", ")}]}"
    }
}

// Clase Principal
class Principal {
    static void main(String[] args) {
        // Creamos una lista de personas
        ListaPersonas listaPersonas = new ListaPersonas()

        // Agregamos algunas personas a la lista
        listaPersonas.agregarPersona(new Persona("Juan", "García", 20))
        listaPersonas.agregarPersona(new Persona("María", "López", 25))
        listaPersonas.agregarPersona(new Persona("Pedro", "Sánchez", 30))

        // Obtenemos la lista de personas en formato JSON
        String json = listaPersonas.obtenerJson()

        // Imprimimos la lista de personas en formato JSON
        println json
    }
}
```

Explicación del código:

* La clase `Persona` representa a una persona con su nombre, apellido y edad.
* La clase `ListaPersonas` representa una lista de personas.
* La clase `Principal` es el punto de entrada del programa.
* En el método `main` se crea una lista de personas y se agregan algunas personas a la lista.
* Se obtiene la lista de personas en formato JSON y se imprime en la consola.

El código es complejo porque utiliza varias clases, métodos y expresiones lambda. También utiliza el formato JSON para representar la lista de personas.