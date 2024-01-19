```groovy
// Definición de la clase persona con propiedades y métodos.
class Persona {
    String nombre
    String apellido
    int edad

    Persona(String nombre, String apellido, int edad) {
        this.nombre = nombre
        this.apellido = apellido
        this.edad = edad
    }

    String getNombreCompleto() {
        return "$nombre $apellido"
    }
}

// Definición de una clase que representa una lista de personas.
class ListaPersonas {
    List<Persona> personas

    ListaPersonas() {
        personas = new ArrayList<>()
    }

    void añadirPersona(Persona persona) {
        personas.add(persona)
    }

    List<Persona> obtenerPersonas() {
        return personas
    }
}

// Función que recibe una lista de personas y devuelve una lista con los nombres completos de las personas.
List<String> obtenerNombresCompletos(ListaPersonas listaPersonas) {
    List<String> nombresCompletos = new ArrayList<>()
    for (Persona persona : listaPersonas.obtenerPersonas()) {
        nombresCompletos.add(persona.getNombreCompleto())
    }
    return nombresCompletos
}

// Función que recibe una lista de personas y devuelve una lista con las personas mayores de edad.
List<Persona> obtenerPersonasMayoresDeEdad(ListaPersonas listaPersonas) {
    List<Persona> personasMayoresDeEdad = new ArrayList<>()
    for (Persona persona : listaPersonas.obtenerPersonas()) {
        if (persona.edad >= 18) {
            personasMayoresDeEdad.add(persona)
        }
    }
    return personasMayoresDeEdad
}

// Función principal del programa.
void main() {
    // Creación de una lista de personas.
    ListaPersonas listaPersonas = new ListaPersonas()

    // Añadir personas a la lista.
    listaPersonas.añadirPersona(new Persona("Juan", "García", 25))
    listaPersonas.añadirPersona(new Persona("María", "López", 30))
    listaPersonas.añadirPersona(new Persona("Pedro", "Sánchez", 40))

    // Obtener los nombres completos de las personas.
    List<String> nombresCompletos = obtenerNombresCompletos(listaPersonas)

    // Imprimir los nombres completos de las personas.
    println("Nombres completos:")
    for (String nombreCompleto : nombresCompletos) {
        println(nombreCompleto)
    }

    // Obtener las personas mayores de edad.
    List<Persona> personasMayoresDeEdad = obtenerPersonasMayoresDeEdad(listaPersonas)

    // Imprimir las personas mayores de edad.
    println("Personas mayores de edad:")
    for (Persona persona : personasMayoresDeEdad) {
        println(persona.getNombreCompleto())
    }
}
```

Este código crea una clase `Persona` con las propiedades `nombre`, `apellido` y `edad`. También define una clase `ListaPersonas` que contiene una lista de personas.

La función `obtenerNombresCompletos` recibe una lista de personas y devuelve una lista con los nombres completos de las personas. La función `obtenerPersonasMayoresDeEdad` recibe una lista de personas y devuelve una lista con las personas mayores de edad.

La función `main` crea una lista de personas, añade personas a la lista y luego llama a las funciones `obtenerNombresCompletos` y `obtenerPersonasMayoresDeEdad` para obtener los nombres completos de las personas y las personas mayores de edad, respectivamente.

Finalmente, imprime los nombres completos de las personas y las personas mayores de edad en la consola.