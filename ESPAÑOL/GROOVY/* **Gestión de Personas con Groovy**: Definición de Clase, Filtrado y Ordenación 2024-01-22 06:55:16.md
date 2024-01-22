```groovy
// Definimos una clase llamada "Persona" con propiedades como "nombre", "edad" y "ocupación"
class Persona {
    String nombre
    int edad
    String ocupación

    // Creamos un constructor para inicializar las propiedades de la clase
    Persona(String nombre, int edad, String ocupación) {
        this.nombre = nombre
        this.edad = edad
        this.ocupación = ocupación
    }

    // Definimos un método para obtener el nombre de la persona
    String getNombre() {
        return nombre
    }

    // Definimos un método para obtener la edad de la persona
    int getEdad() {
        return edad
    }

    // Definimos un método para obtener la ocupación de la persona
    String getOcupación() {
        return ocupación
    }

    // Definimos un método para imprimir los datos de la persona
    void imprimirDatos() {
        println "Nombre: $nombre"
        println "Edad: $edad"
        println "Ocupación: $ocupación"
    }
}

// Creamos una lista de objetos de tipo "Persona"
def personas = []

// Añadimos algunas personas a la lista
personas << new Persona("Juan", 25, "Ingeniero")
personas << new Persona("María", 30, "Doctora")
personas << new Persona("Pedro", 40, "Profesor")

// Imprimimos los datos de cada persona en la lista
personas.each { persona ->
    persona.imprimirDatos()
    println() // Salto de línea
}

// Definimos una función para filtrar las personas por edad
def filtrarPorEdad(int edadMinima, int edadMáxima) {
    personas.findAll { persona ->
        persona.edad >= edadMinima && persona.edad <= edadMáxima
    }
}

// Imprimimos los datos de las personas filtradas por edad
def personasFiltradas = filtrarPorEdad(25, 35)
println "Personas filtradas por edad:"
personasFiltradas.each { persona ->
    persona.imprimirDatos()
    println() // Salto de línea
}

// Definimos una función para ordenar las personas por nombre
def ordenarPorNombre() {
    personas.sort { persona1, persona2 ->
        persona1.nombre <=> persona2.nombre
    }
}

// Imprimimos los datos de las personas ordenadas por nombre
ordenarPorNombre()
println "Personas ordenadas por nombre:"
personas.each { persona ->
    persona.imprimirDatos()
    println() // Salto de línea
}
```

Este código define una clase llamada "Persona" con propiedades como "nombre", "edad" y "ocupación". También define métodos para obtener y imprimir los datos de la persona. A continuación, se crea una lista de objetos de tipo "Persona" y se añaden algunas personas a la lista.

Luego, se definen dos funciones, una para filtrar las personas por edad y otra para ordenarlas por nombre. Estas funciones se utilizan para filtrar y ordenar la lista de personas, y los resultados se imprimen en la consola.

Este código muestra cómo utilizar algunas de las características de Groovy, como las clases, los métodos, las listas y las funciones. También muestra cómo utilizar Groovy para filtrar y ordenar datos de forma sencilla y concisa.