```groovy
// Definir una clase llamada Persona
class Persona {
    String nombre
    String apellido
    int edad
    String profesion

    // Constructor de la clase Persona
    Persona(String nombre, String apellido, int edad, String profesion) {
        this.nombre = nombre
        this.apellido = apellido
        this.edad = edad
        this.profesion = profesion
    }

    // Método para obtener el nombre completo de la persona
    String obtenerNombreCompleto() {
        "$nombre $apellido"
    }
}

// Definir una lista de personas
List<Persona> personas = new ArrayList<>()

// Añadir personas a la lista
personas.add(new Persona("Juan", "García", 30, "Ingeniero"))
personas.add(new Persona("María", "López", 25, "Médica"))
personas.add(new Persona("Pedro", "Sánchez", 40, "Profesor"))

// Imprimir los nombres completos de las personas en la lista
personas.each { persona ->
    println "Nombre completo: ${persona.obtenerNombreCompleto()}"
}

// Filtrar la lista de personas por edad
List<Persona> personasMayoresDe30 = personas.findAll { persona ->
    persona.edad > 30
}

// Imprimir los nombres completos de las personas que son mayores de 30 años
personasMayoresDe30.each { persona ->
    println "Nombre completo: ${persona.obtenerNombreCompleto()}"
}

// Agrupar la lista de personas por profesión
Map<String, List<Persona>> personasPorProfesion = personas.groupBy { persona ->
    persona.profesion
}

// Imprimir los nombres de las profesiones y las personas que ejercen cada profesión
personasPorProfesion.each { profesion, personas ->
    println "Profesión: $profesion"
    personas.each { persona ->
        println " - Nombre completo: ${persona.obtenerNombreCompleto()}"
    }
}
```

Este código crea una clase llamada `Persona` con las propiedades `nombre`, `apellido`, `edad` y `profesion`. Luego se crea una lista de objetos de tipo `Persona` y se añaden tres personas a la lista.

A continuación, se imprimen los nombres completos de las personas en la lista, se filtran las personas que son mayores de 30 años y se imprimen los nombres completos de las personas filtradas.

Finalmente, se agrupa la lista de personas por profesión y se imprimen los nombres de las profesiones y las personas que ejercen cada profesión.