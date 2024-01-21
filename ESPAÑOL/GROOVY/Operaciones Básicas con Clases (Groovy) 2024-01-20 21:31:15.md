```groovy
// Definimos una clase llamada "Persona"
class Persona {

    // Atributos de la clase
    String nombre
    String apellido
    int edad

    // Constructor de la clase
    Persona(String nombre, String apellido, int edad) {
        this.nombre = nombre
        this.apellido = apellido
        this.edad = edad
    }

    // Métodos de la clase
    String getNombreCompleto() {
        return "${nombre} ${apellido}"
    }

    int getEdad() {
        return edad
    }

    void setEdad(int edad) {
        this.edad = edad
    }
}

// Creamos una lista de personas
List<Persona> personas = []

// Agregamos algunas personas a la lista
personas << new Persona("Juan", "Pérez", 25)
personas << new Persona("María", "Gómez", 30)
personas << new Persona("Pedro", "Rodríguez", 35)

// Recorremos la lista de personas e imprimimos sus nombres completos
println("Nombres completos de las personas:")
personas.each { persona ->
    println persona.getNombreCompleto()
}

// Obtenemos la edad promedio de las personas
int edadPromedio = personas.sum { persona -> persona.getEdad() } / personas.size()

// Imprimimos la edad promedio
println("Edad promedio de las personas: ${edadPromedio}")

// Filtramos las personas mayores de 30 años
List<Persona> personasMayoresDe30 = personas.findAll { persona -> persona.getEdad() > 30 }

// Imprimimos los nombres completos de las personas mayores de 30 años
println("Nombres completos de las personas mayores de 30 años:")
personasMayoresDe30.each { persona ->
    println persona.getNombreCompleto()
}

// Ordenamos las personas por edad
List<Persona> personasOrdenadasPorEdad = personas.sort { persona1, persona2 -> persona1.getEdad() <=> persona2.getEdad() }

// Imprimimos los nombres completos de las personas ordenadas por edad
println("Nombres completos de las personas ordenadas por edad:")
personasOrdenadasPorEdad.each { persona ->
    println persona.getNombreCompleto()
}
```

Este código define una clase llamada "Persona" con los atributos "nombre", "apellido" y "edad". También define una lista de personas y agrega algunas personas a la lista.

Luego, el código recorre la lista de personas e imprime sus nombres completos. También obtiene la edad promedio de las personas, filtra las personas mayores de 30 años e imprime sus nombres completos.

Finalmente, el código ordena las personas por edad e imprime sus nombres completos.