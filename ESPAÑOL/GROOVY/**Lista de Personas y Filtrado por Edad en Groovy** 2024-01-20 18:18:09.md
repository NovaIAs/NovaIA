```groovy
// Definición de la clase Persona
class Persona {
    String nombre
    int edad
    String ocupacion
    String ciudad

    // Constructor de la clase Persona
    Persona(String nombre, int edad, String ocupacion, String ciudad) {
        this.nombre = nombre
        this.edad = edad
        this.ocupacion = ocupacion
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

    // Método para obtener la ocupación de la persona
    String getOcupacion() {
        return ocupacion
    }

    // Método para obtener la ciudad de la persona
    String getCiudad() {
        return ciudad
    }

    // Método para imprimir los datos de la persona
    void imprimirDatos() {
        println "Nombre: ${nombre}"
        println "Edad: ${edad}"
        println "Ocupación: ${ocupacion}"
        println "Ciudad: ${ciudad}"
    }
}

// Definición de la lista de personas
List<Persona> personas = new ArrayList<>()

// Añadimos personas a la lista
personas.add(new Persona("Juan", 25, "Ingeniero", "Madrid"))
personas.add(new Persona("María", 30, "Doctora", "Barcelona"))
personas.add(new Persona("Pedro", 35, "Abogado", "Valencia"))
personas.add(new Persona("Ana", 40, "Profesora", "Sevilla"))
personas.add(new Persona("Luis", 45, "Empresario", "Málaga"))

// Recorremos la lista de personas y mostramos sus datos
println "Lista de personas:"
for (Persona persona in personas) {
    persona.imprimirDatos()
    println ""
}

// Filtramos la lista de personas por edad mayor de 30 años
List<Persona> personasMayoresDe30 = personas.findAll { it.edad > 30 }

// Mostramos la lista de personas mayores de 30 años
println "Lista de personas mayores de 30 años:"
for (Persona persona in personasMayoresDe30) {
    persona.imprimirDatos()
    println ""
}
```

Explicación del código:

* Definimos la clase `Persona` que representa a una persona con sus atributos `nombre`, `edad`, `ocupación` y `ciudad`.
* Añadimos personas a la lista `personas`.
* Recorremos la lista de personas y mostramos sus datos.
* Filtramos la lista de personas por edad mayor de 30 años y la guardamos en la lista `personasMayoresDe30`.
* Mostramos la lista de personas mayores de 30 años.