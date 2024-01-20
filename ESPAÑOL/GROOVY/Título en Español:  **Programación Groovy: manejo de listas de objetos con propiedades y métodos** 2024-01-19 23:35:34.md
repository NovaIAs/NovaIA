```groovy
// Definición de un objeto con propiedades y métodos

class Persona {
    String nombre
    int edad

    // Constructor
    Persona(String nombre, int edad) {
        this.nombre = nombre
        this.edad = edad
    }

    // Métodos
    def saludar() {
        println "Hola, soy $nombre y tengo $edad años."
    }

    def esMayorDeEdad() {
        return edad >= 18
    }
}

// Definición de una lista de objetos de tipo Persona

def personas = [
    new Persona("Juan", 25),
    new Persona("María", 30),
    new Persona("Pedro", 15),
    new Persona("Ana", 40)
]

// Iterar sobre la lista de personas y llamar al método saludar() para cada una de ellas

personas.each { persona ->
    persona.saludar()
}

// Filtrar la lista de personas y obtener solo las que son mayores de edad

def personasMayoresDeEdad = personas.findAll { persona ->
    persona.esMayorDeEdad()
}

// Imprimir los nombres de las personas mayores de edad

println "Las personas mayores de edad son:"
personasMayoresDeEdad.each { persona ->
    println persona.nombre
}
```

Explicación del código:

1. Definición de la clase Persona con sus propiedades y métodos.

2. Creación de una lista de objetos de tipo Persona.

3. Iteración sobre la lista de personas y llamada al método saludar() para cada una de ellas.

4. Filtrado de la lista de personas para obtener solo las que son mayores de edad.

5. Impresión de los nombres de las personas mayores de edad.