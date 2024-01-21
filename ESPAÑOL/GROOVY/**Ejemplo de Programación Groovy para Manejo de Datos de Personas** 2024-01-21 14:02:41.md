```groovy
// Definir una clase persona
class Persona {
    String nombre
    int edad
}

// Crear una lista de personas
def personas = new ArrayList<Persona>()

// Agregar personas a la lista
personas.add(new Persona(nombre: "Juan", edad: 20))
personas.add(new Persona(nombre: "María", edad: 25))
personas.add(new Persona(nombre: "Pedro", edad: 30))

// Filtrar la lista de personas por edad mayor a 25 años
def personasMayoresA25 = personas.findAll { it.edad > 25 }

// Imprimir los nombres de las personas mayores a 25 años
personasMayoresA25.each { println it.nombre }

// Crear un mapa de personas por edad
def personasPorEdad = new HashMap<Integer, List<Persona>>()

// Agregar personas al mapa de personas por edad
personas.each { persona ->
    def edad = persona.edad
    if (personasPorEdad.containsKey(edad)) {
        personasPorEdad[edad].add(persona)
    } else {
        personasPorEdad[edad] = new ArrayList<Persona>()
        personasPorEdad[edad].add(persona)
    }
}

// Imprimir el mapa de personas por edad
personasPorEdad.each { edad, personas ->
    println "Edad: $edad"
    personas.each { persona ->
        println "\tNombre: ${persona.nombre}"
    }
}
```

Este código crea una clase `Persona` con dos propiedades: `nombre` y `edad`. Luego, crea una lista de personas y agrega tres personas a la lista. A continuación, filtra la lista de personas por edad mayor a 25 años y imprime los nombres de las personas mayores a 25 años.

Después, crea un mapa de personas por edad. El mapa contiene una clave para cada edad única en la lista de personas. El valor de cada clave es una lista de personas que tienen esa edad.

Finalmente, imprime el mapa de personas por edad. Para cada edad, imprime los nombres de las personas que tienen esa edad.