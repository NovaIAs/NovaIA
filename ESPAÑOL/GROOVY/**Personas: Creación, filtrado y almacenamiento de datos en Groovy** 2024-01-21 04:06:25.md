```groovy
// Definir una clase Persona con sus propiedades y métodos
class Persona {
    String nombre
    int edad
    String ciudad

    String saludar() {
        return "Hola, mi nombre es $nombre y tengo $edad años."
    }
}

// Crear una lista de personas
List<Persona> personas = [
    new Persona(nombre: "Juan", edad: 25, ciudad: "Madrid"),
    new Persona(nombre: "María", edad: 30, ciudad: "Barcelona"),
    new Persona(nombre: "Pedro", edad: 35, ciudad: "Valencia")
]

// Iterar sobre la lista de personas y mostrar sus datos
personas.each { persona ->
    println persona.nombre
    println persona.edad
    println persona.ciudad
    println persona.saludar()
    println ""
}

// Definir un cierre para filtrar a las personas mayores de 30 años
def mayoresDe30 = { persona -> persona.edad > 30 }

// Filtrar la lista de personas usando el cierre
List<Persona> mayoresDe30List = personas.findAll(mayoresDe30)

// Iterar sobre la lista de personas mayores de 30 años y mostrar sus datos
mayoresDe30List.each { persona ->
    println persona.nombre
    println persona.edad
    println persona.ciudad
    println persona.saludar()
    println ""
}

// Crear un mapa con las personas como claves y sus ciudades como valores
Map<Persona, String> personasCiudades = [:]

// Llenar el mapa con las personas y sus ciudades
personas.each { persona ->
    personasCiudades.put(persona, persona.ciudad)
}

// Iterar sobre el mapa y mostrar las claves y los valores
personasCiudades.each { persona, ciudad ->
    println "$persona.nombre vive en $ciudad"
}
```

Explicación del código:

1. Definimos una clase Persona con sus propiedades (nombre, edad y ciudad) y un método para saludar.
2. Creamos una lista de personas con algunos datos de ejemplo.
3. Iteramos sobre la lista de personas y mostramos sus datos utilizando el método saludar.
4. Definimos un cierre para filtrar a las personas mayores de 30 años.
5. Filtramos la lista de personas usando el cierre y obtenemos una lista de personas mayores de 30 años.
6. Iteramos sobre la lista de personas mayores de 30 años y mostramos sus datos.
7. Creamos un mapa con las personas como claves y sus ciudades como valores.
8. Llenamos el mapa con las personas y sus ciudades.
9. Iteramos sobre el mapa y mostramos las claves y los valores.