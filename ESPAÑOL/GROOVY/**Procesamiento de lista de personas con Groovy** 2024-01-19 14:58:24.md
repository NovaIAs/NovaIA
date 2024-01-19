```groovy
// Definimos una clase llamada "Persona"
class Persona {
    String nombre
    int edad
    String ciudad

    // Constructor de la clase Persona
    Persona(String nombre, int edad, String ciudad) {
        this.nombre = nombre
        this.edad = edad
        this.ciudad = ciudad
    }

    // Sobreescribimos el método toString() para mostrar los datos de la persona
    @Override
    String toString() {
        "Nombre: $nombre, Edad: $edad, Ciudad: $ciudad"
    }
}

// Definimos una lista de personas
List<Persona> personas = [
    new Persona("Juan", 25, "Madrid"),
    new Persona("María", 30, "Barcelona"),
    new Persona("Pedro", 35, "Valencia")
]

// Imprimimos la lista de personas usando la función collect()
println personas.collect { persona -> persona.toString() }.join('\n')

// Filtramos la lista de personas por edad mayor a 30 años
List<Persona> mayoresDe30 = personas.findAll { persona -> persona.edad > 30 }

// Imprimimos la lista de personas mayores de 30 años
println mayoresDe30.collect { persona -> persona.toString() }.join('\n')

// Ordenamos la lista de personas por edad
List<Persona> ordenadosPorEdad = personas.sort { persona -> persona.edad }

// Imprimimos la lista de personas ordenada por edad
println ordenadosPorEdad.collect { persona -> persona.toString() }.join('\n')

// Agrupamos la lista de personas por ciudad
Map<String, List<Persona>> agrupadosPorCiudad = personas.groupBy { persona -> persona.ciudad }

// Imprimimos el mapa de personas agrupadas por ciudad
agrupadosPorCiudad.each { ciudad, personas ->
    println "Ciudad: $ciudad"
    println personas.collect { persona -> persona.toString() }.join('\n')
}

// Creamos un mapa con los nombres de las personas como claves y sus edades como valores
Map<String, Integer> nombresYEdades = personas.collectEntries { [it.nombre, it.edad] }

// Imprimimos el mapa de nombres y edades
nombresYEdades.each { nombre, edad ->
    println "$nombre: $edad"
}

// Creamos una lista de los nombres de las personas
List<String> nombres = personas.collect { persona -> persona.nombre }

// Imprimimos la lista de nombres
println nombres.join(', ')

// Creamos una lista de las ciudades de las personas
List<String> ciudades = personas.collect { persona -> persona.ciudad }

// Imprimimos la lista de ciudades
println ciudades.join(', ')

// Creamos una lista de las personas con edad mayor a 30 años y que vivan en Madrid
List<Persona> mayoresDe30EnMadrid = personas.findAll { persona -> persona.edad > 30 && persona.ciudad == "Madrid" }

// Imprimimos la lista de personas mayores de 30 años y que vivan en Madrid
println mayoresDe30EnMadrid.collect { persona -> persona.toString() }.join('\n')
```

Este código es un ejemplo complejo de código Groovy que realiza una serie de operaciones sobre una lista de personas. El código define una clase Persona con tres propiedades: nombre, edad y ciudad. Luego, crea una lista de personas y realiza una serie de operaciones sobre ella, como filtrarla, ordenarla y agruparla. También crea un mapa con los nombres de las personas como claves y sus edades como valores, y una lista de los nombres y ciudades de las personas. Finalmente, filtra la lista de personas para obtener una lista de las personas mayores de 30 años y que vivan en Madrid.