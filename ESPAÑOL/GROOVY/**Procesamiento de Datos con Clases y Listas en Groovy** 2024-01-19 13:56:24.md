```groovy
// Definimos una clase llamada "Persona" con atributos y métodos
class Persona {
    String nombre
    int edad
    String ciudad

    // Método para saludar
    def saludar() {
        println "Hola, mi nombre es ${nombre} y tengo ${edad} años."
    }
}

// Creamos un objeto de la clase "Persona"
persona1 = new Persona(nombre: "Juan", edad: 25, ciudad: "Madrid")

// Llamamos al método "saludar" del objeto "persona1"
persona1.saludar()

// Definimos una lista de objetos de la clase "Persona"
personas = [
    new Persona(nombre: "María", edad: 30, ciudad: "Barcelona"),
    new Persona(nombre: "Pedro", edad: 35, ciudad: "Valencia"),
    new Persona(nombre: "Ana", edad: 40, ciudad: "Sevilla")
]

// Recorremos la lista de objetos "personas" e imprimimos sus nombres
personas.each { persona ->
    println persona.nombre
}

// Filtramos la lista de objetos "personas" para obtener sólo los que son mayores de 30 años
personasMayores30 = personas.findAll { persona -> persona.edad > 30 }

// Imprimimos los nombres de las personas mayores de 30 años
personasMayores30.each { persona ->
    println persona.nombre
}

// Agrupamos la lista de objetos "personas" por ciudad y contamos el número de personas en cada ciudad
personasPorCiudad = personas.groupBy { persona -> persona.ciudad }
personasPorCiudad.each { ciudad, personas ->
    println "En ${ciudad} hay ${personas.size()} personas."
}

// Ordenamos la lista de objetos "personas" por edad de forma descendente
personasOrdenadasPorEdad = personas.sort { a, b -> b.edad - a.edad }

// Imprimimos los nombres de las personas ordenadas por edad de forma descendente
personasOrdenadasPorEdad.each { persona ->
    println persona.nombre
}
```

Este código es un ejemplo de código complejo en Groovy que realiza diversas operaciones con una lista de objetos de la clase "Persona". El código incluye la definición de la clase "Persona", la creación de objetos de esta clase, el recorrido de una lista de objetos, el filtrado de una lista de objetos, el agrupamiento de una lista de objetos y la ordenación de una lista de objetos. El código está bien documentado con comentarios que explican lo que hace cada parte del código.