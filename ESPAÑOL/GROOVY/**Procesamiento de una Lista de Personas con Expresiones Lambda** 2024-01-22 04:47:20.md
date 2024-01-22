```groovy
// Definición de una clase Persona con propiedades y métodos
class Persona {
    String nombre
    int edad

    // Método para saludar
    def saludar() {
        println "Hola, mi nombre es $nombre y tengo $edad años"
    }
}

// Creación de una lista de personas
def personas = [
    new Persona(nombre: "Juan", edad: 25),
    new Persona(nombre: "María", edad: 30),
    new Persona(nombre: "Pedro", edad: 35)
]

// Iteración sobre la lista de personas utilizando una expresión lambda
personas.each { persona ->
    println "Nombre: ${persona.nombre}, Edad: ${persona.edad}"
}

// Uso de una expresión lambda para filtrar la lista de personas
def personasMayoresDe30 = personas.findAll { persona ->
    persona.edad > 30
}

// Uso de una expresión lambda para obtener el nombre de las personas
def nombresPersonas = personas.collect { persona ->
    persona.nombre
}

// Uso de una expresión lambda para contar el número de personas en la lista
def numeroPersonas = personas.size()

// Uso de una expresión lambda para encontrar la persona más joven
def personaMasJoven = personas.min { persona ->
    persona.edad
}

// Uso de una expresión lambda para encontrar la persona más mayor
def personaMasMayor = personas.max { persona ->
    persona.edad
}

// Impresión de resultados
println "Lista de personas mayores de 30 años:"
personasMayoresDe30.each { persona ->
    println "Nombre: ${persona.nombre}, Edad: ${persona.edad}"
}

println "Lista de nombres de personas:"
nombresPersonas.each { nombre ->
    println nombre
}

println "Número de personas en la lista: $numeroPersonas"

println "Persona más joven:"
println "Nombre: ${personaMasJoven.nombre}, Edad: ${personaMasJoven.edad}"

println "Persona más mayor:"
println "Nombre: ${personaMasMayor.nombre}, Edad: ${personaMasMayor.edad}"
```

**Explicación:**

* El código define una clase `Persona` con las propiedades `nombre` y `edad`, así como un método `saludar`.
* Se crea una lista de personas utilizando la clase `Persona`.
* Se utiliza una expresión lambda para iterar sobre la lista de personas e imprimir su nombre y edad.
* Se utiliza una expresión lambda para filtrar la lista de personas y obtener las personas mayores de 30 años.
* Se utiliza una expresión lambda para obtener el nombre de las personas.
* Se utiliza una expresión lambda para contar el número de personas en la lista.
* Se utiliza una expresión lambda para encontrar la persona más joven y la persona más mayor de la lista.
* Se imprimen los resultados en la consola.

Este código muestra algunas de las características más comunes de las expresiones lambda en Groovy, como su uso para iterar sobre colecciones, filtrar colecciones, obtener valores de colecciones y realizar cálculos en colecciones.