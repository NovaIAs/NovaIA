```groovy
import groovy.transform.CompileStatic

// Definimos una clase llamada "Persona" con propiedades para el nombre, la edad y el género.
@CompileStatic
class Persona {
    String nombre
    int edad
    String género

    // Método para saludar de forma personalizada según el género y el nombre de la persona.
    String saludar() {
        if (género == "Masculino") {
            return "Hola, señor ${nombre}."
        } else if (género == "Femenino") {
            return "Hola, señora ${nombre}."
        } else {
            return "Hola, ${nombre}."
        }
    }
}

// Creamos una lista de personas con diferentes nombres, edades y géneros.
List<Persona> personas = [
    new Persona(nombre: "Juan", edad: 25, género: "Masculino"),
    new Persona(nombre: "María", edad: 30, género: "Femenino"),
    new Persona(nombre: "Pablo", edad: 18, género: "Masculino"),
    new Persona(nombre: "Ana", edad: 20, género: "Femenino"),
    new Persona(nombre: "Carlos", edad: 40, género: "Masculino"),
    new Persona(nombre: "Rosa", edad: 35, género: "Femenino"),
    new Persona(nombre: "Diego", edad: 22, género: "Masculino"),
    new Persona(nombre: "Laura", edad: 28, género: "Femenino"),
    new Persona(nombre: "Fernando", edad: 50, género: "Masculino"),
    new Persona(nombre: "Isabel", edad: 45, género: "Femenino")
]

// Recorremos la lista de personas y saludamos a cada una de ellas utilizando el método "saludar()".
for (Persona persona in personas) {
    println persona.saludar()
}

// Adicionalmente, imprimimos la cantidad de personas mayores de 30 años en la lista.
int contador = 0
for (Persona persona in personas) {
    if (persona.edad > 30) {
        contador++
    }
}
println "Cantidad de personas mayores de 30 años: ${contador}"

// Ordenamos la lista de personas por edad en orden ascendente utilizando una expresión lambda.
personas.sort { a, b -> a.edad <=> b.edad }

// Imprimimos la lista de personas ordenada por edad.
println "Lista de personas ordenada por edad:"
for (Persona persona in personas) {
    println "${persona.nombre} (${persona.edad} años)"
}

// Concatenamos el nombre y la edad de cada persona utilizando una expresión cursiva.
List<String> nombresYEdades = personas.collect { "${it.nombre} (${it.edad} años)" }

// Imprimimos la lista de nombres y edades de las personas.
println "Lista de nombres y edades de las personas:"
for (String nombreYEdad in nombresYEdades) {
    println nombreYEdad
}

// Agrupamos las personas por género utilizando una expresión cursiva.
Map<String, List<Persona>> personasPorGénero = personas.groupBy { it.género }

// Imprimimos la lista de personas agrupadas por género.
println "Lista de personas agrupadas por género:"
for (Map.Entry<String, List<Persona>> entrada in personasPorGénero) {
    println "${entrada.key}:"
    for (Persona persona in entrada.value) {
        println "\t${persona.nombre} (${persona.edad} años)"
    }
}
```

Este código en Groovy crea una clase llamada "Persona" con propiedades para el nombre, la edad y el género. Luego, crea una lista de personas con diferentes nombres, edades y géneros y realiza varias operaciones sobre la lista, incluyendo:

* Saludar a cada persona utilizando el método "saludar()", que devuelve un mensaje personalizado según el género y el nombre de la persona.
* Contar la cantidad de personas mayores de 30 años en la lista.
* Ordenar la lista de personas por edad en orden ascendente utilizando una expresión lambda.
* Concatenar el nombre y la edad de cada persona utilizando una expresión cursiva.
* Agrupar las personas por género utilizando una expresión cursiva.

El código imprime los resultados de cada operación en la consola.