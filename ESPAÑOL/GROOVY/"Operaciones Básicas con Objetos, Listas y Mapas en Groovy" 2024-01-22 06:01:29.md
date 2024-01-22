```groovy
import groovy.json.JsonSlurper

// Definir una clase Persona con propiedades nombre y edad
class Persona {
    String nombre
    int edad
}
// Creando una lista de personas
def listaPersonas = [
        new Persona(nombre: 'Juan', edad: 25),
        new Persona(nombre: 'María', edad: 30),
        new Persona(nombre: 'Pedro', edad: 35)
]
// Imprimiendo los nombres de las personas en la lista
println "Nombres de las personas en la lista:"
listaPersonas.each { persona ->
    println "nombre: ${persona.nombre}"
}
// Ordenando la lista de personas por su edad
listaPersonas.sort { a, b -> a.edad <=> b.edad }
// Imprimiendo los nombres de las personas en la lista ordenada
println "\nNombres de las personas en la lista ordenada por edad:"
listaPersonas.each { persona ->
    println "nombre: ${persona.nombre}"
}
// Creando un mapa de personas con sus edades
def mapaPersonas = listaPersonas.collectEntries { persona ->
    [persona.nombre, persona.edad]
}
// Imprimiendo el mapa de personas
println "\nMapa de personas con sus edades:"
mapaPersonas.each { clave, valor ->
    println "Clave: ${clave}, Valor: ${valor}"
}
// Creando un JSON a partir de la lista de personas
def jsonPersonas = new JsonSlurper().parseText(listaPersonas.collect { persona ->
    ["nombre": persona.nombre, "edad": persona.edad]
}.join(","))
// Imprimiendo el JSON de personas
println "\nJSON de personas:"
println jsonPersonas
```

Explicación del código:

* Se define una clase Persona con dos propiedades: nombre y edad.

* Se crea una lista de objetos Persona con diferentes nombres y edades.

* Se imprimen los nombres de las personas en la lista.

* Se ordena la lista de personas por su edad.

* Se imprimen los nombres de las personas en la lista ordenada.

* Se crea un mapa de personas con sus edades.

* Se imprime el mapa de personas.

* Se crea un JSON a partir de la lista de personas.

* Se imprime el JSON de personas.