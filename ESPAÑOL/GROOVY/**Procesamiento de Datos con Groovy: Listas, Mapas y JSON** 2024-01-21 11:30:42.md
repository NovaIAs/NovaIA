```groovy
// Importamos las librerías necesarias
import java.util.List;
import java.util.Map;
import groovy.json.JsonOutput;
import groovy.json.JsonSlurper;

// Definimos una clase Persona con sus atributos y métodos
class Persona {
    String nombre;
    int edad;
    String ciudad;

    Persona(String nombre, int edad, String ciudad) {
        this.nombre = nombre;
        this.edad = edad;
        this.ciudad = ciudad;
    }

    String getNombre() {
        return nombre;
    }

    int getEdad() {
        return edad;
    }

    String getCiudad() {
        return ciudad;
    }

    String toString() {
        return "Persona [nombre: ${nombre}, edad: ${edad}, ciudad: ${ciudad}]"
    }
}

// Creamos una lista de personas
List<Persona> personas = [
    new Persona("Juan", 25, "Sevilla"),
    new Persona("Ana", 30, "Madrid"),
    new Persona("Pedro", 35, "Barcelona"),
    new Persona("María", 40, "Valencia")
];

// Creamos un mapa con los nombres de las personas y sus edades
Map<String, Integer> nombresEdades = personas.collectEntries { [it.nombre, it.edad] }

// Imprimimos la lista de personas
println "Lista de personas:"
personas.each { println it }

// Imprimimos el mapa de nombres y edades
println "Mapa de nombres y edades:"
nombresEdades.each { println "${it.key}: ${it.value}" }

// Convertimos la lista de personas a JSON
String jsonPersonas = JsonOutput.toJson(personas)

// Imprimimos el JSON de personas
println "JSON de personas:"
println jsonPersonas

// Convertimos el mapa de nombres y edades a JSON
String jsonNombresEdades = JsonOutput.toJson(nombresEdades)

// Imprimimos el JSON de nombres y edades
println "JSON de nombres y edades:"
println jsonNombresEdades

// Convertimos el JSON de personas a una lista de personas
List<Persona> personasDesdeJson = new JsonSlurper().parseText(jsonPersonas)

// Imprimimos la lista de personas desde JSON
println "Lista de personas desde JSON:"
personasDesdeJson.each { println it }

// Convertimos el JSON de nombres y edades a un mapa de nombres y edades
Map<String, Integer> nombresEdadesDesdeJson = new JsonSlurper().parseText(jsonNombresEdades)

// Imprimimos el mapa de nombres y edades desde JSON
println "Mapa de nombres y edades desde JSON:"
nombresEdadesDesdeJson.each { println "${it.key}: ${it.value}" }
```

Explicación del código:

* Importamos las librerías necesarias para trabajar con listas, mapas, JSON y conversión entre objetos y JSON.
* Definimos una clase `Persona` con sus atributos y métodos.
* Creamos una lista de personas y un mapa con los nombres de las personas y sus edades.
* Imprimimos la lista de personas y el mapa de nombres y edades.
* Convertimos la lista de personas y el mapa de nombres y edades a JSON.
* Imprimimos el JSON de personas y el JSON de nombres y edades.
* Convertimos el JSON de personas a una lista de personas y el JSON de nombres y edades a un mapa de nombres y edades.
* Imprimimos la lista de personas desde JSON y el mapa de nombres y edades desde JSON.