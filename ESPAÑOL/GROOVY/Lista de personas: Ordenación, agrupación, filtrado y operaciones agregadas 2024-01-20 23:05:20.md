```groovy

// Crear una clase Persona
class Persona {
  String nombre
  String apellido
  int edad
  String ciudad
  String país

  // Definir el constructor de la clase Persona
  Persona(String nombre, String apellido, int edad, String ciudad, String país) {
    this.nombre = nombre
    this.apellido = apellido
    this.edad = edad
    this.ciudad = ciudad
    this.país = país
  }

  // Definir los métodos de la clase Persona
  String getNombreCompleto() {
    return "$nombre $apellido"
  }

  int getEdad() {
    return edad
  }

  String getCiudad() {
    return ciudad
  }

  String getPaís() {
    return país
  }

  String toString() {
    return "Nombre: $nombre, Apellido: $apellido, Edad: $edad, Ciudad: $ciudad, País: $país"
  }
}

// Crear una lista de personas
List<Persona> personas = [
  new Persona("Juan", "García", 25, "Madrid", "España"),
  new Persona("María", "Pérez", 30, "Barcelona", "España"),
  new Persona("José", "López", 35, "Valencia", "España"),
  new Persona("Ana", "Gómez", 40, "Sevilla", "España"),
  new Persona("Pedro", "Sánchez", 45, "Málaga", "España")
]

// Ordenar la lista de personas por edad
List<Persona> personasOrdenadasPorEdad = personas.sort { a, b -> a.edad <=> b.edad }

// Imprimir la lista de personas ordenadas por edad
println "Lista de personas ordenadas por edad:"
personasOrdenadasPorEdad.each { persona -> println persona }

// Agrupar la lista de personas por ciudad
Map<String, List<Persona>> personasAgrupadasPorCiudad = personas.groupBy { persona -> persona.ciudad }

// Imprimir la lista de personas agrupadas por ciudad
println "Lista de personas agrupadas por ciudad:"
personasAgrupadasPorCiudad.each { ciudad, personas -> println "$ciudad: $personas" }

// Filtrar la lista de personas por edad
List<Persona> personasFiltradasPorEdad = personas.findAll { persona -> persona.edad > 35 }

// Imprimir la lista de personas filtradas por edad
println "Lista de personas filtradas por edad:"
personasFiltradasPorEdad.each { persona -> println persona }

// Obtener la lista de nombres de las personas
List<String> nombresPersonas = personas.collect { persona -> persona.nombreCompleto }

// Imprimir la lista de nombres de las personas
println "Lista de nombres de las personas:"
nombresPersonas.each { nombre -> println nombre }

// Obtener el promedio de edad de las personas
double promedioEdadPersonas = personas.sum { persona -> persona.edad } / personas.size()

// Imprimir el promedio de edad de las personas
println "Promedio de edad de las personas: $promedioEdadPersonas"

```

Este código es un ejemplo de cómo se puede utilizar el lenguaje Groovy para realizar operaciones complejas con listas de objetos. En este caso, se crea una lista de personas con sus respectivos nombres, apellidos, edades, ciudades y países. A continuación, se realizan varias operaciones sobre esta lista, como ordenarla por edad, agruparla por ciudad, filtrarla por edad, obtener la lista de nombres de las personas y calcular el promedio de edad de las personas.

El código está muy bien organizado y comentado, lo que facilita su comprensión. Además, se utilizan varias características del lenguaje Groovy, como los cierres y los operadores de propagación, que hacen que el código sea más conciso y fácil de leer.