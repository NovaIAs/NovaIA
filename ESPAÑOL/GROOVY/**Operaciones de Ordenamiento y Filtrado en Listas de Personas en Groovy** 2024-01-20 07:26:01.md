```groovy
// Definición de clases y métodos de Groovy

// Creamos una clase llamada "Persona" con propiedades y métodos
class Persona {
    String nombre
    int edad

    String obtenerNombre() {
        return nombre
    }

    int obtenerEdad() {
        return edad
    }
}

// Creamos una clase llamada "ListaPersonas" que almacena una lista de personas
class ListaPersonas {
    List<Persona> personas

    void agregarPersona(Persona persona) {
        personas.add(persona)
    }

    List<Persona> obtenerPersonas() {
        return personas
    }
}

// Creamos una clase llamada "OperacionesListaPersonas" que realiza operaciones sobre una lista de personas
class OperacionesListaPersonas {
    // Método para ordenar la lista de personas por edad
    List<Persona> ordenarPorEdad(ListaPersonas listaPersonas) {
        List<Persona> listaOrdenada = listaPersonas.obtenerPersonas()
        listaOrdenada.sort { persona1, persona2 -> persona1.edad <=> persona2.edad }
        return listaOrdenada
    }

    // Método para filtrar la lista de personas por edad mínima
    List<Persona> filtrarPorEdadMinima(ListaPersonas listaPersonas, int edadMinima) {
        List<Persona> listaFiltrada = listaPersonas.obtenerPersonas()
        listaFiltrada.removeAll { persona -> persona.edad < edadMinima }
        return listaFiltrada
    }
}

// Creamos una instancia de la clase "OperacionesListaPersonas"
OperacionesListaPersonas operacionesListaPersonas = new OperacionesListaPersonas()

// Creamos una instancia de la clase "ListaPersonas"
ListaPersonas listaPersonas = new ListaPersonas()

// Creamos una lista de personas
List<Persona> personas = [
    new Persona(nombre: "Juan", edad: 25),
    new Persona(nombre: "María", edad: 30),
    new Persona(nombre: "Pedro", edad: 20),
    new Persona(nombre: "Ana", edad: 28)
]

// Agregamos la lista de personas a la instancia de la clase "ListaPersonas"
listaPersonas.personas = personas

// Ordenamos la lista de personas por edad
List<Persona> listaOrdenada = operacionesListaPersonas.ordenarPorEdad(listaPersonas)

// Filtramos la lista de personas por edad mínima
List<Persona> listaFiltrada = operacionesListaPersonas.filtrarPorEdadMinima(listaPersonas, 25)

// Imprimimos la lista de personas ordenada
println "Lista de personas ordenada por edad:"
listaOrdenada.each { persona -> println "${persona.nombre}, ${persona.edad}" }

// Imprimimos la lista de personas filtrada
println "Lista de personas filtrada por edad mínima:"
listaFiltrada.each { persona -> println "${persona.nombre}, ${persona.edad}" }
```

Explicación:

* Definimos las clases `Persona`, `ListaPersonas` y `OperacionesListaPersonas` con sus propiedades y métodos correspondientes.
* Creamos una instancia de la clase `OperacionesListaPersonas` y una instancia de la clase `ListaPersonas`.
* Creamos una lista de personas y la añadimos a la instancia de la clase `ListaPersonas`.
* Ordenamos la lista de personas por edad utilizando el método `ordenarPorEdad` de la clase `OperacionesListaPersonas`.
* Filtramos la lista de personas por edad mínima utilizando el método `filtrarPorEdadMinima` de la clase `OperacionesListaPersonas`.
* Imprimimos la lista de personas ordenada y la lista de personas filtrada.

Este código es complejo porque utiliza varias clases, métodos y colecciones. También utiliza el operador `each` de Groovy para iterar sobre las listas de personas. El código es difícilmente repetible porque es específico para el problema que se está resolviendo.