```groovy
// Definimos una clase Persona con sus atributos y métodos
class Persona {
    String nombre, apellidos
    int edad

    String saludar() {
        "Hola, mi nombre es $nombre $apellidos y tengo $edad años"
    }
}

// Creamos una lista de personas
List<Persona> personas = [
    new Persona(nombre: "Juan", apellidos: "García", edad: 20),
    new Persona(nombre: "María", apellidos: "López", edad: 25),
    new Persona(nombre: "Pedro", apellidos: "Sánchez", edad: 30)
]

// Imprimimos los datos de cada persona
personas.each { persona ->
    println persona.saludar()
}

// Definimos un closure para filtrar las personas mayores de edad
def mayoresDeEdad = { persona -> persona.edad >= 18 }

// Filtramos la lista de personas utilizando el closure
List<Persona> personasMayoresDeEdad = personas.findAll(mayoresDeEdad)

// Imprimimos los datos de las personas mayores de edad
personasMayoresDeEdad.each { persona ->
    println persona.saludar()
}

// Definimos un closure para ordenar las personas por edad
def ordenarPorEdad = { persona1, persona2 -> persona1.edad <=> persona2.edad }

// Ordenamos la lista de personas utilizando el closure
List<Persona> personasOrdenadasPorEdad = personas.sort(ordenarPorEdad)

// Imprimimos los datos de las personas ordenadas por edad
personasOrdenadasPorEdad.each { persona ->
    println persona.saludar()
}

// Definimos un closure para agrupar las personas por edad
def agruparPorEdad = { persona -> persona.edad }

// Agrupamos la lista de personas utilizando el closure
Map<Integer, List<Persona>> personasAgrupadasPorEdad = personas.groupBy(agruparPorEdad)

// Imprimimos los datos de las personas agrupadas por edad
personasAgrupadasPorEdad.each { edad, personas ->
    println "Personas con edad $edad:"
    personas.each { persona ->
        println "  - ${persona.saludar()}"
    }
}
```

Explicación del código:

* Definimos una clase `Persona` con sus atributos `nombre`, `apellidos` y `edad`, y un método `saludar()` que devuelve un saludo con el nombre, apellidos y edad de la persona.
* Creamos una lista de personas utilizando el constructor `new Persona(...)`.
* Imprimimos los datos de cada persona utilizando el método `each()` de la lista.
* Definimos un closure `mayoresDeEdad` que devuelve `true` si la edad de la persona es mayor o igual que 18.
* Filtramos la lista de personas utilizando el método `findAll()` de la lista, pasando el closure `mayoresDeEdad` como argumento.
* Imprimimos los datos de las personas mayores de edad utilizando el método `each()` de la lista.
* Definimos un closure `ordenarPorEdad` que compara la edad de dos personas y devuelve un número negativo si la primera es menor que la segunda, un número positivo si la primera es mayor que la segunda, o cero si son iguales.
* Ordenamos la lista de personas utilizando el método `sort()` de la lista, pasando el closure `ordenarPorEdad` como argumento.
* Imprimimos los datos de las personas ordenadas por edad utilizando el método `each()` de la lista.
* Definimos un closure `agruparPorEdad` que devuelve la edad de la persona.
* Agrupamos la lista de personas utilizando el método `groupBy()` de la lista, pasando el closure `agruparPorEdad` como argumento.
* Imprimimos los datos de las personas agrupadas por edad utilizando el método `each()` del mapa.