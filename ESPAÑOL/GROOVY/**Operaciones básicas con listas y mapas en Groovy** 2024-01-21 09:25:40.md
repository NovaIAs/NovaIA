```groovy
// Definimos una clase de dominio llamada 'Persona' con propiedades 'nombre' y 'edad'
class Persona {
    String nombre
    Integer edad
    // Agregamos un método llamado 'hablar' que devuelve un mensaje con el nombre y la edad de la persona
    String hablar() {
        return "Hola, soy ${nombre} y tengo ${edad} años."
    }
}

// Creamos una lista de personas
def personas = [
    new Persona(nombre: "Juan", edad: 20),
    new Persona(nombre: "María", edad: 25),
    new Persona(nombre: "Pedro", edad: 30),
]

// Recorremos la lista de personas e imprimimos el mensaje de cada una
personas.each { persona ->
    println persona.hablar()
}

// Creamos una variable 'suma' para almacenar la suma de las edades de las personas
def suma = 0

// Recorremos la lista de personas y sumamos la edad de cada una a la variable 'suma'
personas.each { persona ->
    suma += persona.edad
}

// Imprimimos la suma de las edades de las personas
println "La suma de las edades de las personas es: ${suma}"

// Buscamos la persona más joven de la lista
def personaMasJoven = personas.min { persona -> persona.edad }

// Imprimimos el nombre y la edad de la persona más joven
println "La persona más joven es: ${personaMasJoven.nombre} (${personaMasJoven.edad} años)"

// Buscamos la persona más mayor de la lista
def personaMasMayor = personas.max { persona -> persona.edad }

// Imprimimos el nombre y la edad de la persona más mayor
println "La persona más mayor es: ${personaMasMayor.nombre} (${personaMasMayor.edad} años)"

// Filtramos la lista de personas para obtener solo las personas mayores de 25 años
def personasMayoresDe25 = personas.findAll { persona -> persona.edad > 25 }

// Imprimimos los nombres de las personas mayores de 25 años
println "Las personas mayores de 25 años son:"
personasMayoresDe25.each { persona ->
    println persona.nombre
}

// Ordenamos la lista de personas por edad de forma ascendente
def personasOrdenadasPorEdad = personas.sort { persona1, persona2 -> persona1.edad <=> persona2.edad }

// Imprimimos los nombres de las personas ordenadas por edad
println "Las personas ordenadas por edad son:"
personasOrdenadasPorEdad.each { persona ->
    println persona.nombre
}

// Creamos un mapa con los nombres de las personas como claves y sus edades como valores
def mapaPersonas = [:]
personas.each { persona ->
    mapaPersonas[persona.nombre] = persona.edad
}

// Imprimimos el mapa de personas
println "El mapa de personas es:"
mapaPersonas.each { nombre, edad ->
    println "Nombre: ${nombre}, Edad: ${edad}"
}
```
Explicación del código:

* Definimos una clase de dominio llamada 'Persona' con propiedades 'nombre' y 'edad'.
* Creamos una lista de personas.
* Recorremos la lista de personas e imprimimos el mensaje de cada una.
* Creamos una variable 'suma' para almacenar la suma de las edades de las personas.
* Recorremos la lista de personas y sumamos la edad de cada una a la variable 'suma'.
* Imprimimos la suma de las edades de las personas.
* Buscamos la persona más joven de la lista.
* Imprimimos el nombre y la edad de la persona más joven.
* Buscamos la persona más mayor de la lista.
* Imprimimos el nombre y la edad de la persona más mayor.
* Filtramos la lista de personas para obtener solo las personas mayores de 25 años.
* Imprimimos los nombres de las personas mayores de 25 años.
* Ordenamos la lista de personas por edad de forma ascendente.
* Imprimimos los nombres de las personas ordenadas por edad.
* Creamos un mapa con los nombres de las personas como claves y sus edades como valores.
* Imprimimos el mapa de personas.