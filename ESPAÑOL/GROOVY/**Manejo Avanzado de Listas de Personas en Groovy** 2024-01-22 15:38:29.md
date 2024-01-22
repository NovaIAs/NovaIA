```groovy
//Definimos una clase 'Persona' con propiedades nombre y apellido
class Persona {
    String nombre
    String apellido
}

//Definimos una lista de personas
List<Persona> personas = [
    new Persona(nombre: "Juan", apellido: "Garcia"),
    new Persona(nombre: "María", apellido: "Fernández"),
    new Persona(nombre: "Pedro", apellido: "Suárez"),
    new Persona(nombre: "Ana", apellido: "López")
]

//Definimos una función para buscar una persona por nombre y apellido
Persona buscarPersona(String nombre, String apellido) {
    return personas.find {it.nombre == nombre && it.apellido == apellido}
}

//Definimos una función para ordenar la lista de personas por nombre
List<Persona> ordenarPorNombre() {
    return personas.sort {it.nombre}
}

//Definimos una función para ordenar la lista de personas por apellido
List<Persona> ordenarPorApellido() {
    return personas.sort {it.apellido}
}

//Mostramos la lista de personas ordenada por nombre
println "Lista de personas ordenada por nombre:"
println ordenarPorNombre()

//Mostramos la lista de personas ordenada por apellido
println "Lista de personas ordenada por apellido:"
println ordenarPorApellido()

//Buscamos a la persona "Juan García"
Persona persona = buscarPersona("Juan", "García")

//Mostramos el nombre y apellido de la persona encontrada
println "Nombre de la persona encontrada: ${persona.nombre}"
println "Apellido de la persona encontrada: ${persona.apellido}"
```

Explicación:

* La clase `Persona` define las propiedades `nombre` y `apellido` para cada persona.
* La lista `personas` contiene cuatro objetos `Persona` con diferentes nombres y apellidos.
* La función `buscarPersona` busca una persona en la lista por nombre y apellido, y devuelve el objeto `Persona` correspondiente.
* Las funciones `ordenarPorNombre` y `ordenarPorApellido` ordenan la lista de personas por nombre y apellido, respectivamente.
* El código usa las funciones definidas para mostrar la lista de personas ordenada por nombre y apellido, y para buscar a la persona "Juan García" y mostrar su nombre y apellido.