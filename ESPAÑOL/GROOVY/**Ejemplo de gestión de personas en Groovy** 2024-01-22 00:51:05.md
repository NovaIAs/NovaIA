```groovy
// Definir una clase llamada "Persona" con propiedades básicas de una persona
class Persona {
    String nombre
    String apellido
    int edad
    String direccion
    String telefono
}

// Definir una lista de personas
def personas = new ArrayList<Persona>()

// Añadir algunas personas a la lista
personas.add(new Persona(nombre: "Juan", apellido: "Pérez", edad: 25, direccion: "Calle Mayor 123", telefono: "555-123-4567"))
personas.add(new Persona(nombre: "María", apellido: "López", edad: 30, direccion: "Calle Menor 456", telefono: "555-234-5678"))
personas.add(new Persona(nombre: "Pedro", apellido: "García", edad: 35, direccion: "Calle Real 789", telefono: "555-345-6789"))

// Definir un método para imprimir los datos de una persona
def imprimirPersona(Persona persona) {
    println "Nombre: ${persona.nombre}"
    println "Apellido: ${persona.apellido}"
    println "Edad: ${persona.edad}"
    println "Dirección: ${persona.direccion}"
    println "Teléfono: ${persona.telefono}"
    println ""
}

// Imprimir los datos de cada persona en la lista
personas.each { persona ->
    imprimirPersona(persona)
}

// Definir un método para encontrar a las personas mayores de 30 años
def encontrarMayoresDe30(List<Persona> personas) {
    return personas.findAll { persona -> persona.edad > 30 }
}

// Imprimir los datos de las personas mayores de 30 años
encontrarMayoresDe30(personas).each { persona ->
    imprimirPersona(persona)
}

// Definir un método para ordenar a las personas por edad
def ordenarPorEdad(List<Persona> personas) {
    return personas.sort { persona1, persona2 -> persona1.edad <=> persona2.edad }
}

// Imprimir los datos de las personas ordenadas por edad
ordenarPorEdad(personas).each { persona ->
    imprimirPersona(persona)
}
```

Explicación del código:

1. **Definir una clase llamada "Persona"**: Esta clase representa a una persona y contiene las propiedades básicas de una persona, como el nombre, el apellido, la edad, la dirección y el teléfono.
2. **Definir una lista de personas**: Se crea una lista de personas para almacenar los datos de las personas.
3. **Añadir algunas personas a la lista**: Se añaden algunas personas a la lista utilizando el método `add()` de la lista.
4. **Definir un método para imprimir los datos de una persona**: Este método imprime los datos de una persona en la consola.
5. **Imprimir los datos de cada persona en la lista**: Se utiliza el método `each()` de la lista para imprimir los datos de cada persona en la lista.
6. **Definir un método para encontrar a las personas mayores de 30 años**: Este método utiliza el método `findAll()` de la lista para encontrar a las personas mayores de 30 años.
7. **Imprimir los datos de las personas mayores de 30 años**: Se utiliza el método `each()` de la lista para imprimir los datos de las personas mayores de 30 años.
8. **Definir un método para ordenar a las personas por edad**: Este método utiliza el método `sort()` de la lista para ordenar a las personas por edad.
9. **Imprimir los datos de las personas ordenadas por edad**: Se utiliza el método `each()` de la lista para imprimir los datos de las personas ordenadas por edad.