```kotlin
// Definimos una clase padre llamada "Persona" con propiedades comunes a todas las personas.
abstract class Persona {
    var nombre: String = ""
    var edad: Int = 0

    // Definimos un constructor primario para inicializar las propiedades de la persona.
    constructor(nombre: String, edad: Int) {
        this.nombre = nombre
        this.edad = edad
    }

    // Definimos un método abstracto llamado "hablar" que será implementado en las clases hijas.
    abstract fun hablar()
}

// Definimos una clase hija llamada "Estudiante" que hereda de la clase "Persona".
class Estudiante(nombre: String, edad: Int, var curso: String) : Persona(nombre, edad) {

    // Sobrescribimos el método "hablar" para que el estudiante diga su nombre, edad y curso.
    override fun hablar() {
        println("Hola, mi nombre es $nombre, tengo $edad años y estoy en el curso $curso.")
    }
}

// Definimos una clase hija llamada "Profesor" que hereda de la clase "Persona".
class Profesor(nombre: String, edad: Int, var materia: String) : Persona(nombre, edad) {

    // Sobrescribimos el método "hablar" para que el profesor diga su nombre, edad y materia.
    override fun hablar() {
        println("Hola, mi nombre es $nombre, tengo $edad años y enseño $materia.")
    }
}

// Definimos una clase hija llamada "Director" que hereda de la clase "Persona".
class Director(nombre: String, edad: Int) : Persona(nombre, edad) {

    // Sobrescribimos el método "hablar" para que el director diga su nombre y edad.
    override fun hablar() {
        println("Hola, mi nombre es $nombre y tengo $edad años.")
    }
}

// Creamos una lista de personas.
val personas = mutableListOf<Persona>()

// Creamos un estudiante, un profesor y un director.
val estudiante = Estudiante("Juan", 20, "Ingeniería de Sistemas")
val profesor = Profesor("María", 40, "Matemáticas")
val director = Director("Pedro", 50)

// Añadimos las personas a la lista.
personas.add(estudiante)
personas.add(profesor)
personas.add(director)

// Recorremos la lista de personas y hacemos que cada una hable.
for (persona in personas) {
    persona.hablar()
}
```

Explicación del código:

* Definimos una clase padre llamada "Persona" con propiedades comunes a todas las personas, como el nombre y la edad.
* Definimos una clase hija llamada "Estudiante" que hereda de la clase "Persona" y añade una propiedad llamada "curso".
* Definimos una clase hija llamada "Profesor" que hereda de la clase "Persona" y añade una propiedad llamada "materia".
* Definimos una clase hija llamada "Director" que hereda de la clase "Persona".
* Definimos una lista de personas y añadimos un estudiante, un profesor y un director a la lista.
* Recorremos la lista de personas y hacemos que cada una hable.

Este código muestra cómo crear clases padre e hijas en Kotlin y cómo utilizar la herencia para reutilizar código y crear nuevas clases a partir de clases existentes.