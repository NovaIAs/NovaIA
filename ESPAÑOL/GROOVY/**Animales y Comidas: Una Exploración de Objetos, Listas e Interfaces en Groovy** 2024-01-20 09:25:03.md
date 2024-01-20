```groovy
// Definir una clase Animal con atributos y métodos
class Animal {
    String nombre
    int edad
    String raza

    Animal(String nombre, int edad, String raza) {
        this.nombre = nombre
        this.edad = edad
        this.raza = raza
    }

    String ladrar() {
        "¡Guau!"
    }

    void comer() {
        println("El animal está comiendo")
    }
}

// Crear una instancia de la clase Animal
Animal perro = new Animal("Firulais", 5, "Labrador")

// Acceder a los atributos y métodos del objeto
println("El nombre del animal es: ${perro.nombre}")
println("La edad del animal es: ${perro.edad}")
println("La raza del animal es: ${perro.raza}")
println("El animal ladra: ${perro.ladrar()}")
perro.comer()

// Crear una lista de animales
List<Animal> animales = [
    new Animal("Michi", 3, "Gato"),
    new Animal("Pelusa", 2, "Conejo"),
    new Animal("Lorito", 1, "Loro")
]

// Iterar sobre la lista de animales
animales.each { animal ->
    println("El nombre del animal es: ${animal.nombre}")
    println("La edad del animal es: ${animal.edad}")
    println("La raza del animal es: ${animal.raza}")
    println("El animal ladra: ${animal.ladrar()}")
    animal.comer()
    println()
}

// Definir una interfaz Comida con un método comer()
interface Comida {
    void comer()
}

// Crear una clase Pan que implementa la interfaz Comida
class Pan implements Comida {
    void comer() {
        println("El pan está siendo comido")
    }
}

// Crear una clase Queso que implementa la interfaz Comida
class Queso implements Comida {
    void comer() {
        println("El queso está siendo comido")
    }
}

// Crear una lista de comidas
List<Comida> comidas = [
    new Pan(),
    new Queso()
]

// Iterar sobre la lista de comidas
comidas.each { comida ->
    comida.comer()
    println()
}

// Crear una clase Persona con atributos y métodos
class Persona {
    String nombre
    int edad
    String ocupación

    Persona(String nombre, int edad, String ocupación) {
        this.nombre = nombre
        this.edad = edad
        this.ocupación = ocupación
    }

    String hablar() {
        "¡Hola!"
    }

    void trabajar() {
        println("La persona está trabajando")
    }
}

// Crear una instancia de la clase Persona
Persona juan = new Persona("Juan", 25, "Programador")

// Acceder a los atributos y métodos del objeto
println("El nombre de la persona es: ${juan.nombre}")
println("La edad de la persona es: ${juan.edad}")
println("La ocupación de la persona es: ${juan.ocupación}")
println("La persona habla: ${juan.hablar()}")
juan.trabajar()

// Crear una lista de personas
List<Persona> personas = [
    new Persona("María", 30, "Médico"),
    new Persona("Pedro", 40, "Ingeniero"),
    new Persona("Ana", 50, "Profesora")
]

// Iterar sobre la lista de personas
personas.each { persona ->
    println("El nombre de la persona es: ${persona.nombre}")
    println("La edad de la persona es: ${persona.edad}")
    println("La ocupación de la persona es: ${persona.ocupación}")
    println("La persona habla: ${persona.hablar()}")
    persona.trabajar()
    println()
}
```

Explicación del código:

* Se define una clase `Animal` con atributos (nombre, edad, raza) y métodos (ladrar, comer).
* Se crea una instancia de la clase `Animal` y se accede a sus atributos y métodos.
* Se crea una lista de animales y se itera sobre ella, mostrando la información de cada animal.
* Se define una interfaz `Comida` con un método `comer`.
* Se crean dos clases `Pan` y `Queso` que implementan la interfaz `Comida`.
* Se crea una lista de comidas y se itera sobre ella, mostrando la información de cada comida.
* Se define una clase `Persona` con atributos (nombre, edad, ocupación) y métodos (hablar, trabajar).
* Se crea una instancia de la clase `Persona` y se accede a sus atributos y métodos.
* Se crea una lista de personas y se itera sobre ella, mostrando la información de cada persona.