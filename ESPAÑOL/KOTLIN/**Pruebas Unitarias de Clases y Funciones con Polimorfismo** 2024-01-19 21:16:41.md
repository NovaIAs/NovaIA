```kotlin
// Importación de la biblioteca de pruebas de Kotlin
import kotlin.test.Test
import kotlin.test.assertEquals

// Clase `Persona`
class Persona(nombre: String, edad: Int) {
    // Propiedades
    var nombre: String = nombre
    var edad: Int = edad

    // Función para saludar
    fun saludar() {
        println("Hola, mi nombre es $nombre y tengo $edad años.")
    }
}

// Clase `Perro`
class Perro(nombre: String, edad: Int) {
    // Propiedades
    var nombre: String = nombre
    var edad: Int = edad

    // Función para ladrar
    fun ladrar() {
        println("Guau! Mi nombre es $nombre y tengo $edad años.")
    }
}

// Clase `Gato`
class Gato(nombre: String, edad: Int) {
    // Propiedades
    var nombre: String = nombre
    var edad: Int = edad

    // Función para maullar
    fun maullar() {
        println("Miau! Mi nombre es $nombre y tengo $edad años.")
    }
}

// Función `imprimirAnimal`
fun imprimirAnimal(animal: Animal) {
    // Si el animal es un `Perro`
    if (animal is Perro) {
        animal.ladrar()
    }
    // Si el animal es un `Gato`
    else if (animal is Gato) {
        animal.maullar()
    }
    // Si el animal es una `Persona`
    else {
        animal.saludar()
    }
}

// Función `crearAnimal`
fun crearAnimal(tipo: String, nombre: String, edad: Int): Animal {
    // Si el tipo es "perro"
    if (tipo == "perro") {
        return Perro(nombre, edad)
    }
    // Si el tipo es "gato"
    else if (tipo == "gato") {
        return Gato(nombre, edad)
    }
    // Si el tipo es "persona"
    else if (tipo == "persona") {
        return Persona(nombre, edad)
    }
    // Si el tipo no es válido
    else {
        throw IllegalArgumentException("Tipo de animal no válido: $tipo")
    }
}

// Pruebas unitarias

@Test
fun `perro ladra cuando se le pide`() {
    val perro = Perro("Rex", 5)
    assertEquals("Guau! Mi nombre es Rex y tengo 5 años.", perro.ladrar())
}

@Test
fun `gato maulla cuando se le pide`() {
    val gato = Gato("Miau", 3)
    assertEquals("Miau! Mi nombre es Miau y tengo 3 años.", gato.maullar())
}

@Test
fun `persona saluda cuando se le pide`() {
    val persona = Persona("Juan", 25)
    assertEquals("Hola, mi nombre es Juan y tengo 25 años.", persona.saludar())
}

@Test
fun `imprimirAnimal imprime la información del animal`() {
    val perro = Perro("Rex", 5)
    val gato = Gato("Miau", 3)
    val persona = Persona("Juan", 25)
    assertEquals("Guau! Mi nombre es Rex y tengo 5 años.", imprimirAnimal(perro))
    assertEquals("Miau! Mi nombre es Miau y tengo 3 años.", imprimirAnimal(gato))
    assertEquals("Hola, mi nombre es Juan y tengo 25 años.", imprimirAnimal(persona))
}

@Test
fun `crearAnimal crea instancias de animales`() {
    val perro = crearAnimal("perro", "Rex", 5)
    val gato = crearAnimal("gato", "Miau", 3)
    val persona = crearAnimal("persona", "Juan", 25)
    assert(perro is Perro)
    assert(gato is Gato)
    assert(persona is Persona)
    assertEquals("Rex", perro.nombre)
    assertEquals(5, perro.edad)
    assertEquals("Miau", gato.nombre)
    assertEquals(3, gato.edad)
    assertEquals("Juan", persona.nombre)
    assertEquals(25, persona.edad)
}
```

Explicación del código:

* Se definen tres clases: `Persona`, `Perro` y `Gato`. Cada clase tiene dos propiedades: `nombre` y `edad`, y dos funciones: una para saludar (`saludar()`), una para ladrar (`ladrar()`) y una para maullar (`maullar()`) respectivamente.
* Se define una función `imprimirAnimal()` que imprime la información del animal pasado como parámetro.
* Se define una función `crearAnimal()` que crea una instancia de la clase `Perro`, `Gato` o `Persona` dependiendo del tipo pasado como parámetro.
* Se definen pruebas unitarias para las clases y funciones.

Este código es complejo porque:

* Utiliza múltiples clases y funciones.
* Utiliza polimorfismo para imprimir la información del animal.
* Utiliza una función para crear instancias de diferentes clases.
* Utiliza pruebas unitarias para verificar el comportamiento de las clases y funciones.