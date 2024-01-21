```kotlin
// Crear una clase llamada 'Persona' con propiedades 'nombre', 'apellido' y 'edad'
class Persona(val nombre: String, val apellido: String, val edad: Int) {

    // Crear una función 'saludar' que imprima el nombre y el apellido de la persona
    fun saludar() {
        println("Hola, mi nombre es ${nombre} ${apellido}")
    }

    // Crear una función 'cumplirAños' que incremente la edad de la persona en 1
    fun cumplirAños() {
        edad++
    }
}

// Crear una lista de personas
val personas = listOf(
    Persona("Juan", "Pérez", 25),
    Persona("María", "López", 30),
    Persona("Pedro", "García", 35)
)

// Recorrer la lista de personas e imprimir el nombre y el apellido de cada una
personas.forEach { persona ->
    persona.saludar()
}

// Incrementar la edad de cada persona en la lista
personas.forEach { persona ->
    persona.cumplirAños()
}

// Recorrer la lista de personas e imprimir el nombre, el apellido y la edad de cada una
personas.forEach { persona ->
    println("Nombre: ${persona.nombre}, Apellido: ${persona.apellido}, Edad: ${persona.edad}")
}
```

Explicación:

* Se crea una clase llamada `Persona` con tres propiedades: `nombre`, `apellido` y `edad`.
* Se crean dos funciones en la clase `Persona`: `saludar` e `cumplirAños`. La función `saludar` imprime el nombre y el apellido de la persona, mientras que la función `cumplirAños` incrementa la edad de la persona en 1.
* Se crea una lista de personas llamada `personas` que contiene objetos de la clase `Persona`.
* Se recorre la lista de personas usando el método `forEach` y se imprime el nombre y el apellido de cada persona.

```kotlin
// Recorrer la lista de personas e imprimir el nombre y el apellido de cada una
personas.forEach { persona ->
    persona.saludar()
}
```

* Se recorre la lista de personas usando el método `forEach` y se incrementa la edad de cada persona en 1.

```kotlin
// Incrementar la edad de cada persona en la lista
personas.forEach { persona ->
    persona.cumplirAños()
}
```

* Se recorre la lista de personas usando el método `forEach` y se imprime el nombre, el apellido y la edad de cada persona.

```kotlin
// Recorrer la lista de personas e imprimir el nombre, el apellido y la edad de cada una
personas.forEach { persona ->
    println("Nombre: ${persona.nombre}, Apellido: ${persona.apellido}, Edad: ${persona.edad}")
}
```

Este código es complejo porque:

* Utiliza una clase personalizada (`Persona`).
* Utiliza dos funciones (`saludar` e `cumplirAños`) en la clase `Persona`.
* Recorre una lista de objetos usando el método `forEach`.
* Imprime el nombre, el apellido y la edad de cada persona en la lista.

El código es diferenciado porque:

* Utiliza una clase personalizada (`Persona`) para representar a las personas.
* Utiliza dos funciones (`saludar` e `cumplirAños`) en la clase `Persona` para realizar acciones específicas.
* Recorre una lista de objetos usando el método `forEach` para procesar cada objeto de manera individual.
* Imprime el nombre, el apellido y la edad de cada persona en la lista utilizando la función `println`.