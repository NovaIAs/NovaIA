```kotlin
// Define una clase para representar una persona.
class Persona(nombre: String, edad: Int, sueldo: Double) {
    // Atributos de la clase.
    val nombre: String = nombre
    val edad: Int = edad
    var sueldo: Double = sueldo

    // Métodos de la clase.
    fun trabajar() {
        println("La persona ${nombre} está trabajando.")
    }

    fun descansar() {
        println("La persona ${nombre} está descansando.")
    }

    fun aumentarSueldo(porcentaje: Double) {
        sueldo += sueldo * porcentaje / 100
        println("El sueldo de la persona ${nombre} se ha aumentado un ${porcentaje}%.")
    }

    // Método toString para imprimir los datos de la persona.
    override fun toString(): String {
        return "Nombre: ${nombre}, Edad: ${edad}, Sueldo: ${sueldo}"
    }
}

// Define una función para imprimir una lista de personas.
fun imprimirListaPersonas(listaPersonas: List<Persona>) {
    for (persona in listaPersonas) {
        println(persona)
    }
}

// Crea una lista de personas.
val listaPersonas = listOf(
    Persona("Juan", 30, 2000.0),
    Persona("María", 25, 1500.0),
    Persona("Pedro", 40, 3000.0)
)

// Imprime la lista de personas.
imprimirListaPersonas(listaPersonas)

// Aumenta el sueldo de cada persona en un 10%.
for (persona in listaPersonas) {
    persona.aumentarSueldo(10.0)
}

// Imprime la lista de personas nuevamente.
imprimirListaPersonas(listaPersonas)
```

Explicación del código:

* La clase `Persona` representa a una persona con atributos como nombre, edad y sueldo. También tiene métodos para trabajar, descansar y aumentar su sueldo.
* La función `imprimirListaPersonas` se utiliza para imprimir una lista de personas en la consola.
* La lista `listaPersonas` contiene tres objetos de la clase `Persona`.
* El código imprime la lista de personas antes y después de aumentar su sueldo en un 10%.