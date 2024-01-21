```kotlin
// 1. Definir una clase para representar un perro
class Perro(val nombre: String, val raza: String, val edad: Int) {

    // Propiedades
    var peso: Double = 0.0
    var altura: Double = 0.0

    // Métodos
    fun ladrar() {
        println("¡Guau!")
    }

    fun comer(comida: String) {
        println("El perro $nombre está comiendo $comida.")
    }

    fun correr() {
        println("El perro $nombre está corriendo.")
    }

    fun dormir() {
        println("El perro $nombre está durmiendo.")
    }

    override fun toString(): String {
        return "Nombre: $nombre, Raza: $raza, Edad: $edad, Peso: $peso, Altura: $altura"
    }
}

// 2. Crear una función para generar una lista de perros aleatorios
fun generarListaPerros(cantidad: Int): List<Perro> {
    val listaPerros = mutableListOf<Perro>()
    for (i in 1..cantidad) {
        val nombre = "Perro $i"
        val raza = "Raza $i"
        val edad = (1..10).random()
        val perro = Perro(nombre, raza, edad)
        listaPerros.add(perro)
    }
    return listaPerros
}

// 3. Crear una función para encontrar el perro más viejo de una lista
fun encontrarPerroMasViejo(listaPerros: List<Perro>): Perro? {
    var perroMasViejo: Perro? = null
    for (perro in listaPerros) {
        if (perroMasViejo == null || perro.edad > perroMasViejo.edad) {
            perroMasViejo = perro
        }
    }
    return perroMasViejo
}

// 4. Crear una función para encontrar el perro más pesado de una lista
fun encontrarPerroMasPesado(listaPerros: List<Perro>): Perro? {
    var perroMasPesado: Perro? = null
    for (perro in listaPerros) {
        if (perroMasPesado == null || perro.peso > perroMasPesado.peso) {
            perroMasPesado = perro
        }
    }
    return perroMasPesado
}

// 5. Crear una función para encontrar el perro más alto de una lista
fun encontrarPerroMasAlto(listaPerros: List<Perro>): Perro? {
    var perroMasAlto: Perro? = null
    for (perro in listaPerros) {
        if (perroMasAlto == null || perro.altura > perroMasAlto.altura) {
            perroMasAlto = perro
        }
    }
    return perroMasAlto
}

// 6. Crear una función para imprimir los datos de un perro
fun imprimirDatosPerro(perro: Perro) {
    println("Nombre: ${perro.nombre}")
    println("Raza: ${perro.raza}")
    println("Edad: ${perro.edad}")
    println("Peso: ${perro.peso}")
    println("Altura: ${perro.altura}")
}

// 7. Crear una función principal para probar las funciones anteriores
fun main() {
    // Generar una lista de perros aleatorios
    val listaPerros = generarListaPerros(10)

    // Imprimir la lista de perros
    println("Lista de perros:")
    for (perro in listaPerros) {
        println(perro)
    }

    // Encontrar el perro más viejo
    val perroMasViejo = encontrarPerroMasViejo(listaPerros)
    println("El perro más viejo es:")
    if (perroMasViejo != null) {
        imprimirDatosPerro(perroMasViejo)
    } else {
        println("No hay perros en la lista.")
    }

    // Encontrar el perro más pesado
    val perroMasPesado = encontrarPerroMasPesado(listaPerros)
    println("El perro más pesado es:")
    if (perroMasPesado != null) {
        imprimirDatosPerro(perroMasPesado)
    } else {
        println("No hay perros en la lista.")
    }

    // Encontrar el perro más alto
    val perroMasAlto = encontrarPerroMasAlto(listaPerros)
    println("El perro más alto es:")
    if (perroMasAlto != null) {
        imprimirDatosPerro(perroMasAlto)
    } else {
        println("No hay perros en la lista.")
    }
}
```

Explicación del código:

1. **Clase Perro:** Se define una clase llamada `Perro` que representa a un perro con propiedades como `nombre`, `raza`, `edad`, `peso` y `altura`. También incluye métodos para ladrar, comer, correr y dormir.

2. **Función `generarListaPerros`:** Esta función genera una lista de perros aleatorios con nombres, razas y edades aleatorias.

3. **Función `encontrarPerroMasViejo`:** Esta función encuentra el perro más viejo de una lista de perros.

4. **Función `encontrarPerroMasPesado`:** Esta función encuentra el perro más pesado de una lista de perros.

5. **Función `encontrarPerroMasAlto`:** Esta función encuentra el perro más alto de una lista de perros.

6. **Función `imprimirDatosPerro`:** Esta función imprime los datos de un perro, como su nombre, raza, edad, peso y altura.

7. **Función `main`:** Esta es la función principal del programa. Genera una lista de perros aleatorios, imprime la lista, encuentra el perro más viejo, más pesado y más alto, y luego imprime los datos de estos perros.

Este código es complejo porque incluye varias funciones con diferentes propósitos y utiliza conceptos de programación como clases, listas y bucles. También utiliza objetos `null` para manejar casos en los que no hay perros en la lista. Este código es un buen ejemplo de cómo se puede usar Kotlin para crear programas complejos y eficientes.