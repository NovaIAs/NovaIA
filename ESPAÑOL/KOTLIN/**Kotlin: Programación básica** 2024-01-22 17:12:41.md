```kotlin
// Definir una clase con propiedades y métodos
class Persona(val nombre: String, val apellido: String, val edad: Int) {

    // Propiedad calculada para el nombre completo
    val nombreCompleto: String
        get() = "$nombre $apellido"

    // Método para saludar
    fun saludar() {
        println("Hola, mi nombre es $nombreCompleto y tengo $edad años.")
    }
}

// Crear una instancia de la clase Persona
val persona1 = Persona("Juan", "García", 25)

// Llamada a los métodos de la clase Persona
persona1.saludar()
println("Nombre completo: ${persona1.nombreCompleto}")

// Definir una función con parámetros y tipo de retorno
fun sumar(a: Int, b: Int): Int {
    return a + b
}

// Llamada a la función sumar
val resultado = sumar(10, 20)
println("Suma: $resultado")

// Definir una clase genérica con un parámetro de tipo
class Lista<T>(val elementos: Array<T>) {

    // Método para agregar un elemento a la lista
    fun agregar(elemento: T) {
        elementos.plus(elemento)
    }

    // Método para obtener un elemento de la lista por su índice
    fun obtener(indice: Int): T {
        return elementos[indice]
    }
}

// Crear una instancia de la clase Lista con un tipo de elemento específico
val listaDeNumeros = Lista(arrayOf(1, 2, 3, 4, 5))

// Agregar un elemento a la lista
listaDeNumeros.agregar(6)

// Obtener un elemento de la lista por su índice
val elementoObtenido = listaDeNumeros.obtener(2)
println("Elemento obtenido: $elementoObtenido")

// Definir una función lambda
val lambda: (Int, Int) -> Int = { a, b -> a + b }

// Llamada a la función lambda
val resultadoLambda = lambda(10, 20)
println("Resultado lambda: $resultadoLambda")

// Definir una clase anónima
val claseAnonima = object : Comparator<Int> {
    override fun compare(a: Int, b: Int): Int {
        return a - b
    }
}

// Llamada al método de la clase anónima
val resultadoComparacion = claseAnonima.compare(10, 20)
println("Resultado comparación: $resultadoComparacion")

// Definir una expresión when
val numero = 10
when (numero) {
    1 -> println("Uno")
    2 -> println("Dos")
    3 -> println("Tres")
    else -> println("Otro número")
}

// Definir un bucle for con un rango
for (i in 1..10) {
    println("Número: $i")
}

// Definir un bucle while
var i = 1
while (i <= 10) {
    println("Número: $i")
    i++
}

// Definir un bucle do-while
do {
    println("Número: $i")
    i++
} while (i <= 10)

// Crear una instancia de una interfaz
val miInterfaz = object : MiInterfaz {
    override fun metodo1() {
        println("Método 1 implementado")
    }

    override fun metodo2(): Int {
        return 10
    }
}

// Llamada a los métodos de la instancia de la interfaz
miInterfaz.metodo1()
val resultadoInterfaz = miInterfaz.metodo2()
println("Resultado interfaz: $resultadoInterfaz")

// Crear una instancia de una clase abstracta
val miClaseAbstracta = MiClaseAbstracta()

// Llamada a los métodos de la clase abstracta
miClaseAbstracta.metodo1()
miClaseAbstracta.metodo2()

// Crear una instancia de una clase heredada
val miClaseHerdada = MiClaseHerdada()

// Llamada a los métodos de la clase heredada
miClaseHerdada.metodo1()
miClaseHerdada.metodo2()
miClaseHerdada.metodo3()
```

Explicación del código:

* Se define una clase `Persona` con propiedades y métodos, incluida una propiedad calculada para el nombre completo y un método para saludar.
* Se crea una instancia de la clase `Persona` y se llaman a sus métodos.
* Se define una función `sumar` con parámetros y tipo de retorno, y se llama a la función para sumar dos números.
* Se define una clase genérica `Lista` con un parámetro de tipo, y se crean instancias de la clase para listas de números y de cadenas.
* Se definen una función lambda, una clase anónima y una expresión `when`.
* Se definen bucles `for`, `while` y `do-while`.
* Se crean instancias de una interfaz y de una clase abstracta, y se llaman a sus métodos.
* Se crea una instancia de una clase heredada y se llaman a sus métodos.