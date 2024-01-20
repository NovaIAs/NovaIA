```kotlin
// Función principal: punto de entrada del programa
fun main(args: Array<String>) {
    // Variables y constantes
    // x e y son variables mutables (se pueden cambiar)
    var x = 10
    var y = 20

    // z es una constante (no se puede cambiar)
    val z = 30

    // Operadores
    // Operadores aritméticos
    val suma = x + y
    val resta = x - y
    val multiplicacion = x * y
    val division = x / y

    // Operadores lógicos
    // Operador AND
    val resultado = x > 0 && y > 0

    // Operador OR
    val resultado2 = x > 0 || y > 0

    // Operador NOT
    val resultado3 = !resultado2

    // Condicionales
    // Operador if
    if (x > 0) {
        // Bloque de código que se ejecuta si la condición es verdadera
        println("x es positivo")
    } else {
        // Bloque de código que se ejecuta si la condición es falsa
        println("x es negativo o cero")
    }

    // Operador when
    when (x) {
        1 -> println("x es 1")
        2 -> println("x es 2")
        3 -> println("x es 3")
        else -> println("x es otro valor")
    }

    // Ciclos
    // Ciclo for
    for (i in 1..10) {
        // Bloque de código que se ejecuta para cada valor de la variable de iteración
        println(i)
    }

    // Ciclo while
    var contador = 1
    while (contador <= 10) {
        // Bloque de código que se ejecuta mientras la condición sea verdadera
        println(contador)

        // Incrementa el valor de la variable de iteración
        contador++
    }

    // Ciclo do-while
    contador = 1
    do {
        // Bloque de código que se ejecuta al menos una vez
        println(contador)

        // Incrementa el valor de la variable de iteración
        contador++
    } while (contador <= 10)

    // Funciones
    // Función que suma dos números
    fun suma(a: Int, b: Int): Int {
        return a + b
    }

    // Llamada a la función suma
    val resultadoSuma = suma(x, y)
    println("El resultado de la suma es $resultadoSuma")

    // Clases
    // Clase Persona
    class Persona(nombre: String, edad: Int) {
        // Propiedades
        val nombre: String = nombre
        val edad: Int = edad

        // Métodos
        fun saludar() {
            println("Hola, soy $nombre y tengo $edad años")
        }
    }

    // Objeto de la clase Persona
    val persona = Persona("Juan", 25)
    persona.saludar()

    // Arreglos
    val arreglo = intArrayOf(1, 2, 3, 4, 5)

    // Recorrer el arreglo
    for (elemento in arreglo) {
        println(elemento)
    }

    // Mapas
    val mapa = mutableMapOf("nombre" to "Juan", "edad" to 25)

    // Añadir un elemento al mapa
    mapa["apellido"] = "Pérez"

    // Recorrer el mapa
    for ((clave, valor) in mapa) {
        println("$clave: $valor")
    }

    // Conjuntos
    val conjunto = mutableSetOf(1, 2, 3, 4, 5)

    // Añadir un elemento al conjunto
    conjunto.add(6)

    // Recorrer el conjunto
    for (elemento in conjunto) {
        println(elemento)
    }

    // Rangos
    val rango = 1..10

    // Recorrer el rango
    for (numero in rango) {
        println(numero)
    }

    // Lambdas
    // Función lambda que suma dos números
    val sumaLambda = { a: Int, b: Int -> a + b }

    // Llamada a la función lambda
    val resultadoSumaLambda = sumaLambda(x, y)
    println("El resultado de la suma utilizando una función lambda es $resultadoSumaLambda")
}
```

Explicación del código:

* **Variables y constantes:** Las variables y constantes se utilizan para almacenar datos. Las variables son mutables (se pueden cambiar), mientras que las constantes no.
* **Operadores:** Los operadores se utilizan para realizar operaciones entre datos. Hay muchos tipos de operadores, incluyendo operadores aritméticos (suma, resta, multiplicación, división), operadores lógicos (AND, OR, NOT) y operadores de comparación (mayor que, menor que, igual a).
* **Condicionales:** Los condicionales se utilizan para controlar el flujo del programa. Hay dos tipos principales de condicionales: el operador **if** y el operador **when**.
* **Ciclos:** Los ciclos se utilizan para repetir un bloque de código un número determinado de veces. Hay tres tipos principales de ciclos: el ciclo **for**, el ciclo **while** y el ciclo **do-while**.
* **Funciones:** Las funciones son bloques de código que se pueden llamar desde otras partes del programa. Se utilizan para organizar el código y hacerlo más reutilizable.
* **Clases:** Las clases son utilizadas para crear objetos. Los objetos son instancias de clases y contienen datos y métodos.
* **Arreglos:** Los arreglos son colecciones ordenadas de elementos. Se utilizan para almacenar datos relacionados.
* **Mapas:** Los mapas son colecciones de pares clave-valor. Se utilizan para almacenar datos asociados.
* **Conjuntos:** Los conjuntos son colecciones de elementos únicos. Se utilizan para almacenar datos que no necesitan estar ordenados.
* **Rangos:** Los rangos son colecciones de números consecutivos. Se utilizan para generar listas de números y para iterar sobre ellos.
* **Lambdas:** Las lambdas son funciones anónimas. Se utilizan para escribir código de forma más concisa y expresiva.