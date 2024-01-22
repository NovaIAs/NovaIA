```kotlin
// Importamos las bibliotecas necesarias
import java.util.Scanner
import kotlin.random.Random

// Creamos un objeto Scanner para leer la entrada del usuario
val scanner = Scanner(System.`in`)

// Creamos una función para generar números aleatorios
fun generarNumeroAleatorio(min: Int, max: Int): Int {
    return Random.nextInt(min, max + 1)
}

// Creamos una función para comprobar si un número es primo
fun esPrimo(numero: Int): Boolean {
    if (numero <= 1) {
        return false
    }
    for (i in 2 until numero) {
        if (numero % i == 0) {
            return false
        }
    }
    return true
}

// Creamos una función para encontrar el mayor número primo entre dos números
fun encontrarMayorPrimo(min: Int, max: Int): Int {
    var mayorPrimo = 0
    for (i in min..max) {
        if (esPrimo(i) && i > mayorPrimo) {
            mayorPrimo = i
        }
    }
    return mayorPrimo
}

// Pedimos al usuario que introduzca dos números
println("Introduce el número mínimo:")
val min = scanner.nextInt()

println("Introduce el número máximo:")
val max = scanner.nextInt()

// Comprobamos si los números introducidos son válidos
if (min > max) {
    println("El número mínimo no puede ser mayor que el número máximo.")
    return
}

// Generamos un número aleatorio entre los dos números introducidos
val numeroAleatorio = generarNumeroAleatorio(min, max)

// Comprobamos si el número aleatorio es primo
if (esPrimo(numeroAleatorio)) {
    println("El número aleatorio $numeroAleatorio es primo.")
} else {
    println("El número aleatorio $numeroAleatorio no es primo.")
}

// Encontramos el mayor número primo entre los dos números introducidos
val mayorPrimo = encontrarMayorPrimo(min, max)

// Mostramos el mayor número primo encontrado
println("El mayor número primo entre $min y $max es $mayorPrimo.")
```

Este código genera un número aleatorio entre dos números introducidos por el usuario, comprueba si el número aleatorio es primo y encuentra el mayor número primo entre los dos números introducidos.

El código está dividido en diferentes funciones, cada una de las cuales realiza una tarea específica. Esto hace que el código sea más fácil de leer y mantener.

Las funciones utilizan los siguientes tipos de datos:

* **Int:** Un tipo de datos entero de 32 bits.
* **Boolean:** Un tipo de datos booleano que puede tener dos valores: verdadero o falso.

Las funciones utilizan los siguientes operadores:

* **+:** El operador de suma.
* **-:** El operador de resta.
* **\*:** El operador de multiplicación.
* **/:** El operador de división.
* **%:** El operador de módulo.
* **==:** El operador de igualdad.
* **\!=:** El operador de desigualdad.
* **>:** El operador de mayor que.
* **<:** El operador de menor que.
* **>=:** El operador de mayor o igual que.
* **<=:** El operador de menor o igual que.
* **&&:** El operador AND lógico.
* **\|:** El operador OR lógico.
* **!:** El operador NOT lógico.

Las funciones utilizan las siguientes sentencias de control:

* **if:** La sentencia if se utiliza para ejecutar un bloque de código si una condición es verdadera.
* **else:** La sentencia else se utiliza para ejecutar un bloque de código si una condición es falsa.
* **for:** La sentencia for se utiliza para ejecutar un bloque de código un número determinado de veces.
* **while:** La sentencia while se utiliza para ejecutar un bloque de código mientras una condición sea verdadera.
* **do-while:** La sentencia do-while se utiliza para ejecutar un bloque de código al menos una vez y luego mientras una condición sea verdadera.

Las funciones utilizan las siguientes funciones integradas:

* **Random.nextInt:** La función Random.nextInt se utiliza para generar un número aleatorio entre dos números.
* **kotlin.random.Random:** La clase Random se utiliza para generar números aleatorios.