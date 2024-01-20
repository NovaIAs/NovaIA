```kotlin
// Importar las librerías necesarias
import java.util.Scanner
import java.lang.Math

// Crear una clase para representar un punto en el espacio
class Punto(val x: Double, val y: Double) {

    // Método para calcular la distancia entre dos puntos
    fun distancia(otroPunto: Punto): Double {
        return Math.sqrt(Math.pow(x - otroPunto.x, 2.0) + Math.pow(y - otroPunto.y, 2.0))
    }

    // Método para imprimir las coordenadas del punto
    fun imprimir() {
        println("($x, $y)")
    }
}

// Crear una clase para representar una línea recta
class Linea(val puntoInicial: Punto, val puntoFinal: Punto) {

    // Método para calcular la longitud de la línea
    fun longitud(): Double {
        return puntoInicial.distancia(puntoFinal)
    }

    // Método para imprimir la línea
    fun imprimir() {
        println("Línea de ${puntoInicial.imprimir()} a ${puntoFinal.imprimir()}")
    }
}

// Crear una clase para representar un triángulo
class Triangulo(val puntoA: Punto, val puntoB: Punto, val puntoC: Punto) {

    // Método para calcular el área del triángulo
    fun area(): Double {
        val ladoAB = puntoA.distancia(puntoB)
        val ladoBC = puntoB.distancia(puntoC)
        val ladoCA = puntoC.distancia(puntoA)
        val semiperimetro = (ladoAB + ladoBC + ladoCA) / 2
        return Math.sqrt(semiperimetro * (semiperimetro - ladoAB) * (semiperimetro - ladoBC) * (semiperimetro - ladoCA))
    }

    // Método para imprimir el triángulo
    fun imprimir() {
        println("Triángulo de ${puntoA.imprimir()} a ${puntoB.imprimir()} a ${puntoC.imprimir()}")
    }
}

// Crear una clase para representar un círculo
class Circulo(val centro: Punto, val radio: Double) {

    // Método para calcular el área del círculo
    fun area(): Double {
        return Math.PI * Math.pow(radio, 2.0)
    }

    // Método para imprimir el círculo
    fun imprimir() {
        println("Círculo de centro ${centro.imprimir()} y radio $radio")
    }
}

// Función principal
fun main(args: Array<String>) {

    // Crear un escáner para leer la entrada del usuario
    val scanner = Scanner(System.`in`)

    // Pedir al usuario que introduzca las coordenadas de los puntos
    println("Introduce las coordenadas del punto A:")
    val xA = scanner.nextDouble()
    val yA = scanner.nextDouble()
    val puntoA = Punto(xA, yA)

    println("Introduce las coordenadas del punto B:")
    val xB = scanner.nextDouble()
    val yB = scanner.nextDouble()
    val puntoB = Punto(xB, yB)

    println("Introduce las coordenadas del punto C:")
    val xC = scanner.nextDouble()
    val yC = scanner.nextDouble()
    val puntoC = Punto(xC, yC)

    // Crear un triángulo con los puntos introducidos por el usuario
    val triangulo = Triangulo(puntoA, puntoB, puntoC)

    // Imprimir el triángulo
    triangulo.imprimir()

    // Calcular el área del triángulo
    val areaTriangulo = triangulo.area()

    // Imprimir el área del triángulo
    println("El área del triángulo es: $areaTriangulo")
}
```

Este código es un ejemplo de un código complejo en Kotlin. Crea varias clases para representar diferentes objetos geométricos, como puntos, líneas, triángulos y círculos. También incluye una función principal que solicita al usuario que introduzca las coordenadas de los puntos y crea un triángulo con estos puntos. El código calcula el área del triángulo y lo imprime en la consola.

El código está bien estructurado y utiliza clases y objetos para representar los diferentes objetos geométricos. También utiliza funciones para realizar cálculos y operaciones en los objetos. El código está bien comentado, lo que facilita su comprensión.