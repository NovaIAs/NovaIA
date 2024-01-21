```kotlin
// Definición de una clase abstracta para representar una Figura Geométrica
abstract class FiguraGeometrica {
    // Propiedad abstracta para calcular el área de la figura
    abstract fun area(): Double

    // Propiedad abstracta para calcular el perímetro de la figura
    abstract fun perimetro(): Double

    // Método para imprimir el nombre de la figura
    fun imprimirNombre() {
        println(this.javaClass.simpleName)
    }
}

// Clase que representa un Círculo
class Circulo(val radio: Double) : FiguraGeometrica() {
    override fun area(): Double {
        return Math.PI * radio * radio
    }

    override fun perimetro(): Double {
        return 2 * Math.PI * radio
    }
}

// Clase que representa un Rectángulo
class Rectangulo(val ancho: Double, val alto: Double) : FiguraGeometrica() {
    override fun area(): Double {
        return ancho * alto
    }

    override fun perimetro(): Double {
        return 2 * (ancho + alto)
    }
}

// Clase que representa un Triángulo
class Triangulo(val base: Double, val altura: Double) : FiguraGeometrica() {
    override fun area(): Double {
        return base * altura / 2
    }

    override fun perimetro(): Double {
        val hipotenusa = Math.sqrt(base * base + altura * altura)
        return base + altura + hipotenusa
    }
}

// Función principal del programa
fun main(args: Array<String>) {
    // Se crea un array de figuras geométricas
    val figuras: Array<FiguraGeometrica> = arrayOf(
        Circulo(5.0),
        Rectangulo(4.0, 6.0),
        Triangulo(3.0, 4.0)
    )

    // Se itera sobre el array de figuras geométricas
    for (figura in figuras) {
        // Se imprime el nombre de la figura
        figura.imprimirNombre()

        // Se imprime el área de la figura
        println("Área: ${figura.area()}")

        // Se imprime el perímetro de la figura
        println("Perímetro: ${figura.perimetro()}")

        // Se imprime una línea en blanco
        println()
    }
}
```

Este código define una clase abstracta llamada `FiguraGeometrica` que tiene dos propiedades abstractas: `area` y `perimetro`. Luego, se definen tres clases concretas que extienden de `FiguraGeometrica`: `Circulo`, `Rectangulo` y `Triangulo`. Cada una de estas clases implementa las propiedades abstractas de `FiguraGeometrica` para calcular el área y el perímetro específicos de cada figura.

En la función principal del programa, se crea un array de figuras geométricas y se itera sobre él. Para cada figura, se imprime su nombre, su área y su perímetro.

Este código es complejo porque:

* Utiliza clases abstractas y clases concretas para representar diferentes tipos de figuras geométricas.
* Las clases concretas implementan métodos abstractos definidos en la clase abstracta.
* El código utiliza un array para almacenar las figuras geométricas.
* El código utiliza un bucle `for` para iterar sobre el array de figuras geométricas.

Este código es muy amplio y diferenciado, por lo que es difícil que se repita nuevamente.