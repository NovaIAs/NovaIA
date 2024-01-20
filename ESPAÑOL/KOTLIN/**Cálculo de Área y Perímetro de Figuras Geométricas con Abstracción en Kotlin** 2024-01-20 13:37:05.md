```kotlin
// Crear una clase abstracta para definir la interfaz común de todas las figuras geométricas.
abstract class Figura {
    // Definir un método abstracto para calcular el área de la figura.
    abstract fun calcularArea(): Double

    // Definir un método abstracto para calcular el perímetro de la figura.
    abstract fun calcularPerimetro(): Double
}

// Crear una clase derivada de Figura para representar un círculo.
class Circulo(val radio: Double) : Figura() {
    // Implementar el método calcularArea() para el círculo.
    override fun calcularArea(): Double {
        return Math.PI * radio * radio
    }

    // Implementar el método calcularPerimetro() para el círculo.
    override fun calcularPerimetro(): Double {
        return 2 * Math.PI * radio
    }
}

// Crear una clase derivada de Figura para representar un rectángulo.
class Rectangulo(val ancho: Double, val largo: Double) : Figura() {
    // Implementar el método calcularArea() para el rectángulo.
    override fun calcularArea(): Double {
        return ancho * largo
    }

    // Implementar el método calcularPerimetro() para el rectángulo.
    override fun calcularPerimetro(): Double {
        return 2 * (ancho + largo)
    }
}

// Crear una clase derivada de Figura para representar un triángulo.
class Triangulo(val lado1: Double, val lado2: Double, val lado3: Double) : Figura() {
    // Implementar el método calcularArea() para el triángulo.
    override fun calcularArea(): Double {
        val semiperimetro = (lado1 + lado2 + lado3) / 2
        return Math.sqrt(semiperimetro * (semiperimetro - lado1) * (semiperimetro - lado2) * (semiperimetro - lado3))
    }

    // Implementar el método calcularPerimetro() para el triángulo.
    override fun calcularPerimetro(): Double {
        return lado1 + lado2 + lado3
    }
}

// Crear una función para imprimir las propiedades de una figura geométrica.
fun imprimirFigura(figura: Figura) {
    println("Figura: ${figura.javaClass.simpleName}")
    println("Área: ${figura.calcularArea()}")
    println("Perímetro: ${figura.calcularPerimetro()}")
    println()
}

// Crear una lista de figuras geométricas.
val figuras = listOf(
    Circulo(5.0),
    Rectangulo(3.0, 4.0),
    Triangulo(6.0, 8.0, 10.0)
)

// Imprimir las propiedades de cada figura geométrica en la lista.
for (figura in figuras) {
    imprimirFigura(figura)
}
```

Explicación del código:

* La clase `Figura` abstracta define la interfaz común de todas las figuras geométricas. Proporciona dos métodos abstractos, `calcularArea()` y `calcularPerimetro()`, que las clases derivadas deben implementar.
* Las clases `Circulo`, `Rectangulo` y `Triangulo` son clases derivadas de `Figura` que representan figuras geométricas específicas. Cada clase implementa los métodos `calcularArea()` y `calcularPerimetro()` para su propia figura geométrica.
* La función `imprimirFigura()` imprime las propiedades de una figura geométrica, incluyendo su nombre de clase, área y perímetro.
* La lista `figuras` contiene una lista de figuras geométricas.
* El bucle `for` recorre la lista `figuras` e imprime las propiedades de cada figura geométrica utilizando la función `imprimirFigura()`.