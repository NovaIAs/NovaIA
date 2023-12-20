Claro! Aqui está um código complexo em Kotlin que utiliza conceitos avançados da linguagem:

```kotlin
// Classe base abstrata para representar uma forma geométrica
abstract class FormaGeometrica {

    // Método abstrato para calcular a área da forma
    abstract fun calcularArea(): Double

    // Método abstrato para calcular o perímetro da forma
    abstract fun calcularPerimetro(): Double

    // Método para exibir informações sobre a forma
    open fun exibirInformacoes() {
        println("Área: ${calcularArea()}")
        println("Perímetro: ${calcularPerimetro()}")
    }
}

// Classe retângulo que herda da classe FormaGeometrica
class Retangulo(private val base: Double, private val altura: Double) : FormaGeometrica() {

    // Implementação do método para calcular a área do retângulo
    override fun calcularArea(): Double {
        return base * altura
    }

    // Implementação do método para calcular o perímetro do retângulo
    override fun calcularPerimetro(): Double {
        return 2 * (base + altura)
    }

    // Sobrescrita do método exibirInformacoes para exibir informações específicas do retângulo
    override fun exibirInformacoes() {
        println("Retângulo:")
        println("Base: $base")
        println("Altura: $altura")
        super.exibirInformacoes()
    }
}

// Classe círculo que herda da classe FormaGeometrica
class Circulo(private val raio: Double) : FormaGeometrica() {

    // Implementação do método para calcular a área do círculo
    override fun calcularArea(): Double {
        return Math.PI * raio * raio
    }

    // Implementação do método para calcular o perímetro do círculo
    override fun calcularPerimetro(): Double {
        return 2 * Math.PI * raio
    }

    // Sobrescrita do método exibirInformacoes para exibir informações específicas do círculo
    override fun exibirInformacoes() {
        println("Círculo:")
        println("Raio: $raio")
        super.exibirInformacoes()
    }
}

fun main() {
    // Criando uma lista de formas geométricas
    val formas = mutableListOf<FormaGeometrica>()

    // Adicionando algumas formas à lista
    formas.add(Retangulo(5.0, 3.0))
    formas.add(Circulo(7.0))

    // Iterando sobre a lista de formas e exibindo as informações de cada uma
    for (forma in formas) {
        forma.exibirInformacoes()
        println("--------------------")
    }
}
```

Nesse código, temos as classes `FormaGeometrica`, `Retangulo` e `Circulo`. A classe `FormaGeometrica` é uma classe abstrata que define métodos abstratos para calcular a área e o perímetro de uma forma geométrica, além de um método para exibir informações gerais sobre a forma. As classes `Retangulo` e `Circulo` herdam da classe `FormaGeometrica` e implementam os métodos abstratos de acordo com suas características específicas.

No método `main`, criamos uma lista de formas geométricas e adicionamos um retângulo e um círculo a essa lista. Em seguida, iteramos sobre a lista e chamamos o método `exibirInformacoes()` de cada forma, que imprime na tela as informações específicas de cada forma, como a base e altura do retângulo e o raio do círculo, além da área e perímetro calculados.

Esse código é um exemplo de uso de conceitos avançados da linguagem Kotlin, como classes abstratas, polimorfismo e herança. Ele mostra como é possível criar uma estrutura de classes que permite calcular e exibir informações sobre formas geométricas de maneira flexível e eficiente.