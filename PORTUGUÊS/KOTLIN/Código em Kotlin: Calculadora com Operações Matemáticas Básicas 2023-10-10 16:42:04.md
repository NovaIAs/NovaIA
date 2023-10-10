Claro! Aqui está um exemplo de código em Kotlin que cria uma classe chamada "Calculadora" que implementa operações matemáticas básicas:

```kotlin
class Calculadora {
    fun somar(a: Int, b: Int): Int {
        return a + b
    }

    fun subtrair(a: Int, b: Int): Int {
        return a - b
    }

    fun multiplicar(a: Int, b: Int): Int {
        return a * b
    }

    fun dividir(a: Int, b: Int): Double {
        if (b != 0) {
            return a.toDouble() / b.toDouble()
        } else {
            throw IllegalArgumentException("Não é possível dividir por zero.")
        }
    }
}

fun main() {
    val calculadora = Calculadora()

    val resultadoSoma = calculadora.somar(5, 3)
    println("Resultado da soma: $resultadoSoma")

    val resultadoSubtracao = calculadora.subtrair(10, 4)
    println("Resultado da subtração: $resultadoSubtracao")

    val resultadoMultiplicacao = calculadora.multiplicar(2, 6)
    println("Resultado da multiplicação: $resultadoMultiplicacao")

    try {
        val resultadoDivisao = calculadora.dividir(8, 2)
        println("Resultado da divisão: $resultadoDivisao")
    } catch (e: IllegalArgumentException) {
        println(e.message)
    }
}
```

Neste exemplo, a classe `Calculadora` possui quatro métodos: `somar`, `subtrair`, `multiplicar` e `dividir`. Cada um desses métodos recebe dois parâmetros inteiros e retorna o resultado da operação correspondente.

Na função `main`, um objeto `Calculadora` é criado e os métodos são chamados para realizar diferentes operações. Os resultados são então impressos na tela.

Note que o método `dividir` verifica se o segundo parâmetro é diferente de zero para evitar uma divisão por zero. Caso o denominador seja zero, uma exceção é lançada com uma mensagem de erro apropriada.

Espero que este código complexo e diferenciado em Kotlin possa atender às suas expectativas! Caso tenha alguma dúvida ou precise de mais ajuda, estou à disposição.