Claro! Aqui está um código complexo em Kotlin que utiliza conceitos avançados de programação para criar uma calculadora interativa. Este código permite ao usuário realizar operações matemáticas básicas, como adição, subtração, multiplicação e divisão. O programa também possui um recurso para calcular o fatorial de um número digitado pelo usuário.

```kotlin
import java.util.*

fun main() {
    val scanner = Scanner(System.`in`)
    
    println("Bem-vindo(a) à calculadora interativa!")
    println("Digite o número correspondente à operação desejada:")
    println("1 - Adição")
    println("2 - Subtração")
    println("3 - Multiplicação")
    println("4 - Divisão")
    println("5 - Calcular fatorial")
    
    val operacao = scanner.nextInt()
    
    when (operacao) {
        1 -> {
            println("Digite o primeiro número:")
            val numero1 = scanner.nextDouble()
            println("Digite o segundo número:")
            val numero2 = scanner.nextDouble()
            val resultado = numero1 + numero2
            println("O resultado da adição é: $resultado")
        }
        2 -> {
            println("Digite o primeiro número:")
            val numero1 = scanner.nextDouble()
            println("Digite o segundo número:")
            val numero2 = scanner.nextDouble()
            val resultado = numero1 - numero2
            println("O resultado da subtração é: $resultado")
        }
        3 -> {
            println("Digite o primeiro número:")
            val numero1 = scanner.nextDouble()
            println("Digite o segundo número:")
            val numero2 = scanner.nextDouble()
            val resultado = numero1 * numero2
            println("O resultado da multiplicação é: $resultado")
        }
        4 -> {
            println("Digite o dividendo:")
            val dividendo = scanner.nextDouble()
            println("Digite o divisor:")
            val divisor = scanner.nextDouble()
            if (divisor != 0.0) {
                val resultado = dividendo / divisor
                println("O resultado da divisão é: $resultado")
            } else {
                println("Erro: Divisão por zero não é permitida!")
            }
        }
        5 -> {
            println("Digite o número para calcular o fatorial:")
            val numero = scanner.nextInt()
            if (numero >= 0) {
                val resultado = calcularFatorial(numero)
                println("O fatorial de $numero é: $resultado")
            } else {
                println("Erro: Não é possível calcular o fatorial de um número negativo!")
            }
        }
        else -> {
            println("Erro: Operação inválida!")
        }
    }
}

fun calcularFatorial(numero: Int): Long {
    var fatorial: Long = 1
    for (i in 1..numero) {
        fatorial *= i
    }
    return fatorial
}
```

Neste código, utilizamos a função `main()` para iniciar o programa. Utilizamos a classe `Scanner` para capturar a entrada do usuário. O usuário pode escolher a operação desejada digitando um número correspondente. Em seguida, utilizamos uma estrutura de controle `when` para executar a operação selecionada.

Para as operações de adição, subtração e multiplicação, solicitamos ao usuário que digite os números envolvidos e realizamos o cálculo desejado, exibindo o resultado na tela.

Na operação de divisão, verificamos se o divisor é diferente de zero antes de realizar o cálculo. Caso seja zero, exibimos uma mensagem de erro.

Na operação de cálculo de fatorial, solicitamos ao usuário que digite o número desejado e verificamos se é um valor válido (não negativo). Utilizamos uma função auxiliar `calcularFatorial()` para realizar o cálculo do fatorial usando um loop `for`.

Espero que este código complexo em Kotlin seja útil para você!