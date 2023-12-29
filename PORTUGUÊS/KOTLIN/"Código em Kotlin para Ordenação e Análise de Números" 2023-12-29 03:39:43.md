Claro! Aqui está um código complexo em Kotlin que envolve o uso de várias bibliotecas e recursos avançados:

```kotlin
import java.util.*

fun main() {
    val scanner = Scanner(System.`in`)
    val listaNumeros = mutableListOf<Int>()

    println("Digite a quantidade de números que deseja inserir:")
    val quantidadeNumeros = scanner.nextInt()

    println("Digite os números:")
    for (i in 0 until quantidadeNumeros) {
        val numero = scanner.nextInt()
        listaNumeros.add(numero)
    }

    println("Agora, vamos exibir os números em ordem crescente:")
    listaNumeros.sort()
    listaNumeros.forEach { numero ->
        println(numero)
    }

    val soma = listaNumeros.sum()
    println("A soma de todos os números é: $soma")

    val media = soma.toDouble() / listaNumeros.size
    println("A média dos números é: $media")

    val numerosPares = listaNumeros.filter { it % 2 == 0 }
    println("Os números pares da lista são:")
    numerosPares.forEach { numero ->
        println(numero)
    }

    val numerosImpares = listaNumeros.filter { it % 2 != 0 }
    println("Os números ímpares da lista são:")
    numerosImpares.forEach { numero ->
        println(numero)
    }

    if (listaNumeros.isNotEmpty()) {
        val maiorNumero = listaNumeros.maxOrNull()
        println("O maior número da lista é: $maiorNumero")

        val menorNumero = listaNumeros.minOrNull()
        println("O menor número da lista é: $menorNumero")
    }
}
```

Este código em Kotlin permite ao usuário digitar uma quantidade de números desejada, armazená-los em uma lista e realizar várias operações com eles. Aqui está o que o código faz:

1. Solicita ao usuário a quantidade de números que deseja inserir.
2. Solicita ao usuário para digitar os números.
3. Armazena os números em uma lista.
4. Classifica a lista em ordem crescente.
5. Exibe os números em ordem crescente.
6. Calcula a soma de todos os números.
7. Calcula a média dos números.
8. Filtra os números pares e os exibe.
9. Filtra os números ímpares e os exibe.
10. Identifica e exibe o maior e o menor número da lista.

Espero que este código atenda às suas expectativas! Lembre-se de que você pode modificá-lo e adaptá-lo conforme necessário.