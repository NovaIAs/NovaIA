Claro! Aqui está um exemplo de um código complexo em Kotlin que envolve manipulação de dados, estruturas de controle e funções:

```kotlin
import kotlin.math.sqrt

fun main() {
    val numeros = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val numerosPares = filtrarNumerosPares(numeros)
    val numerosPrimos = encontrarNumerosPrimos(numeros)

    val somaPares = calcularSoma(numerosPares)
    val mediaPrimos = calcularMedia(numerosPrimos)

    println("Números pares: $numerosPares")
    println("Soma dos números pares: $somaPares")
    println("Números primos: $numerosPrimos")
    println("Média dos números primos: $mediaPrimos")
}

fun filtrarNumerosPares(numeros: List<Int>): List<Int> {
    return numeros.filter { it % 2 == 0 }
}

fun encontrarNumerosPrimos(numeros: List<Int>): List<Int> {
    return numeros.filter { numero ->
        var ehPrimo = true
        if (numero < 2) {
            ehPrimo = false
        } else {
            for (i in 2..sqrt(numero.toDouble()).toInt()) {
                if (numero % i == 0) {
                    ehPrimo = false
                    break
                }
            }
        }
        ehPrimo
    }
}

fun calcularSoma(numeros: List<Int>): Int {
    var soma = 0
    for (numero in numeros) {
        soma += numero
    }
    return soma
}

fun calcularMedia(numeros: List<Int>): Double {
    val soma = calcularSoma(numeros)
    return soma.toDouble() / numeros.size
}
```

Neste código, começamos declarando uma lista de números de 1 a 10 (`val numeros = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)`). Em seguida, utilizamos duas funções para filtrar os números pares e os números primos dessa lista.

A função `filtrarNumerosPares` recebe a lista de números como parâmetro e utiliza a função `filter` para retornar apenas os números pares.

A função `encontrarNumerosPrimos` também recebe a lista de números como parâmetro e utiliza a função `filter` para retornar apenas os números primos. Para verificar se um número é primo, utilizamos um loop que verifica se o número é divisível por algum número entre 2 e sua raiz quadrada.

Depois de filtrar os números pares e primos, utilizamos as funções `calcularSoma` e `calcularMedia` para calcular a soma dos números pares e a média dos números primos, respectivamente. Essas funções usam loops simples para percorrer a lista de números e realizar as operações necessárias.

Finalmente, no `main`, chamamos todas as funções e imprimimos na tela os resultados obtidos.

Espero que este exemplo atenda às suas expectativas!