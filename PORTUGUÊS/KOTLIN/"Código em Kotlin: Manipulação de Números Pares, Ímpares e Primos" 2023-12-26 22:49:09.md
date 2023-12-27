Claro! Aqui está um exemplo de um código complexo em Kotlin que utiliza conceitos avançados de programação:

```kotlin
import kotlin.math.sqrt

fun main() {
    val numeros = mutableListOf<Int>()

    for (i in 1..100) {
        numeros.add(i)
    }

    val numerosPares = numeros.filter { it % 2 == 0 }
    val somaDosPares = numerosPares.sum()
    val mediaDosPares = somaDosPares.toDouble() / numerosPares.size

    val numerosImpares = numeros.filter { it % 2 != 0 }
    val maiorNumeroImpar = numerosImpares.max()
    val menorNumeroImpar = numerosImpares.min()

    val numerosPrimos = numeros.filter { isPrimo(it) }
    val quantidadeNumerosPrimos = numerosPrimos.size

    println("Números pares: $numerosPares")
    println("Soma dos números pares: $somaDosPares")
    println("Média dos números pares: $mediaDosPares")
    println("Maior número ímpar: $maiorNumeroImpar")
    println("Menor número ímpar: $menorNumeroImpar")
    println("Quantidade de números primos: $quantidadeNumerosPrimos")
}

fun isPrimo(numero: Int): Boolean {
    if (numero < 2) {
        return false
    }

    for (i in 2..sqrt(numero.toDouble()).toInt()) {
        if (numero % i == 0) {
            return false
        }
    }

    return true
}
```

Neste código, começamos definindo uma lista de números de 1 a 100. Em seguida, filtramos essa lista para obter os números pares e calculamos a soma e a média desses números. Também filtramos a lista para obter os números ímpares e encontramos o maior e o menor número ímpar.

Além disso, implementamos uma função `isPrimo` que verifica se um número é primo. Utilizamos o algoritmo básico de divisão para verificar se o número é divisível por algum número entre 2 e a raiz quadrada desse número.

Por fim, exibimos os resultados na saída padrão.

Esse código demonstra o uso de conceitos como listas, filtros, soma, média, máximo, mínimo e verificação de números primos. Espero que seja útil para você!