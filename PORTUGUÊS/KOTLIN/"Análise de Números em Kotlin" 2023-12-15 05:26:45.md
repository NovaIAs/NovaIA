Claro! Aqui está um código complexo em Kotlin para você:

```kotlin
import java.util.*

fun main() {
    val scanner = Scanner(System.`in`)
    val listaNumeros = mutableListOf<Int>()

    println("Digite a quantidade de números que deseja inserir na lista:")
    val quantidadeNumeros = scanner.nextInt()

    println("Digite os números:")
    for (i in 0 until quantidadeNumeros) {
        val numero = scanner.nextInt()
        listaNumeros.add(numero)
    }

    val maiorNumero = encontrarMaiorNumero(listaNumeros)
    val menorNumero = encontrarMenorNumero(listaNumeros)
    val mediaNumeros = calcularMedia(listaNumeros)
    val numerosPares = encontrarNumerosPares(listaNumeros)
    val numerosImpares = encontrarNumerosImpares(listaNumeros)

    println("O maior número da lista é: $maiorNumero")
    println("O menor número da lista é: $menorNumero")
    println("A média dos números da lista é: $mediaNumeros")
    println("Os números pares da lista são: $numerosPares")
    println("Os números ímpares da lista são: $numerosImpares")
}

fun encontrarMaiorNumero(lista: List<Int>): Int {
    var maior = lista[0]
    for (numero in lista) {
        if (numero > maior) {
            maior = numero
        }
    }
    return maior
}

fun encontrarMenorNumero(lista: List<Int>): Int {
    var menor = lista[0]
    for (numero in lista) {
        if (numero < menor) {
            menor = numero
        }
    }
    return menor
}

fun calcularMedia(lista: List<Int>): Double {
    var soma = 0
    for (numero in lista) {
        soma += numero
    }
    return soma.toDouble() / lista.size
}

fun encontrarNumerosPares(lista: List<Int>): List<Int> {
    val numerosPares = mutableListOf<Int>()
    for (numero in lista) {
        if (numero % 2 == 0) {
            numerosPares.add(numero)
        }
    }
    return numerosPares
}

fun encontrarNumerosImpares(lista: List<Int>): List<Int> {
    val numerosImpares = mutableListOf<Int>()
    for (numero in lista) {
        if (numero % 2 != 0) {
            numerosImpares.add(numero)
        }
    }
    return numerosImpares
}
```

Este código solicita ao usuário que insira a quantidade de números que deseja adicionar a uma lista. Em seguida, o usuário pode fornecer os números. O código então calcula o maior e o menor número na lista, a média dos números, e separa os números pares e ímpares em listas separadas. Por fim, exibe os resultados na tela.

Essa é apenas uma das várias abordagens possíveis para um código complexo em Kotlin. Você pode modificá-lo e expandi-lo de acordo com suas necessidades.