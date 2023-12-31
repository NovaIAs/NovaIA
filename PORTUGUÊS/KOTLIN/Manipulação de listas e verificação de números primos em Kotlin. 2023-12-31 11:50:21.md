Claro! Aqui está um exemplo de um código Kotlin complexo:

```kotlin
import kotlin.random.Random

fun main() {
    val listaNumeros = mutableListOf<Int>()

    for (i in 1..10) {
        val numeroAleatorio = Random.nextInt(1, 101)
        listaNumeros.add(numeroAleatorio)
    }

    println("Lista original: $listaNumeros")

    val listaOrdenada = listaNumeros.sorted()
    println("Lista ordenada: $listaOrdenada")

    val soma = listaNumeros.sum()
    println("Soma dos números: $soma")

    val media = soma.toDouble() / listaNumeros.size
    println("Média dos números: $media")

    val numerosPares = listaNumeros.filter { it % 2 == 0 }
    println("Números pares: $numerosPares")

    val numerosImpares = listaNumeros.filter { it % 2 != 0 }
    println("Números ímpares: $numerosImpares")

    val numerosPrimos = listaNumeros.filter { numero -> isPrimo(numero) }
    println("Números primos: $numerosPrimos")
}

fun isPrimo(numero: Int): Boolean {
    if (numero <= 1) {
        return false
    }

    for (i in 2 until numero) {
        if (numero % i == 0) {
            return false
        }
    }

    return true
}
```

Neste exemplo, criamos um programa que gera uma lista de 10 números aleatórios e realiza diversas operações com esses números.

Primeiro, criamos uma lista vazia chamada `listaNumeros` do tipo `MutableList<Int>`. Em seguida, utilizamos um loop `for` para gerar 10 números aleatórios (entre 1 e 100) e adicioná-los à lista.

Depois, imprimimos a lista original usando `println("Lista original: $listaNumeros")`.

Em seguida, utilizamos a função `sorted()` para ordenar a lista em ordem crescente e a armazenamos na variável `listaOrdenada`. Imprimimos a lista ordenada usando `println("Lista ordenada: $listaOrdenada")`.

Calculamos a soma dos números da lista utilizando a função `sum()` e a armazenamos na variável `soma`. Imprimimos a soma usando `println("Soma dos números: $soma")`.

Para calcular a média dos números, dividimos a soma pelo tamanho da lista (convertido para `Double`) e armazenamos o resultado na variável `media`. Imprimimos a média usando `println("Média dos números: $media")`.

Filtramos os números pares da lista utilizando a função `filter` e uma lambda que verifica se o número é divisível por 2. Armazenamos os números pares na variável `numerosPares` e os imprimimos usando `println("Números pares: $numerosPares")`.

Da mesma forma, filtramos os números ímpares da lista utilizando uma lambda que verifica se o número não é divisível por 2. Armazenamos os números ímpares na variável `numerosImpares` e os imprimimos usando `println("Números ímpares: $numerosImpares")`.

Por fim, utilizamos a função `isPrimo()` para verificar quais números da lista são primos. A função `isPrimo()` verifica se o número é menor ou igual a 1 e se é divisível por algum número entre 2 e o próprio número (excluindo-o). Os números primos são filtrados e armazenados na variável `numerosPrimos`, que é impressa usando `println("Números primos: $numerosPrimos")`.

Esse código mostra alguns exemplos de uso de funções e operações com listas em Kotlin, além de implementar a verificação de números primos.