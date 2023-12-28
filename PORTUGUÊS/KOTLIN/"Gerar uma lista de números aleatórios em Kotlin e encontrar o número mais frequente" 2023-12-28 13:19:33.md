Claro! Aqui está um código complexo em Kotlin:

```kotlin
import java.util.Random

fun main() {
    val listaNumeros = gerarListaNumeros(1000) // Gera uma lista de 1000 números aleatórios
    val listaOrdenada = ordenarLista(listaNumeros) // Ordena a lista de números
    val numeroMaisFrequente = encontrarNumeroMaisFrequente(listaOrdenada) // Encontra o número mais frequente na lista

    println("Lista de números gerados:")
    println(listaNumeros)

    println("\nLista de números ordenada:")
    println(listaOrdenada)

    println("\nNúmero mais frequente:")
    println(numeroMaisFrequente)
}

fun gerarListaNumeros(tamanho: Int): List<Int> {
    val random = Random()
    val lista = mutableListOf<Int>()

    for (i in 0 until tamanho) {
        val numero = random.nextInt(100) // Gera um número aleatório entre 0 e 99
        lista.add(numero)
    }

    return lista
}

fun ordenarLista(lista: List<Int>): List<Int> {
    return lista.sorted()
}

fun encontrarNumeroMaisFrequente(lista: List<Int>): Int {
    val contagemNumeros = mutableMapOf<Int, Int>()
    var numeroMaisFrequente = 0
    var frequenciaMaxima = 0

    for (numero in lista) {
        val frequencia = contagemNumeros.getOrDefault(numero, 0) + 1
        contagemNumeros[numero] = frequencia

        if (frequencia > frequenciaMaxima) {
            frequenciaMaxima = frequencia
            numeroMaisFrequente = numero
        }
    }

    return numeroMaisFrequente
}
```

Neste código, criei uma função `gerarListaNumeros` que gera uma lista de números aleatórios entre 0 e 99. Os números são gerados usando a classe `Random` do Java.

Em seguida, temos a função `ordenarLista`, que recebe a lista gerada e retorna uma nova lista ordenada em ordem crescente.

Por fim, a função `encontrarNumeroMaisFrequente` recebe a lista ordenada e utiliza um mapa para contar a frequência de cada número na lista. Em seguida, percorre o mapa para encontrar o número com a maior frequência.

No `main`, chamamos essas funções e imprimimos os resultados na tela. Primeiro, mostramos a lista de números gerados, depois a lista ordenada e por fim o número mais frequente.

Este código é um exemplo de como você pode gerar uma lista de números aleatórios, ordená-la e encontrar o número mais frequente usando Kotlin. Claro, você pode personalizar e expandir esse código conforme necessário.