Claro! Aqui está um exemplo de um código complexo em Kotlin:

```kotlin
import java.util.*

fun main() {
    // Criando uma lista de números aleatórios
    val listaNumeros = gerarListaNumerosAleatorios(10, 1, 100)

    // Imprimindo a lista original
    println("Lista original:")
    imprimirLista(listaNumeros)

    // Filtrando os números pares
    val listaPares = filtrarNumerosPares(listaNumeros)

    // Imprimindo a lista de números pares
    println("Lista de números pares:")
    imprimirLista(listaPares)

    // Calculando a média dos números ímpares
    val mediaImpares = calcularMediaNumerosImpares(listaNumeros)

    // Imprimindo a média dos números ímpares
    println("Média dos números ímpares: $mediaImpares")

    // Ordenando a lista de números de forma decrescente
    val listaOrdenada = ordenarListaDecrescente(listaNumeros)

    // Imprimindo a lista ordenada
    println("Lista ordenada de forma decrescente:")
    imprimirLista(listaOrdenada)
}

fun gerarListaNumerosAleatorios(tamanho: Int, min: Int, max: Int): List<Int> {
    val lista = mutableListOf<Int>()
    val random = Random()

    for (i in 0 until tamanho) {
        val numeroAleatorio = random.nextInt(max - min + 1) + min
        lista.add(numeroAleatorio)
    }

    return lista
}

fun imprimirLista(lista: List<Int>) {
    for (numero in lista) {
        print("$numero ")
    }
    println()
}

fun filtrarNumerosPares(lista: List<Int>): List<Int> {
    return lista.filter { it % 2 == 0 }
}

fun calcularMediaNumerosImpares(lista: List<Int>): Double {
    val numerosImpares = lista.filter { it % 2 != 0 }
    val somaImpares = numerosImpares.sum()
    return somaImpares.toDouble() / numerosImpares.size
}

fun ordenarListaDecrescente(lista: List<Int>): List<Int> {
    return lista.sortedDescending()
}
```

Neste código, temos várias funções que realizam diferentes operações em uma lista de números aleatórios:

1. A função `gerarListaNumerosAleatorios` cria uma lista com um determinado tamanho e preenche com números aleatórios dentro de um intervalo especificado.
2. A função `imprimirLista` imprime os elementos da lista no console.
3. A função `filtrarNumerosPares` filtra os números pares da lista original.
4. A função `calcularMediaNumerosImpares` calcula a média dos números ímpares da lista original.
5. A função `ordenarListaDecrescente` ordena a lista de números de forma decrescente.

Na função `main`, utilizamos essas funções para realizar as operações desejadas na lista de números aleatórios gerada. No final, cada etapa é impressa no console para visualização dos resultados.

Espero que isso lhe dê uma ideia de como um código complexo em Kotlin pode ser estruturado e implementado.