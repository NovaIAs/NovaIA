Claro! Aqui está um exemplo de código em Kotlin que implementa um algoritmo de busca binária recursiva em um array ordenado:

```kotlin
fun buscaBinariaRecursiva(array: IntArray, elemento: Int): Int {
    return buscaBinariaRecursivaAuxiliar(array, elemento, 0, array.size - 1)
}

fun buscaBinariaRecursivaAuxiliar(array: IntArray, elemento: Int, inicio: Int, fim: Int): Int {
    if (inicio > fim) {
        return -1 // Elemento não encontrado
    }

    val meio = (inicio + fim) / 2

    if (array[meio] == elemento) {
        return meio // Elemento encontrado
    } else if (array[meio] < elemento) {
        return buscaBinariaRecursivaAuxiliar(array, elemento, meio + 1, fim)
    } else {
        return buscaBinariaRecursivaAuxiliar(array, elemento, inicio, meio - 1)
    }
}

fun main() {
    val array = intArrayOf(1, 3, 5, 7, 9, 11, 13, 15)
    val elemento = 7
    val indice = buscaBinariaRecursiva(array, elemento)

    if (indice != -1) {
        println("Elemento encontrado no índice $indice.")
    } else {
        println("Elemento não encontrado.")
    }
}
```

Neste código, temos a função `buscaBinariaRecursiva` que faz a chamada inicial para a função auxiliar `buscaBinariaRecursivaAuxiliar`. A função auxiliar implementa a lógica da busca binária recursiva, dividindo o array ao meio a cada iteração e comparando o elemento do meio com o elemento desejado.

A função retorna o índice do elemento caso seja encontrado, ou -1 caso contrário. No `main`, temos um exemplo de uso, onde um array ordenado é definido e a busca binária é realizada para encontrar o elemento 7.

Esse código é eficiente para buscar elementos em arrays ordenados, pois reduz o espaço de busca pela metade a cada iteração, resultando em uma complexidade de tempo de O(log n).