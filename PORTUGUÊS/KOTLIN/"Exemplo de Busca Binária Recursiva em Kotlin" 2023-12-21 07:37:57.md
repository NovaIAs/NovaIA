Claro! Aqui está um exemplo de um código complexo em Kotlin que implementa um algoritmo de busca binária recursiva:

```kotlin
fun buscaBinariaRecursiva(array: IntArray, alvo: Int): Int {
    return buscaBinariaRecursivaAuxiliar(array, alvo, 0, array.size - 1)
}

fun buscaBinariaRecursivaAuxiliar(array: IntArray, alvo: Int, inicio: Int, fim: Int): Int {
    if (inicio > fim) {
        return -1 // Elemento não encontrado
    }

    val meio = (inicio + fim) / 2

    if (array[meio] == alvo) {
        return meio // Elemento encontrado
    } else if (array[meio] > alvo) {
        return buscaBinariaRecursivaAuxiliar(array, alvo, inicio, meio - 1)
    } else {
        return buscaBinariaRecursivaAuxiliar(array, alvo, meio + 1, fim)
    }
}

fun main() {
    val array = intArrayOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val alvo = 6

    val resultado = buscaBinariaRecursiva(array, alvo)

    if (resultado == -1) {
        println("Elemento não encontrado no array.")
    } else {
        println("Elemento encontrado no índice $resultado do array.")
    }
}
```

Neste código, temos a função `buscaBinariaRecursiva` que é responsável por iniciar a busca binária recursiva. Essa função chama a função auxiliar `buscaBinariaRecursivaAuxiliar`, que realiza a busca binária recursiva de fato.

A função `buscaBinariaRecursivaAuxiliar` recebe o array, o elemento alvo, o índice de início e o índice de fim do subarray atual. Através de comparações, ela divide o array ao meio e decide em qual metade continuar a busca. O processo é repetido até que o elemento seja encontrado ou que não haja mais elementos para buscar.

No `main`, é criado um array de exemplo e escolhemos o elemento 6 como alvo da busca. O resultado da busca é então exibido no console.

Esse código implementa um algoritmo de busca eficiente e é escrito em Kotlin, uma linguagem de programação moderna e expressiva. Espero que seja útil para o seu projeto!