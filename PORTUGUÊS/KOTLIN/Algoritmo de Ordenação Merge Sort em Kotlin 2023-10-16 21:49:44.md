Claro! Aqui está um código complexo em Kotlin que implementa um algoritmo de ordenação chamado Merge Sort. O Merge Sort é um algoritmo de ordenação eficiente que divide a lista em sub-listas menores, ordena cada sub-lista e, em seguida, combina as sub-listas ordenadas para obter uma lista completamente ordenada.

```kotlin
fun mergeSort(arr: IntArray) {
    if (arr.size <= 1) {
        return
    }
    
    val middle = arr.size / 2
    val left = arr.copyOfRange(0, middle)
    val right = arr.copyOfRange(middle, arr.size)
    
    mergeSort(left)
    mergeSort(right)
    
    merge(arr, left, right)
}

fun merge(arr: IntArray, left: IntArray, right: IntArray) {
    var i = 0
    var j = 0
    var k = 0
    
    while (i < left.size && j < right.size) {
        if (left[i] <= right[j]) {
            arr[k++] = left[i++]
        } else {
            arr[k++] = right[j++]
        }
    }
    
    while (i < left.size) {
        arr[k++] = left[i++]
    }
    
    while (j < right.size) {
        arr[k++] = right[j++]
    }
}

fun main() {
    val arr = intArrayOf(64, 34, 25, 12, 22, 11, 90)
    mergeSort(arr)
    
    println("Array Ordenado:")
    for (element in arr) {
        print("$element ")
    }
}
```

Nesse código, a função `mergeSort()` recebe um array de números inteiros como parâmetro e implementa o algoritmo de Merge Sort para ordenar o array. Primeiro, a função verifica se o tamanho do array é menor ou igual a 1; se for, não é necessário ordenar, então a função retorna. Caso contrário, a função divide o array em duas partes, esquerda e direita, e chama recursivamente a função `mergeSort()` para ordenar cada uma dessas partes. Depois disso, a função `merge()` é chamada para combinar as partes ordenadas e produzir o array finalmente ordenado.

A função `merge()` recebe o array original e as partes esquerda e direita como parâmetros. Ela possui três variáveis de controle: `i`, `j` e `k`. Essas variáveis são usadas para iterar pelos elementos das partes esquerda e direita, e também para atualizar o array original. O loop while é usado para comparar os elementos das partes esquerda e direita e mesclá-los em ordem crescente. Depois disso, dois outros loops while são usados para adicionar os elementos restantes, se houver, das partes esquerda e direita ao array original.

No `main()`, um array de exemplo é criado e passado para a função `mergeSort()` para ordenação. Em seguida, os elementos do array ordenado são impressos na tela.

Espero que esse código complexo em Kotlin seja útil e atenda às suas expectativas!