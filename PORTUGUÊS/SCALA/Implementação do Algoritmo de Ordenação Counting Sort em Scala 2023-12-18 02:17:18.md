Claro! Vou criar um código complexo em Scala para você. Este código será uma implementação do algoritmo de ordenação chamado Counting Sort, que é um algoritmo de ordenação estável e eficiente para classificar elementos em um intervalo específico de valores.

```scala
object CountingSort {
  def countingSort(arr: Array[Int], maxValue: Int): Array[Int] = {
    val countArray = Array.ofDim[Int](maxValue + 1)
    val sortedArray = Array.ofDim[Int](arr.length)

    for (i <- arr.indices) {
      countArray(arr(i)) += 1
    }

    for (i <- 1 to maxValue) {
      countArray(i) += countArray(i - 1)
    }

    for (i <- arr.indices.reverse) {
      sortedArray(countArray(arr(i)) - 1) = arr(i)
      countArray(arr(i)) -= 1
    }

    sortedArray
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(4, 2, 2, 8, 3, 3, 1)
    val maxValue = 8

    val sortedArr = countingSort(arr, maxValue)

    println("Array original: " + arr.mkString(", "))
    println("Array ordenado: " + sortedArr.mkString(", "))
  }
}
```

Neste código, temos uma função `countingSort` que recebe um array de inteiros `arr` e o valor máximo do intervalo de valores presentes no array `maxValue`. A função retorna um novo array ordenado.

Dentro da função `countingSort`, criamos dois arrays: `countArray`, que será usado para contar a ocorrência de cada valor no array original, e `sortedArray`, que será usado para armazenar o array ordenado.

Em seguida, percorremos o array original e incrementamos o valor correspondente no `countArray`. Depois, percorremos o `countArray` e atualizamos cada posição somando o valor da posição anterior.

Por fim, percorremos o array original de trás para frente e colocamos cada elemento em sua posição correta no `sortedArray`, atualizando o valor correspondente no `countArray`.

Na função `main`, criamos um array de exemplo, chamamos a função `countingSort` passando o array e o valor máximo, e imprimimos o array original e o array ordenado. Neste caso, o resultado será:

```
Array original: 4, 2, 2, 8, 3, 3, 1
Array ordenado: 1, 2, 2, 3, 3, 4, 8
```

Espero que este código atenda às suas expectativas!