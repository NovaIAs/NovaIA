Claro! Aqui está um exemplo de código em Scala que implementa um algoritmo de ordenação chamado Merge Sort:

```scala
object MergeSort {
  def mergeSort(arr: Array[Int]): Array[Int] = {
    if (arr.length <= 1) arr // retorna o array se já estiver ordenado ou vazio

    val meio = arr.length / 2
    val (esquerda, direita) = arr.splitAt(meio) // divide o array em duas partes

    merge(mergeSort(esquerda), mergeSort(direita)) // combina as duas partes ordenadas
  }

  def merge(esquerda: Array[Int], direita: Array[Int]): Array[Int] = {
    var resultado = Array.empty[Int] // array vazio para armazenar o resultado da ordenação
    var i = 0
    var j = 0

    while (i < esquerda.length && j < direita.length) {
      if (esquerda(i) <= direita(j)) {
        resultado = resultado :+ esquerda(i)
        i += 1
      } else {
        resultado = resultado :+ direita(j)
        j += 1
      }
    }

    // Adiciona o restante dos elementos da parte esquerda, se houver
    while (i < esquerda.length) {
      resultado = resultado :+ esquerda(i)
      i += 1
    }

    // Adiciona o restante dos elementos da parte direita, se houver
    while (j < direita.length) {
      resultado = resultado :+ direita(j)
      j += 1
    }

    resultado
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(64, 34, 25, 12, 22, 11, 90)
    val arrOrdenado = mergeSort(arr)

    println("Array desordenado: " + arr.mkString(", "))
    println("Array ordenado pelo Merge Sort: " + arrOrdenado.mkString(", "))
  }
}
```

Neste código, a função `mergeSort` recebe um array de inteiros e realiza a ordenação usando o algoritmo Merge Sort. Se o array tiver tamanho igual ou menor que 1, o array é retornado imediatamente, pois já está ordenado ou vazio.

A função `mergeSort` divide o array em duas partes, chamando recursivamente o `mergeSort` para cada parte. Em seguida, chama a função `merge` para combinar as duas partes ordenadas em um único array ordenado.

A função `merge` recebe duas partes (esquerda e direita) do array e cria um novo array vazio chamado `resultado` para armazenar o resultado da ordenação. Em seguida, usando um loop while, compara os elementos das partes esquerda e direita e adiciona o menor elemento ao array `resultado`. Depois, adiciona o restante dos elementos da parte esquerda ou direita, caso ainda existam.

Por fim, no método `main`, é criado um exemplo de array desordenado e é chamada a função `mergeSort` para ordená-lo. Os resultados são impressos no console.

Espero que esse exemplo possa te ajudar a entender melhor o algoritmo Merge Sort em Scala!