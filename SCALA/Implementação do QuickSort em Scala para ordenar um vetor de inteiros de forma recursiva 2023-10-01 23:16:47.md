Claro! Aqui está um código em Scala que implementa o algoritmo de ordenação QuickSort de forma recursiva:

```scala
object QuickSort {
  def main(args: Array[String]): Unit = {
    val vetor = Array(9, 4, 7, 2, 1, 5, 8, 3, 6)
    println("Vetor original:")
    vetor.foreach(print)
    println("\nVetor ordenado:")
    quickSort(vetor, 0, vetor.length - 1)
    vetor.foreach(print)
  }

  def quickSort(vetor: Array[Int], inicio: Int, fim: Int): Unit = {
    if (inicio < fim) {
      val pivo = particionar(vetor, inicio, fim)
      quickSort(vetor, inicio, pivo - 1)
      quickSort(vetor, pivo + 1, fim)
    }
  }

  def particionar(vetor: Array[Int], inicio: Int, fim: Int): Int = {
    val pivo = vetor(fim)
    var i = inicio - 1
    for (j <- inicio until fim) {
      if (vetor(j) <= pivo) {
        i += 1
        trocar(vetor, i, j)
      }
    }
    trocar(vetor, i + 1, fim)
    i + 1
  }

  def trocar(vetor: Array[Int], i: Int, j: Int): Unit = {
    val temp = vetor(i)
    vetor(i) = vetor(j)
    vetor(j) = temp
  }
}
```

Esse código implementa o algoritmo de ordenação QuickSort em Scala. Ele recebe um vetor de inteiros como entrada e utiliza a função `quickSort` para ordenar o vetor de forma recursiva. A função `particionar` é responsável por escolher um elemento como pivô e rearranjar os elementos do vetor de forma que todos os elementos menores que o pivô fiquem à sua esquerda, e todos os elementos maiores fiquem à sua direita. A função `trocar` é usada para trocar dois elementos de posição no vetor. O vetor é impresso antes e depois da ordenação para visualizar o resultado.