Claro! Aqui está um código em Scala que implementa um algoritmo de ordenação chamado Merge Sort. O Merge Sort é um algoritmo eficiente que utiliza a técnica de "dividir para conquistar" para ordenar uma lista de elementos.

```scala
object MergeSort {
  def mergeSort(lista: List[Int]): List[Int] = {
    // Verifica se a lista está vazia ou contém apenas um elemento
    if (lista.isEmpty || lista.tail.isEmpty) {
      lista
    } else {
      val (esquerda, direita) = lista.splitAt(lista.length / 2)
      merge(mergeSort(esquerda), mergeSort(direita))
    }
  }

  def merge(esquerda: List[Int], direita: List[Int]): List[Int] = {
    (esquerda, direita) match {
      case (_, Nil) => esquerda
      case (Nil, _) => direita
      case (x :: xs, y :: ys) =>
        if (x <= y) {
          x :: merge(xs, direita)
        } else {
          y :: merge(esquerda, ys)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    val lista = List(5, 2, 8, 3, 1, 9, 4, 7, 6)
    val listaOrdenada = mergeSort(lista)
    println("Lista original: " + lista.mkString(", "))
    println("Lista ordenada: " + listaOrdenada.mkString(", "))
  }
}
```

Neste código, a função `mergeSort` implementa o algoritmo de ordenação. Ela recebe uma lista de inteiros como entrada e verifica se a lista está vazia ou contém apenas um elemento. Caso seja um dos casos base, a lista é retornada diretamente. Caso contrário, a lista é dividida em duas metades, a função `mergeSort` é chamada recursivamente para cada metade e, por fim, as duas metades são mescladas usando a função `merge`.

A função `merge` recebe duas listas ordenadas como entrada e realiza a mesclagem. Ela verifica se uma das listas está vazia e, nesse caso, retorna a outra lista como resultado. Caso contrário, ela compara o primeiro elemento de cada lista e insere o menor na lista resultante, chamando recursivamente a função `merge` com a lista de entrada apropriada.

No método `main`, uma lista de exemplo é criada e a função `mergeSort` é chamada para ordená-la. Em seguida, as listas original e ordenada são impressas no console.

Espero que este código atenda às suas necessidades!