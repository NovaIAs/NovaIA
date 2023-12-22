Claro! Vou gerar um código em Scala para você. Este código é uma implementação do algoritmo de ordenação Merge Sort, que é um algoritmo de classificação eficiente e divide e conquista. 

Aqui está o código em Scala:

```scala
object MergeSort {

  def mergeSort(lista: List[Int]): List[Int] = {
    val tamanho = lista.length
    if (tamanho <= 1) {
      lista
    } else {
      val (esquerda, direita) = lista.splitAt(tamanho / 2)
      merge(mergeSort(esquerda), mergeSort(direita))
    }
  }

  def merge(esquerda: List[Int], direita: List[Int]): List[Int] = (esquerda, direita) match {
    case (Nil, direita) => direita
    case (esquerda, Nil) => esquerda
    case (esq :: esqResto, dir :: dirResto) =>
      if (esq < dir) esq :: merge(esqResto, direita)
      else dir :: merge(esquerda, dirResto)
  }

  def main(args: Array[String]): Unit = {
    val lista = List(9, 5, 1, 8, 3, 10, 2, 7, 6, 4)
    val listaOrdenada = mergeSort(lista)
    println("Lista original: " + lista)
    println("Lista ordenada: " + listaOrdenada)
  }
}
```

Neste código, a função `mergeSort` recebe uma lista de inteiros e retorna a lista ordenada. Ela começa verificando se a lista possui tamanho 1 ou menos. Se for o caso, a lista já está ordenada e é retornada. Caso contrário, a lista é dividida em duas partes, esquerda e direita, e a função `merge` é chamada para combinar as partes.

A função `merge` recebe duas listas ordenadas, `esquerda` e `direita`, e retorna uma nova lista ordenada combinando as duas. Ela utiliza uma combinação de casamento de padrões para tratar os casos em que uma das listas é vazia. Caso contrário, ela compara o primeiro elemento de cada lista e adiciona o menor na nova lista, chamando recursivamente a função `merge` para combinar o restante das listas.

No método `main`, é criada uma lista de exemplo e o algoritmo Merge Sort é aplicado a ela. Por fim, o resultado é impresso no console.

Espero que este código atenda às suas expectativas!