Claro! Aqui está um exemplo de código em Scala que utiliza recursos avançados da linguagem e implementa um algoritmo de ordenação chamado Merge Sort:

```scala
object MergeSort {
  def mergeSort(lista: List[Int]): List[Int] = {
    if (lista.length <= 1) lista
    else {
      val meio = lista.length / 2
      val (esquerda, direita) = lista.splitAt(meio)
      merge(mergeSort(esquerda), mergeSort(direita))
    }
  }

  def merge(esquerda: List[Int], direita: List[Int]): List[Int] = {
    (esquerda, direita) match {
      case (Nil, direita) => direita
      case (esquerda, Nil) => esquerda
      case (headE :: tailE, headD :: tailD) =>
        if (headE < headD) headE :: merge(tailE, direita)
        else headD :: merge(esquerda, tailD)
    }
  }

  def main(args: Array[String]): Unit = {
    val lista = List(9, 5, 1, 8, 3, 10, 4, 2, 7, 6)
    val listaOrdenada = mergeSort(lista)
    println("Lista Ordenada: " + listaOrdenada)
  }
}
```

Neste código, a função `mergeSort` implementa o algoritmo de ordenação Merge Sort. Ela recebe uma lista de inteiros como parâmetro e retorna a lista ordenada. 

Primeiro, é verificado se a lista tem tamanho menor ou igual a 1. Nesse caso, a lista é retornada, pois já está ordenada. Caso contrário, a lista é dividida em duas partes, a esquerda e a direita, utilizando o método `splitAt`. Em seguida, é feita uma chamada recursiva para ordenar as duas partes separadamente através de `mergeSort`.

A função `merge` é responsável por combinar as duas listas ordenadas em uma única lista ordenada. Ela recebe a lista da esquerda e a lista da direita como parâmetros. São feitos três casos de casamento de padrões: se uma das listas for vazia, a outra lista é retornada; caso contrário, o primeiro elemento de cada lista é comparado e o menor é adicionado à lista resultante. Essa comparação e adição são feitas de forma recursiva usando a função `merge`.

No método `main`, é criada uma lista desordenada de números inteiros e a função `mergeSort` é chamada para ordená-la. O resultado é impresso no console.