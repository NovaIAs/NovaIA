Aqui está um exemplo de código em Scala que implementa um algoritmo de ordenação chamado Merge Sort. O Merge Sort é um algoritmo eficiente que divide a lista em sublistas menores, realiza a ordenação dessas sublistas e, em seguida, combina as sublistas ordenadas em uma única lista ordenada.

```scala
object MergeSort {

  def mergeSort(lista: List[Int]): List[Int] = {
    if (lista.length <= 1) {
      lista
    } else {
      val meio = lista.length / 2
      val (esquerda, direita) = lista.splitAt(meio)
      merge(mergeSort(esquerda), mergeSort(direita))
    }
  }

  def merge(esquerda: List[Int], direita: List[Int]): List[Int] =
    (esquerda, direita) match {
      case (Nil, direita) => direita
      case (esquerda, Nil) => esquerda
      case (cabeçaEsquerda :: caudaEsquerda, cabeçaDireita :: caudaDireita) =>
        if (cabeçaEsquerda < cabeçaDireita) {
          cabeçaEsquerda :: merge(caudaEsquerda, direita)
        } else {
          cabeçaDireita :: merge(esquerda, caudaDireita)
        }
    }

  def main(args: Array[String]): Unit = {
    val listaDesordenada = List(9, 7, 5, 3, 1, 8, 6, 4, 2)
    val listaOrdenada = mergeSort(listaDesordenada)
    println(listaOrdenada)
  }
}
```

Neste código, a função `mergeSort` implementa o algoritmo de divisão e conquista do Merge Sort. Se a lista de entrada tiver apenas um elemento ou nenhum elemento, ela retorna a própria lista. Caso contrário, divide a lista ao meio e chama recursivamente o `mergeSort` nas duas metades, combinando-as posteriormente usando a função `merge`.

A função `merge` combina duas listas ordenadas em uma única lista ordenada. Ela compara o primeiro elemento de cada lista e adiciona o menor elemento à lista resultante. Em seguida, chama recursivamente a função `merge` para combinar o restante das listas.

No método `main`, é criada uma lista desordenada de números inteiros e é chamada a função `mergeSort` para ordená-la. O resultado é impresso no console.

Esse código é uma implementação simples e eficiente do Merge Sort em Scala, que pode ser aplicado para ordenar qualquer lista de números inteiros.