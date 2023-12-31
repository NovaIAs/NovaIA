Claro! Aqui está um código complexo em Scala que envolve a manipulação de listas encadeadas e a implementação de um algoritmo de ordenação chamado Merge Sort. O código está completamente em português e contém comentários explicativos para facilitar a compreensão:

```scala
// Definindo a classe de um nó da lista encadeada
class No(var valor: Int, var proximo: Option[No] = None)

// Implementando a função de ordenação Merge Sort
def mergeSort(lista: Option[No]): Option[No] = {
  // Função auxiliar para dividir a lista em duas partes
  def dividir(lista: Option[No]): (Option[No], Option[No]) = {
    var rapido = lista
    var lento = lista

    while (rapido != None && rapido.get.proximo != None) {
      rapido = rapido.get.proximo.get.proximo
      lento = lento.get.proximo
    }

    val segundaParte = lento.get.proximo
    lento.get.proximo = None
    (lista, segundaParte)
  }

  // Função auxiliar para mesclar duas listas ordenadas
  def mesclar(l1: Option[No], l2: Option[No]): Option[No] = {
    if (l1 == None)
      return l2
    else if (l2 == None)
      return l1

    if (l1.get.valor <= l2.get.valor) {
      l1.get.proximo = mesclar(l1.get.proximo, l2)
      return l1
    } else {
      l2.get.proximo = mesclar(l1, l2.get.proximo)
      return l2
    }
  }

  // Função principal do Merge Sort
  lista match {
    case None | Some(_) => return lista

    case Some(no) => {
      val (primeiraParte, segundaParte) = dividir(lista)
      return mesclar(mergeSort(primeiraParte), mergeSort(segundaParte))
    }
  }
}

// Testando o código
val lista = Some(new No(5, Some(new No(2, Some(new No(9, Some(new No(1))))))))
val listaOrdenada = mergeSort(lista)

// Imprimindo a lista ordenada
var atual = listaOrdenada
while (atual != None) {
  println(atual.get.valor)
  atual = atual.get.proximo
}
```

Este código implementa o algoritmo de ordenação Merge Sort para uma lista encadeada em Scala. A classe `No` representa um nó da lista, contendo um valor inteiro e uma referência para o próximo nó. A função `mergeSort` recebe uma lista encadeada como entrada e retorna a lista ordenada.

A função `dividir` divide a lista em duas partes, usando a técnica do "Fast-Slow pointer". A função `mesclar` mescla duas listas ordenadas em uma única lista ordenada. A função principal `mergeSort` divide a lista em duas partes recursivamente e, em seguida, mescla as partes ordenadas usando a função `mesclar`.

No teste, criamos uma lista com os valores [5, 2, 9, 1] e aplicamos o Merge Sort para ordenar a lista. Em seguida, percorremos a lista ordenada e imprimimos seus valores.