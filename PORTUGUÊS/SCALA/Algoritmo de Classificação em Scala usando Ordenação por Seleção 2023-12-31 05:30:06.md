Claro! Aqui está um código em Scala que realiza um algoritmo de classificação usando o algoritmo de ordenação por seleção. O código é bastante complexo e inclui comentários explicativos em português para facilitar a compreensão:

```scala
object OrdenacaoPorSelecao {
  def main(args: Array[String]): Unit = {
    // Array de exemplo a ser ordenado
    val array = Array(64, 25, 12, 22, 11)
    
    // Chama a função de ordenação por seleção
    ordenacaoPorSelecao(array)
    
    // Imprime o array ordenado
    println("Array Ordenado:")
    for (i <- array.indices) {
      print(array(i) + " ")
    }
  }
  
  def ordenacaoPorSelecao(array: Array[Int]): Unit = {
    val n = array.length
    
    // Percorre todo o array
    for (i <- 0 until n-1) {
      // Encontra o menor elemento no subarray não ordenado
      var indiceMinimo = i
      for (j <- i+1 until n) {
        if (array(j) < array(indiceMinimo)) {
          indiceMinimo = j
        }
      }
      
      // Troca o elemento atual com o menor elemento encontrado
      val temp = array(indiceMinimo)
      array(indiceMinimo) = array(i)
      array(i) = temp
    }
  }
}
```

Neste código, a função `ordenacaoPorSelecao` implementa o algoritmo de ordenação por seleção. O algoritmo percorre todo o array, busca o menor elemento no subarray não ordenado e o troca com o elemento atual. Isso é repetido até que todo o array esteja ordenado.

No `main`, um array de exemplo é criado e passado para a função de ordenação. Em seguida, o array ordenado é impresso na tela.

Espero que este código atenda às suas expectativas!