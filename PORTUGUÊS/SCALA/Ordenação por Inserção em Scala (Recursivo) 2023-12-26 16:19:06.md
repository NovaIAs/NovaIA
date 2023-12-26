Claro! Aqui está um código complexo em Scala que implementa um algoritmo de Ordenação por Inserção de forma recursiva:

```scala
object OrdenacaoPorInsercao {

  def main(args: Array[String]): Unit = {
    val array = Array(9, 7, 5, 3, 1, 2, 4, 6, 8)
    println("Array original: " + array.mkString(" "))
    ordenar(array)
    println("Array ordenado: " + array.mkString(" "))
  }

  def ordenar(array: Array[Int]): Unit = {
    if (array.length > 1) {
      ordenar(array, array.length - 1)
    }
  }

  def ordenar(array: Array[Int], n: Int): Unit = {
    if (n > 0) {
      ordenar(array, n - 1)
      inserir(array, n)
    }
  }

  def inserir(array: Array[Int], n: Int): Unit = {
    val chave = array(n)
    var j = n - 1

    while (j >= 0 && array(j) > chave) {
      array(j + 1) = array(j)
      j -= 1
    }

    array(j + 1) = chave
  }
}
```

Neste código, implementamos a Ordenação por Inserção de forma recursiva. A função `main` é o ponto de entrada do programa, onde criamos um array desordenado com valores inteiros. Em seguida, chamamos a função `ordenar`, passando o array como parâmetro.

A função `ordenar` verifica se o array possui mais de um elemento. Caso possua, chama a função `ordenar` novamente, passando o array e o índice do último elemento como parâmetros.

A função `ordenar` recursiva tem como objetivo inserir o elemento do índice `n` na posição correta dentro do array. Primeiro, ela chama a função `ordenar` novamente, passando o array e o índice `n - 1`. Em seguida, chama a função `inserir` para realizar a inserção do elemento na posição correta.

A função `inserir` realiza a inserção do elemento na posição correta do array, utilizando um laço `while` para deslocar os elementos maiores que a chave uma posição para a direita. Por fim, insere a chave na posição correta.

O resultado será a impressão do array original desordenado e do array final ordenado.