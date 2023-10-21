Claro! Aqui está um código complexo em Scala que implementa um algoritmo de ordenação chamado Merge Sort:

```scala
object MergeSort {

  def mergeSort(arr: Array[Int]): Array[Int] = {
    if (arr.length <= 1) arr
    else {
      val mid = arr.length / 2
      val (left, right) = arr.splitAt(mid)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def merge(left: Array[Int], right: Array[Int]): Array[Int] = {
    var i = 0
    var j = 0
    val merged = new Array[Int](left.length + right.length)
    var index = 0

    while (i < left.length && j < right.length) {
      if (left(i) <= right(j)) {
        merged(index) = left(i)
        i += 1
      } else {
        merged(index) = right(j)
        j += 1
      }
      index += 1
    }

    while (i < left.length) {
      merged(index) = left(i)
      i += 1
      index += 1
    }

    while (j < right.length) {
      merged(index) = right(j)
      j += 1
      index += 1
    }

    merged
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(9, 5, 1, 8, 3, 7, 2, 6, 4)
    val sortedArr = mergeSort(arr)
    println("Array original: " + arr.mkString(", "))
    println("Array ordenado: " + sortedArr.mkString(", "))
  }
}
```

Explicação do código:
1. A função `mergeSort` é a implementação do algoritmo Merge Sort. Ela recebe um array de inteiros e retorna o mesmo array ordenado.
2. A condição de parada da recursão é quando o array tem tamanho igual ou menor que 1, pois arrays vazios ou com um único elemento já são considerados ordenados.
3. A função `splitAt` é utilizada para dividir o array em duas partes iguais, gerando os arrays `left` e `right`.
4. Os arrays `left` e `right` são ordenados chamando recursivamente a função `mergeSort`.
5. A função `merge` é responsável por mesclar os arrays `left` e `right` em um único array ordenado.
6. Nessa função, são utilizados três ponteiros: `i` para o array `left`, `j` para o array `right` e `index` para o array resultante `merged`.
7. O laço while é utilizado para percorrer os arrays `left` e `right` comparando os elementos e inserindo-os no array `merged` em ordem crescente.
8. Os laços while subsequentes são utilizados para inserir os elementos que sobraram de um dos arrays, caso eles tenham tamanhos diferentes.
9. No método `main`, é criado um array de exemplo, `arr`, com números desordenados.
10. A função `mergeSort` é chamada passando o array `arr` e o resultado é armazenado em `sortedArr`.
11. Por fim, o array original e o array ordenado são impressos no console.

Esse código implementa uma das técnicas mais eficientes de ordenação e é comumente utilizada em diversas aplicações.