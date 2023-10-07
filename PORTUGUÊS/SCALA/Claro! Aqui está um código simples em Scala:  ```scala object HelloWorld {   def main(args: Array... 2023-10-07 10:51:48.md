Claro! Aqui está um código complexo em Scala:

```scala
import scala.collection.mutable

object ComplexCode {
  def main(args: Array[String]): Unit = {
    // Criando uma lista vazia
    val lista: mutable.ListBuffer[Int] = mutable.ListBuffer()

    // Gerando números aleatórios e adicionando-os à lista
    for (i <- 1 to 10) {
      val numero = scala.util.Random.nextInt(100)
      lista += numero
    }

    // Imprimindo a lista original
    println("Lista original:")
    println(lista)

    // Filtrando apenas os números pares da lista
    val listaPares = lista.filter(_ % 2 == 0)

    // Imprimindo a lista de números pares
    println("Lista de números pares:")
    println(listaPares)

    // Calculando a média dos números pares
    val mediaPares = listaPares.sum.toDouble / listaPares.length

    // Imprimindo a média dos números pares
    println("Média dos números pares:")
    println(mediaPares)

    // Ordenando a lista original em ordem decrescente
    val listaOrdenada = lista.sorted.reverse

    // Imprimindo a lista original ordenada em ordem decrescente
    println("Lista original ordenada em ordem decrescente:")
    println(listaOrdenada)

    // Criando um mapa vazio
    val mapa: mutable.Map[Int, Int] = mutable.Map()

    // Contando a quantidade de ocorrências de cada número na lista original
    for (numero <- lista) {
      if (mapa.contains(numero)) {
        mapa(numero) += 1
      } else {
        mapa(numero) = 1
      }
    }

    // Imprimindo o mapa com as contagens
    println("Mapa com as contagens:")
    println(mapa)
  }
}
```

Este código em Scala realiza as seguintes operações:

1. Cria uma lista vazia usando a classe `mutable.ListBuffer`.
2. Gera 10 números aleatórios e os adiciona à lista.
3. Imprime a lista original.
4. Filtra apenas os números pares da lista usando o método `filter`.
5. Imprime a lista de números pares.
6. Calcula a média dos números pares.
7. Imprime a média dos números pares.
8. Ordena a lista original em ordem decrescente usando os métodos `sorted` e `reverse`.
9. Imprime a lista original ordenada em ordem decrescente.
10. Cria um mapa vazio usando a classe `mutable.Map`.
11. Conta a quantidade de ocorrências de cada número na lista original e armazena as contagens no mapa.
12. Imprime o mapa com as contagens.

Espero que este exemplo atenda às suas expectativas! Se você tiver mais alguma solicitação, estou à disposição.