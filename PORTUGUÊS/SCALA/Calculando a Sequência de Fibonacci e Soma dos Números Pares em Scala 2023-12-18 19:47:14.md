Claro, aqui está um código complexo em Scala:

```scala
import scala.collection.mutable

object Fibonacci {
  def main(args: Array[String]): Unit = {
    val n = 10

    val fibonacciSeq = calcularFibonacci(n)
    val somaPares = calcularSomaPares(fibonacciSeq)

    println(s"Os primeiros $n números da sequência de Fibonacci são: $fibonacciSeq")
    println(s"A soma dos números pares na sequência é: $somaPares")
  }

  def calcularFibonacci(n: Int): List[Int] = {
    val fibonacciSeq = mutable.ListBuffer(0, 1)

    while (fibonacciSeq.length < n) {
      val nextNumber = fibonacciSeq(fibonacciSeq.length - 1) + fibonacciSeq(fibonacciSeq.length - 2)
      fibonacciSeq += nextNumber
    }

    fibonacciSeq.toList
  }

  def calcularSomaPares(seq: List[Int]): Int = {
    seq.filter(_ % 2 == 0).sum
  }
}
```

Explicação do código:

1. Começamos importando a classe `mutable` do pacote `scala.collection`, que contém a implementação de uma lista mutável.
2. Em seguida, declaramos o objeto `Fibonacci` que será responsável por calcular a sequência de Fibonacci.
3. Dentro do objeto `Fibonacci`, temos o método `main` que é o ponto de entrada do programa. Ele recebe um array de argumentos como parâmetro, mas neste caso não estamos utilizando.
4. Definimos a variável `n` com o valor 10, que representa a quantidade de números da sequência de Fibonacci que queremos calcular.
5. Chamamos o método `calcularFibonacci` passando `n` como argumento para obter a sequência de Fibonacci.
6. Em seguida, chamamos o método `calcularSomaPares` passando a sequência de Fibonacci como argumento para calcular a soma dos números pares.
7. Por fim, imprimimos os resultados na tela utilizando a função `println`.

A função `calcularFibonacci` utiliza um laço `while` para gerar a sequência de Fibonacci. Começamos com os dois primeiros números da sequência (0 e 1) e adicionamos o próximo número somando os dois últimos números da sequência. Repetimos esse processo até obter a sequência com `n` números desejados.

A função `calcularSomaPares` recebe a sequência de Fibonacci como parâmetro e utiliza a função `filter` para filtrar apenas os números pares. Em seguida, utilizamos a função `sum` para calcular a soma desses números.

Espero que este código atenda às suas expectativas.