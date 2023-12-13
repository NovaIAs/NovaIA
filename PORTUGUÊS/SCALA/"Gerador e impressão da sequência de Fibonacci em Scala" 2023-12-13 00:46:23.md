Claro! Aqui está um código complexo em Scala:

```scala
import scala.collection.mutable.ArrayBuffer

object Fibonacci {
  def main(args: Array[String]): Unit = {
    val n = 50
    val fibonacciSequence = generateFibonacciSequence(n)
    
    println(s"Sequência de Fibonacci dos primeiros $n números:")
    fibonacciSequence.foreach(println)
  }
  
  def generateFibonacciSequence(n: Int): Array[BigInt] = {
    val sequence = ArrayBuffer[BigInt](0, 1)
    
    for (i <- 2 until n) {
      val nextNumber = sequence(i-1) + sequence(i-2)
      sequence.append(nextNumber)
    }
    
    sequence.toArray
  }
}
```

Este código em Scala gera e imprime a sequência de Fibonacci dos primeiros 50 números. Aqui está uma breve explicação de cada parte do código:

1. Na primeira linha, importamos a classe `ArrayBuffer` do pacote `scala.collection.mutable`. A classe `ArrayBuffer` nos permitirá criar e modificar um array dinâmico.

2. Em seguida, declaramos um objeto `Fibonacci` e definimos o método `main` como ponto de entrada do programa.

3. Dentro do método `main`, definimos a variável `n` como o número de elementos que desejamos na sequência de Fibonacci.

4. Criamos um array dinâmico chamado `fibonacciSequence` para armazenar os números da sequência de Fibonacci.

5. Imprimimos a mensagem inicial.

6. Chamamos a função `generateFibonacciSequence` e passamos `n` como argumento. Essa função retorna um array contendo a sequência de Fibonacci.

7. Utilizamos um loop `foreach` para iterar sobre cada número da sequência e imprimi-los.

8. Fora do método `main`, definimos a função `generateFibonacciSequence`. Ela recebe um argumento `n` que representa o tamanho da sequência desejada.

9. Dentro da função, inicializamos o array `sequence` com os dois primeiros números da sequência de Fibonacci: 0 e 1.

10. Utilizamos um loop `for` para calcular os próximos números da sequência. Começamos em 2 e iteramos até `n-1`.

11. Dentro do loop, calculamos o próximo número da sequência somando os dois números anteriores (`sequence(i-1)` e `sequence(i-2)`) e o adicionamos ao final do array `sequence` utilizando o método `append`.

12. Por fim, convertemos o array `sequence` em um array regular utilizando o método `toArray` e o retornamos.

Espero que este código atenda às suas expectativas!