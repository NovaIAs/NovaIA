Aqui está um código complexo em Scala que realiza uma tarefa de processamento de dados com explicações detalhadas:

```scala
import scala.io.Source

// Função para calcular a média dos valores em um array
def calcularMedia(valores: Array[Double]): Double = {
  val soma = valores.sum
  val quantidade = valores.length
  soma / quantidade
}

// Função para calcular o desvio padrão dos valores em um array
def calcularDesvioPadrao(valores: Array[Double]): Double = {
  val media = calcularMedia(valores)
  val somaQuadrados = valores.map(valor => Math.pow(valor - media, 2)).sum
  Math.sqrt(somaQuadrados / valores.length)
}

// Função principal
def main(args: Array[String]): Unit = {
  // Lendo os valores de um arquivo de entrada
  val linhas = Source.fromFile("entrada.txt").getLines().toList
  val valores = linhas.flatMap(_.split(" ")).map(_.toDouble)
  
  // Calculando a média e o desvio padrão dos valores
  val media = calcularMedia(valores)
  val desvioPadrao = calcularDesvioPadrao(valores)
  
  // Imprimindo os resultados
  println(s"Média: $media")
  println(s"Desvio padrão: $desvioPadrao")
}
```

Neste código, começamos importando a biblioteca `scala.io.Source` para ler os valores de entrada de um arquivo.

Em seguida, temos duas funções principais: `calcularMedia` e `calcularDesvioPadrao`. A função `calcularMedia` recebe um array de valores e retorna a média deles. A função `calcularDesvioPadrao` recebe um array de valores e retorna o desvio padrão.

Na função `main`, começamos lendo as linhas do arquivo de entrada e convertendo-as em uma lista de valores. Em seguida, chamamos as funções `calcularMedia` e `calcularDesvioPadrao` para obter a média e o desvio padrão dos valores.

Por fim, imprimimos os resultados na tela utilizando a função `println`.

Certifique-se de ter um arquivo chamado "entrada.txt" no mesmo diretório do código Scala, contendo os valores de entrada separados por espaços.