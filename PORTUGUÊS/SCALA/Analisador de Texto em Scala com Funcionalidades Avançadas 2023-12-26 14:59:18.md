Claro! Aqui está um código complexo em Scala que utiliza algumas funcionalidades avançadas da linguagem:

```scala
import scala.io.Source

object AnalisadorTexto {
  def main(args: Array[String]): Unit = {
    // Ler o arquivo de texto
    val texto = Source.fromFile("caminho/para/o/arquivo.txt").getLines().mkString("\n")
    
    // Contar o número de palavras
    val palavras = contarPalavras(texto)
    println(s"Número de palavras: $palavras")
    
    // Contar o número de linhas
    val linhas = contarLinhas(texto)
    println(s"Número de linhas: $linhas")
    
    // Calcular a média de palavras por linha
    val mediaPalavrasPorLinha = calcularMediaPalavrasPorLinha(texto)
    println(s"Média de palavras por linha: $mediaPalavrasPorLinha")
    
    // Imprimir as palavras mais frequentes
    val palavrasFrequentes = encontrarPalavrasFrequentes(texto, 5)
    println("Palavras mais frequentes:")
    palavrasFrequentes.foreach(println)
  }
  
  def contarPalavras(texto: String): Int = {
    texto.split("\\s+").length
  }
  
  def contarLinhas(texto: String): Int = {
    texto.split("\n").length
  }
  
  def calcularMediaPalavrasPorLinha(texto: String): Double = {
    val linhas = texto.split("\n")
    val palavrasPorLinha = linhas.map(_.split("\\s+").length)
    palavrasPorLinha.sum.toDouble / palavrasPorLinha.length.toDouble
  }
  
  def encontrarPalavrasFrequentes(texto: String, n: Int): List[String] = {
    val palavras = texto.split("\\s+")
    val frequencias = palavras.groupBy(identity).mapValues(_.length)
    frequencias.toList.sortBy(-_._2).take(n).map(_._1)
  }
}
```

Neste código, temos um objeto chamado `AnalisadorTexto` que contém diversos métodos para analisar um arquivo de texto. 

Primeiro, o programa lê o arquivo de texto especificado pelo caminho fornecido e armazena seu conteúdo em uma variável chamada `texto`. 

Em seguida, são chamados os seguintes métodos:

- `contarPalavras`: recebe o texto como parâmetro e utiliza a função `split("\\s+")` para dividir o texto em palavras separadas por espaços em branco. Em seguida, é retornada a quantidade de palavras encontradas.

- `contarLinhas`: recebe o texto como parâmetro e utiliza a função `split("\n")` para dividir o texto em linhas separadas por quebras de linha. Em seguida, é retornada a quantidade de linhas encontradas.

- `calcularMediaPalavrasPorLinha`: recebe o texto como parâmetro, utiliza a função `split("\n")` para dividir o texto em linhas e, em seguida, utiliza a função `split("\\s+")` para dividir cada linha em palavras. É calculada a quantidade de palavras por linha e, ao final, é retornada a média.

- `encontrarPalavrasFrequentes`: recebe o texto e um valor `n` como parâmetros. Primeiro, o texto é dividido em palavras utilizando a função `split("\\s+")`. Em seguida, as palavras são agrupadas utilizando a função `groupBy(identity)` e as frequências de cada palavra são contadas utilizando `mapValues(_.length)`. Depois, a lista de frequências é ordenada em ordem decrescente e as `n` palavras mais frequentes são retornadas.

Este código demonstra o uso de algumas funcionalidades avançadas do Scala, como funções de alta ordem (`foreach`, `map`, `sortBy`), expressões lambda (`_._2`) e manipulação de coleções (`split`, `groupBy`, `mapValues`). Ele pode ser útil para analisar e extrair informações de um arquivo de texto.