Claro! Aqui está um código complexo em Scala que envolve o uso de classes, herança, padrões de projeto e manipulação de arquivos.

```scala
import scala.io.Source
import java.io.{File, PrintWriter}

// Classe abstrata para representar uma forma geométrica
abstract class FormaGeometrica {
  def area(): Double
  def perimetro(): Double
}

// Classe para representar um retângulo
class Retangulo(largura: Double, altura: Double) extends FormaGeometrica {
  override def area(): Double = largura * altura
  override def perimetro(): Double = 2 * (largura + altura)
}

// Classe para representar um círculo
class Circulo(raio: Double) extends FormaGeometrica {
  override def area(): Double = Math.PI * Math.pow(raio, 2)
  override def perimetro(): Double = 2 * Math.PI * raio
}

// Classe para representar um quadrado
class Quadrado(lado: Double) extends Retangulo(lado, lado)

// Classe para representar um triângulo
class Triangulo(base: Double, altura: Double) extends FormaGeometrica {
  override def area(): Double = (base * altura) / 2
  override def perimetro(): Double = base + altura + Math.sqrt(Math.pow(base, 2) + Math.pow(altura, 2))
}

// Classe que representa uma calculadora de formas geométricas
object CalculadoraFormasGeometricas {
  def calcularArea(forma: FormaGeometrica): Double = forma.area()
  def calcularPerimetro(forma: FormaGeometrica): Double = forma.perimetro()
}

// Classe para representar um arquivo de saída
class ArquivoSaida(nomeArquivo: String) {
  private val file = new File(nomeArquivo)
  private val writer = new PrintWriter(file)

  def escreverLinhas(linhas: List[String]): Unit = {
    linhas.foreach(line => writer.write(line + "\n"))
    writer.close()
  }
}

// Classe que processa as formas geométricas e escreve os resultados em um arquivo de saída
object ProcessadorFormasGeometricas {
  def processarFormasGeometricas(formas: List[FormaGeometrica], nomeArquivoSaida: String): Unit = {
    val linhas = formas.map(forma => s"Área: ${CalculadoraFormasGeometricas.calcularArea(forma)}, Perímetro: ${CalculadoraFormasGeometricas.calcularPerimetro(forma)}")
    val arquivoSaida = new ArquivoSaida(nomeArquivoSaida)
    arquivoSaida.escreverLinhas(linhas)
  }
}

// Exemplo de uso do código
object Main {
  def main(args: Array[String]): Unit = {
    val retangulo = new Retangulo(5, 8)
    val circulo = new Circulo(3)
    val quadrado = new Quadrado(4)
    val triangulo = new Triangulo(6, 8)

    val formas = List(retangulo, circulo, quadrado, triangulo)

    ProcessadorFormasGeometricas.processarFormasGeometricas(formas, "resultados.txt")
  }
}
```

Nesse código, temos a definição de várias classes para representar diferentes formas geométricas, como retângulo, círculo, quadrado e triângulo. Cada forma geométrica implementa os métodos `area()` e `perimetro()` da classe abstrata `FormaGeometrica`.

A classe `CalculadoraFormasGeometricas` possui métodos estáticos para calcular a área e o perímetro de uma forma geométrica.

A classe `ArquivoSaida` representa um arquivo de saída, onde podemos escrever linhas de texto. A classe `ProcessadorFormasGeometricas` recebe uma lista de formas geométricas, calcula a área e o perímetro de cada uma delas usando a `CalculadoraFormasGeometricas` e escreve os resultados em um arquivo de saída.

No exemplo de uso no objeto `Main`, criamos algumas instâncias de formas geométricas e as adicionamos em uma lista. Em seguida, chamamos o método `processarFormasGeometricas` do `ProcessadorFormasGeometricas` passando a lista de formas e o nome do arquivo de saída. Os resultados serão escritos no arquivo "resultados.txt".