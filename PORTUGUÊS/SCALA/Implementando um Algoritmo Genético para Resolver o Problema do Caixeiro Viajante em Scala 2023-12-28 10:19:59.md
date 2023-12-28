Claro! Aqui está um exemplo de código em Scala que implementa um algoritmo genético para resolver o problema clássico do caixeiro viajante (TSP - Traveling Salesman Problem):

```scala
import scala.util.Random

object TSPGenetico {

  val cidades = List(
    "Cidade A", "Cidade B", "Cidade C", "Cidade D", "Cidade E",
    "Cidade F", "Cidade G", "Cidade H", "Cidade I", "Cidade J"
  )
  val numCidades = cidades.length
  val tamanhoPopulacao = 100
  val numGeracoes = 1000
  val taxaMutacao = 0.1

  case class Rota(distancia: Double, caminho: List[Int])

  def distancia(cidade1: Int, cidade2: Int): Double = {
    // Implemente aqui o cálculo da distância entre duas cidades
    // Pode ser utilizado o cálculo da distância euclidiana
    // Ou qualquer outro algoritmo de cálculo de distância de sua preferência
    // Mas lembre-se de retornar um valor numérico representando a distância
    // entre as duas cidades
  }

  def gerarRotaAleatoria(): List[Int] = {
    val rota = Random.shuffle(0 until numCidades).toList
    rota
  }

  def calcularDistanciaTotal(caminho: List[Int]): Double = {
    val distanciaTotal = caminho.zip(caminho.tail :+ caminho.head)
      .map { case (cidade1, cidade2) => distancia(cidade1, cidade2) }
      .sum
    distanciaTotal
  }

  def calcularFitness(populacao: List[List[Int]]): List[Rota] = {
    populacao.map { caminho =>
      val distanciaTotal = calcularDistanciaTotal(caminho)
      Rota(distanciaTotal, caminho)
    }
  }

  def selecionarPais(populacao: List[Rota]): List[Rota] = {
    val pais = populacao.sortBy(_.distancia).take(tamanhoPopulacao / 2)
    pais
  }

  def cruzarPais(pais: List[Rota]): List[List[Int]] = {
    val filhos = for {
      pai1 <- pais
      pai2 <- pais
      if pai1 != pai2
    } yield {
      val pontoCorte = Random.between(1, numCidades)
      val filho1 = pai1.caminho.take(pontoCorte) ++ pai2.caminho.drop(pontoCorte)
      val filho2 = pai2.caminho.take(pontoCorte) ++ pai1.caminho.drop(pontoCorte)
      List(filho1, filho2)
    }
    filhos.flatten
  }

  def mutarPopulacao(populacao: List[List[Int]]): List[List[Int]] = {
    populacao.map { caminho =>
      if (Random.nextDouble() < taxaMutacao) {
        val posicao1 = Random.between(0, numCidades)
        val posicao2 = Random.between(0, numCidades)
        val caminhoMutado = caminho.updated(posicao1, caminho(posicao2)).updated(posicao2, caminho(posicao1))
        caminhoMutado
      } else {
        caminho
      }
    }
  }

  def encontrarMelhorRota(): Rota = {
    var populacao = List.fill(tamanhoPopulacao)(gerarRotaAleatoria())
    var geracao = 0

    while (geracao < numGeracoes) {
      val populacaoFitness = calcularFitness(populacao)
      val melhorRotaGeracao = populacaoFitness.minBy(_.distancia)
      val pais = selecionarPais(populacaoFitness)
      val filhos = cruzarPais(pais)
      populacao = mutarPopulacao(filhos)
      geracao += 1
    }

    val populacaoFitness = calcularFitness(populacao)
    val melhorRota = populacaoFitness.minBy(_.distancia)

    melhorRota
  }

  def main(args: Array[String]): Unit = {
    val melhorRota = encontrarMelhorRota()

    println("Melhor rota encontrada:")
    println(melhorRota.caminho.map(cidades(_)).mkString(" -> "))
    println("Distância total: " + melhorRota.distancia)
  }

}
```

Neste código, primeiro definimos uma lista de cidades e o tamanho da população inicial, o número de gerações e a taxa de mutação para o algoritmo genético.

A seguir, temos a definição da classe `Rota`, que representa uma rota possível. Essa classe possui uma distância total e uma lista de cidades que compõem o caminho.

A função `distancia` é responsável por calcular a distância entre duas cidades, de acordo com a fórmula que você escolher para o problema.

A função `gerarRotaAleatoria` gera uma rota aleatória, embaralhando a lista de cidades.

A função `calcularDistanciaTotal` recebe um caminho (lista de cidades) e calcula a distância total percorrida ao visitar todas essas cidades.

A função `calcularFitness` recebe uma população de rotas e retorna uma lista de rotas com suas respectivas distâncias totais.

A função `selecionarPais` seleciona os melhores pais para a próxima geração, com base na distância total.

A função `cruzarPais` realiza o cruzamento entre os pais selecionados, gerando uma lista de filhos.

A função `mutarPopulacao` realiza a mutação dos indivíduos na população, com uma probabilidade definida.

A função `encontrarMelhorRota` é responsável por executar o algoritmo genético completo, gerando a população inicial, evoluindo a população através de seleção, cruzamento e mutação, até chegar ao número máximo de gerações. No final, retorna a melhor rota encontrada.

Por fim, a função `main` chama a função `encontrarMelhorRota` e imprime a melhor rota encontrada, junto com sua distância total.

Espero que este código atenda às suas expectativas! Lembre-se de ajustar os parâmetros conforme a necessidade do problema e implementar a função `distancia` de acordo com o cálculo desejado.