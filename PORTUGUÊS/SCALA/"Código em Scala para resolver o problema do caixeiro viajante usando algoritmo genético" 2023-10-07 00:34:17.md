Claro! Aqui está um exemplo de código em Scala que implementa um algoritmo genético para resolver o problema do caixeiro viajante (Traveling Salesman Problem - TSP):

```scala
import scala.util.Random

object TSPGeneticAlgorithm {
  case class City(id: Int, x: Double, y: Double)
  case class Route(cities: List[City])

  def calculateDistance(city1: City, city2: City): Double = {
    val dx = city1.x - city2.x
    val dy = city1.y - city2.y
    Math.sqrt(dx * dx + dy * dy)
  }

  def calculateRouteDistance(route: Route): Double = {
    route.cities.zip(route.cities.tail :+ route.cities.head)
      .map { case (city1, city2) => calculateDistance(city1, city2) }
      .sum
  }

  def generateRandomRoute(cities: List[City]): Route = {
    Route(Random.shuffle(cities))
  }

  def generateInitialPopulation(cities: List[City], populationSize: Int): List[Route] = {
    List.fill(populationSize)(generateRandomRoute(cities))
  }

  def crossover(route1: Route, route2: Route): Route = {
    val cities1 = route1.cities.toSet
    val cities2 = route2.cities.toSet
    val commonCities = cities1.intersect(cities2)
    val missingCities = (cities1 diff commonCities).toList

    def helper(route: Route, cities: List[City]): Route = cities match {
      case Nil => route
      case head :: tail =>
        val index = route.cities.indexOf(head)
        val newCities = route.cities.take(index) ++ route.cities.drop(index).filterNot(commonCities.contains)
        helper(Route(newCities :+ head), tail)
    }

    helper(route1, missingCities)
  }

  def mutation(route: Route): Route = {
    val (left, right) = route.cities.splitAt(Random.nextInt(route.cities.length))
    Route(left ++ right.reverse)
  }

  def evolvePopulation(population: List[Route], eliteSize: Int): List[Route] = {
    val elite = population.sortBy(calculateRouteDistance).take(eliteSize)
    val offspring = for (_ <- elite.indices) yield {
      val parent1 = elite(Random.nextInt(eliteSize))
      val parent2 = elite(Random.nextInt(eliteSize))
      crossover(parent1, parent2)
    }
    elite ++ offspring.map(mutation)
  }

  def findShortestRoute(cities: List[City], populationSize: Int, eliteSize: Int, generations: Int): Route = {
    val initialPopulation = generateInitialPopulation(cities, populationSize)
    val finalPopulation = (0 until generations).foldLeft(initialPopulation) { case (population, _) =>
      evolvePopulation(population, eliteSize)
    }
    finalPopulation.minBy(calculateRouteDistance)
  }

  def main(args: Array[String]): Unit = {
    val cities = List(
      City(1, 1.0, 1.0),
      City(2, 2.0, 2.0),
      City(3, 3.0, 3.0),
      // adicione mais cidades aqui...
    )

    val populationSize = 100
    val eliteSize = 20
    val generations = 100

    val shortestRoute = findShortestRoute(cities, populationSize, eliteSize, generations)
    val shortestDistance = calculateRouteDistance(shortestRoute)

    println(s"Menor distância encontrada: $shortestDistance")
    println(s"Melhor rota encontrada: ${shortestRoute.cities.map(_.id).mkString(" -> ")}")
  }
}
```

Neste código, temos um objeto `TSPGeneticAlgorithm` que contém várias funções para resolver o problema do caixeiro viajante utilizando um algoritmo genético.

O primeiro passo é definir as classes `City` e `Route`, que representam uma cidade e uma rota (sequência de cidades), respectivamente. A função `calculateDistance` calcula a distância euclidiana entre duas cidades. A função `calculateRouteDistance` calcula a distância total de uma rota, somando as distâncias entre cada par de cidades consecutivas.

A função `generateRandomRoute` gera uma rota aleatória a partir de uma lista de cidades. A função `generateInitialPopulation` gera uma população inicial de rotas aleatórias. A função `crossover` realiza o cruzamento entre duas rotas, misturando as cidades em comum e mantendo a ordem das cidades não comuns. A função `mutation` realiza uma mutação em uma rota, invertendo a ordem de uma parte aleatória da rota.

A função `evolvePopulation` recebe uma população de rotas e produz uma nova população evoluída, selecionando as rotas mais aptas para sobreviverem (elite) e gerando novas rotas por cruzamento e mutação. A função `findShortestRoute` é responsável por encontrar a rota de menor distância entre todas as gerações.

No método `main`, definimos uma lista de cidades de exemplo e os parâmetros do algoritmo (tamanho da população, tamanho da elite e número de gerações). Em seguida, chamamos a função `findShortestRoute` para obter a rota de menor distância encontrada e exibimos os resultados na tela.

Este código é apenas um exemplo de implementação do algoritmo genético para resolver o problema do caixeiro viajante em Scala. É possível adaptá-lo e personalizá-lo de acordo com as necessidades específicas do problema em questão.