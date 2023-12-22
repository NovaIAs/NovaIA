Aqui está um exemplo de código complexo em Kotlin que implementa um algoritmo genético para resolver o problema clássico do Caixeiro Viajante:

```kotlin
import kotlin.random.Random

data class Cidade(val nome: String, val coordenadas: Pair<Double, Double>)

class PopulacaoInicial(private val cidades: List<Cidade>, private val tamanhoPopulacao: Int) {
    private val populacao: MutableList<List<Cidade>> = mutableListOf()

    init {
        gerarPopulacaoInicial()
    }

    private fun gerarPopulacaoInicial() {
        repeat(tamanhoPopulacao) {
            val cidadesAleatorias = cidades.shuffled()
            populacao.add(cidadesAleatorias)
        }
    }

    fun getPopulacao(): List<List<Cidade>> {
        return populacao
    }
}

class AlgoritmoGenetico(
    private val cidades: List<Cidade>,
    private val tamanhoPopulacao: Int,
    private val taxaMutacao: Double,
    private val geracoes: Int
) {
    private var populacaoAtual: List<List<Cidade>> = listOf()

    fun executar(): List<Cidade> {
        val populacaoInicial = PopulacaoInicial(cidades, tamanhoPopulacao)
        populacaoAtual = populacaoInicial.getPopulacao()

        repeat(geracoes) {
            val novaPopulacao = mutableListOf<List<Cidade>>()

            while (novaPopulacao.size < tamanhoPopulacao) {
                val pai1 = selecaoTorneio()
                val pai2 = selecaoTorneio()

                val filho = crossover(pai1, pai2)
                novaPopulacao.add(mutacao(filho))
            }

            populacaoAtual = novaPopulacao
        }

        return obterMelhorIndividuo()
    }

    private fun selecaoTorneio(): List<Cidade> {
        val individuo1 = populacaoAtual[Random.nextInt(populacaoAtual.size)]
        val individuo2 = populacaoAtual[Random.nextInt(populacaoAtual.size)]

        return if (fitness(individuo1) < fitness(individuo2)) individuo1 else individuo2
    }

    private fun crossover(pai1: List<Cidade>, pai2: List<Cidade>): List<Cidade> {
        val filho = pai1.toMutableList()
        val pontosCorte = listOf(Random.nextInt(pai1.size), Random.nextInt(pai1.size)).sorted()

        for (i in pontosCorte[0] until pontosCorte[1]) {
            filho[i] = pai2[i]
        }

        return filho
    }

    private fun mutacao(individuo: List<Cidade>): List<Cidade> {
        val individuoMutado = individuo.toMutableList()

        if (Random.nextDouble() < taxaMutacao) {
            val index1 = Random.nextInt(individuoMutado.size)
            val index2 = Random.nextInt(individuoMutado.size)

            val cidade1 = individuoMutado[index1]
            individuoMutado[index1] = individuoMutado[index2]
            individuoMutado[index2] = cidade1
        }

        return individuoMutado
    }

    private fun fitness(individuo: List<Cidade>): Double {
        var distancia = calcularDistancia(individuo[0], individuo[individuo.size - 1])

        for (i in 1 until individuo.size) {
            distancia += calcularDistancia(individuo[i], individuo[i - 1])
        }

        return 1 / distancia
    }

    private fun calcularDistancia(cidade1: Cidade, cidade2: Cidade): Double {
        val xDiff = cidade1.coordenadas.first - cidade2.coordenadas.first
        val yDiff = cidade1.coordenadas.second - cidade2.coordenadas.second
        return Math.sqrt(xDiff * xDiff + yDiff * yDiff)
    }

    private fun obterMelhorIndividuo(): List<Cidade> {
        return populacaoAtual.maxByOrNull { fitness(it) } ?: emptyList()
    }
}

fun main() {
    val cidades = listOf(
        Cidade("A", Pair(0.0, 0.0)),
        Cidade("B", Pair(1.0, 5.0)),
        Cidade("C", Pair(2.0, 3.0)),
        Cidade("D", Pair(4.0, 2.0)),
        Cidade("E", Pair(6.0, 4.0))
    )

    val tamanhoPopulacao = 100
    val taxaMutacao = 0.1
    val geracoes = 100

    val algoritmoGenetico = AlgoritmoGenetico(cidades, tamanhoPopulacao, taxaMutacao, geracoes)
    val melhorRota = algoritmoGenetico.executar()

    println("Melhor rota encontrada: ")
    melhorRota.forEachIndexed { index, cidade ->
        println("${index + 1}. ${cidade.nome}")
    }
}
```

Este código implementa um algoritmo genético para resolver o problema do Caixeiro Viajante, onde o algoritmo tenta encontrar a rota mais curta que visita todas as cidades uma única vez e volta ao ponto de partida.

O código começa definindo uma classe `Cidade` que representa uma cidade com um nome e suas coordenadas. Em seguida, a classe `PopulacaoInicial` é criada para gerar uma população inicial de rotas aleatórias.

A classe principal `AlgoritmoGenetico` é responsável por executar o algoritmo genético. Ela possui métodos para seleção de pais, crossover, mutação, cálculo de fitness (distância percorrida) e obtenção do melhor indivíduo da população.

No método `main`, são definidas as cidades e configurados os parâmetros do algoritmo genético. Em seguida, o algoritmo é executado e a melhor rota encontrada é exibida no console.

O código utiliza conceitos de programação orientada a objetos, estruturas de dados como listas e pares de coordenadas, além de funções para realizar as operações do algoritmo genético.

Este é apenas um exemplo de um código complexo em Kotlin, e existem diversas outras abordagens e implementações possíveis para resolver o problema do Caixeiro Viajante e outros desafios de programação.