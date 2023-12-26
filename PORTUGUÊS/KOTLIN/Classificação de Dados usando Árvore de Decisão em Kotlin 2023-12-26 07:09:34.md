Claro! Aqui está um exemplo de código complexo em Kotlin que realiza a classificação de dados usando o algoritmo de árvore de decisão (Decision Tree):

```kotlin
import kotlin.math.log2

data class Exemplo(val atributos: List<Double>, val classe: String)

class No(val atributo: String? = null, val valor: Double? = null, var filhos: MutableMap<Double, No> = mutableMapOf(), var classe: String? = null)

fun calcularEntropia(exemplos: List<Exemplo>): Double {
    val classes = exemplos.groupBy { it.classe }.values
    val total = exemplos.size.toDouble()
    var entropia = 0.0

    for (classe in classes) {
        val probabilidade = classe.size / total
        entropia -= probabilidade * log2(probabilidade)
    }

    return entropia
}

fun calcularGanhoInformacao(exemplos: List<Exemplo>, atributo: Int): Double {
    val atributoValores = exemplos.map { it.atributos[atributo] }.distinct()
    val entropiaInicial = calcularEntropia(exemplos)
    var ganhoInformacao = entropiaInicial

    for (valor in atributoValores) {
        val subconjunto = exemplos.filter { it.atributos[atributo] == valor }
        val probabilidade = subconjunto.size / exemplos.size.toDouble()
        ganhoInformacao -= probabilidade * calcularEntropia(subconjunto)
    }

    return ganhoInformacao
}

fun criarArvoreDecisao(exemplos: List<Exemplo>, atributos: List<String>, atributoClasse: String): No {
    if (exemplos.all { it.classe == exemplos[0].classe }) {
        return No(classe = exemplos[0].classe)
    }

    if (atributos.isEmpty()) {
        val classes = exemplos.groupBy { it.classe }.map { it.key to it.value.size }
        val classeMaisFrequente = classes.maxByOrNull { it.second }?.first
        return No(classe = classeMaisFrequente)
    }

    val ganhosInformacao = atributos.mapIndexed { index, atributo ->
        index to calcularGanhoInformacao(exemplos, index)
    }.toMap()

    val melhorAtributo = ganhosInformacao.maxByOrNull { it.value }?.key
    val novoAtributos = atributos.toMutableList().apply { removeAt(melhorAtributo!!) }

    val no = No(atributo = atributos[melhorAtributo])

    val atributoValores = exemplos.map { it.atributos[melhorAtributo] }.distinct()
    for (valor in atributoValores) {
        val subconjunto = exemplos.filter { it.atributos[melhorAtributo] == valor }
        no.filhos[valor] = criarArvoreDecisao(subconjunto, novoAtributos, atributoClasse)
    }

    return no
}

fun classificarExemplo(exemplo: Exemplo, arvore: No): String {
    var noAtual = arvore

    while (noAtual.classe == null) {
        val valorAtributo = exemplo.atributos[noAtual.atributo?.let { exemplo.atributos.indexOf(it) }!!]
        noAtual = noAtual.filhos[valorAtributo]!!
    }

    return noAtual.classe!!
}

fun main() {
    val exemplos = listOf(
        Exemplo(listOf(6.4, 2.8, 5.6, 2.2), "virginica"),
        Exemplo(listOf(5.0, 2.3, 3.3, 1.0), "versicolor"),
        Exemplo(listOf(4.9, 3.1, 1.5, 0.1), "setosa"),
        // Adicionar mais exemplos aqui...
    )

    val atributos = listOf("sepal_length", "sepal_width", "petal_length", "petal_width")

    val atributoClasse = "classe"

    val arvoreDecisao = criarArvoreDecisao(exemplos, atributos, atributoClasse)

    val exemploClassificar = Exemplo(listOf(5.8, 2.7, 4.1, 1.0), "")

    val classeExemploClassificado = classificarExemplo(exemploClassificar, arvoreDecisao)

    println("Exemplo classificado como: $classeExemploClassificado")
}
```

Este código em Kotlin implementa um algoritmo de árvore de decisão para classificar dados. A função `criarArvoreDecisao` é responsável por construir a árvore de decisão a partir de um conjunto de exemplos de treinamento, atributos e a classe que se deseja classificar. A função `classificarExemplo` usa a árvore de decisão gerada para classificar um exemplo desconhecido.

O código é composto por várias funções auxiliares, como `calcularEntropia` e `calcularGanhoInformacao`, que realizam cálculos estatísticos para determinar a melhor divisão dos dados nos nós da árvore.

No exemplo fornecido, há uma lista de exemplos de treinamento, uma lista de atributos e uma string representando o atributo classe. O algoritmo usa esses dados para construir a árvore de decisão e, em seguida, classifica um exemplo de teste.

É importante ressaltar que esse código pode ser adaptado e personalizado de acordo com as necessidades específicas do problema que você está enfrentando.