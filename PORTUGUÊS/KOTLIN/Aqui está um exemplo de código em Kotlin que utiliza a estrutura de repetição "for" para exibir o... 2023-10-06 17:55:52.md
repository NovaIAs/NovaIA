Claro! Aqui está um exemplo de um código complexo em Kotlin:

```
fun main() {
    val listaNomes = mutableListOf<String>()
    
    // Preencher a lista com nomes
    for (i in 1..10) {
        listaNomes.add("Nome $i")
    }
    
    // Filtrar os nomes que começam com a letra 'A'
    val nomesFiltrados = listaNomes.filter { nome ->
        nome.startsWith("A")
    }
    
    // Mapear os nomes filtrados para maiúsculas
    val nomesMaiusculos = nomesFiltrados.map { nome ->
        nome.toUpperCase()
    }
    
    // Imprimir os nomes maiúsculos
    nomesMaiusculos.forEach { nome ->
        println(nome)
    }
    
    // Calcular a quantidade de nomes na lista
    val quantidadeNomes = listaNomes.size
    
    // Imprimir a quantidade de nomes
    println("Quantidade de nomes: $quantidadeNomes")
    
    // Criar um mapa para contar a quantidade de ocorrências de cada letra nos nomes
    val mapaOcorrencias = mutableMapOf<Char, Int>()
    
    // Percorrer todos os nomes
    for (nome in listaNomes) {
        // Percorrer as letras de cada nome
        for (letra in nome) {
            // Verificar se a letra já existe no mapa
            if (mapaOcorrencias.containsKey(letra)) {
                // Se existir, incrementar a quantidade de ocorrências
                mapaOcorrencias[letra] = mapaOcorrencias[letra]!! + 1
            } else {
                // Se não existir, adicionar a letra ao mapa com quantidade 1
                mapaOcorrencias[letra] = 1
            }
        }
    }
    
    // Imprimir as ocorrências de cada letra
    for ((letra, ocorrencias) in mapaOcorrencias) {
        println("Letra '$letra' ocorre $ocorrencias vezes")
    }
}
```

Neste exemplo, criamos um programa em Kotlin que realiza diversas operações com uma lista de nomes. Primeiro, preenchemos a lista com 10 nomes. Em seguida, filtramos os nomes que começam com a letra 'A' e os convertemos para maiúsculas. Depois, imprimimos os nomes maiúsculos. Calculamos a quantidade total de nomes na lista e a exibimos na tela.

Além disso, criamos um mapa para contar a quantidade de ocorrências de cada letra nos nomes. Percorremos cada nome e cada letra, verificando se a letra já existe no mapa. Se existir, incrementamos a quantidade de ocorrências; caso contrário, adicionamos a letra ao mapa com quantidade 1. Por fim, imprimimos as ocorrências de cada letra.

Esse código exemplifica diversas funcionalidades do Kotlin, como a manipulação de listas, filtros, mapeamentos, loops e a utilização de mapas.