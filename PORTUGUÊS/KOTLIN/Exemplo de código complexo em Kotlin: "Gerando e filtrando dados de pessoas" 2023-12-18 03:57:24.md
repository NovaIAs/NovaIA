Claro! Aqui está um exemplo de código Kotlin complexo que você pode usar como ponto de partida:

```kotlin
import kotlin.random.Random

class Pessoa(val nome: String, val idade: Int)

fun main() {
    val pessoas = mutableListOf<Pessoa>()

    for (i in 1..10) {
        val nome = gerarNomeAleatorio()
        val idade = gerarIdadeAleatoria()

        val pessoa = Pessoa(nome, idade)
        pessoas.add(pessoa)
    }

    val pessoasMaioresDe18 = filtrarPessoasMaioresDe18(pessoas)

    val pessoaMaisVelha = encontrarPessoaMaisVelha(pessoasMaioresDe18)

    imprimirDadosPessoas(pessoas)
    imprimirDadosPessoas(pessoasMaioresDe18)
    imprimirPessoaMaisVelha(pessoaMaisVelha)
}

fun gerarNomeAleatorio(): String {
    val nomes = listOf("João", "Maria", "Pedro", "Ana", "Carlos")
    return nomes.random()
}

fun gerarIdadeAleatoria(): Int {
    return Random.nextInt(1, 50)
}

fun filtrarPessoasMaioresDe18(pessoas: List<Pessoa>): List<Pessoa> {
    return pessoas.filter { it.idade >= 18 }
}

fun encontrarPessoaMaisVelha(pessoas: List<Pessoa>): Pessoa? {
    return pessoas.maxByOrNull { it.idade }
}

fun imprimirDadosPessoas(pessoas: List<Pessoa>) {
    println("Dados das pessoas:")
    for (pessoa in pessoas) {
        println("Nome: ${pessoa.nome}, Idade: ${pessoa.idade}")
    }
    println()
}

fun imprimirPessoaMaisVelha(pessoa: Pessoa?) {
    if (pessoa != null) {
        println("Pessoa mais velha:")
        println("Nome: ${pessoa.nome}, Idade: ${pessoa.idade}")
        println()
    } else {
        println("Não foi encontrada nenhuma pessoa maior de 18 anos.")
    }
}
```

Neste código, temos uma classe `Pessoa` que representa uma pessoa com um nome e uma idade. A função `main` é o ponto de entrada do programa. Nela, criamos uma lista de `Pessoa` chamada `pessoas`.

Em seguida, usamos um loop `for` para gerar 10 pessoas aleatórias, com nomes e idades gerados aleatoriamente usando as funções `gerarNomeAleatorio` e `gerarIdadeAleatoria`. Cada pessoa é adicionada à lista `pessoas`.

Depois disso, filtramos as pessoas maiores de 18 anos usando a função `filtrarPessoasMaioresDe18` e armazenamos o resultado em uma nova lista chamada `pessoasMaioresDe18`.

Em seguida, encontramos a pessoa mais velha na lista `pessoasMaioresDe18` usando a função `encontrarPessoaMaisVelha`.

Finalmente, chamamos as funções `imprimirDadosPessoas` para imprimir os dados de todas as pessoas e `imprimirPessoaMaisVelha` para imprimir os dados da pessoa mais velha (se existir).

Espero que este exemplo atenda às suas expectativas! Lembre-se de que você sempre pode personalizar e expandir o código conforme necessário.