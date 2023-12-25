Certamente! Aqui está um exemplo de código complexo em Kotlin:

```kotlin
import java.util.*

// Classe para representar uma pessoa
data class Pessoa(val nome: String, val idade: Int)

// Função para ordenar uma lista de pessoas por idade, do mais novo para o mais velho
fun ordenarPessoasPorIdade(pessoas: List<Pessoa>): List<Pessoa> {
    return pessoas.sortedBy { it.idade }
}

// Função para calcular a média de idade das pessoas em uma lista
fun calcularMediaIdade(pessoas: List<Pessoa>): Double {
    val somaIdades = pessoas.map { it.idade }.sum()
    return somaIdades.toDouble() / pessoas.size
}

// Função para encontrar a pessoa mais velha em uma lista
fun encontrarPessoaMaisVelha(pessoas: List<Pessoa>): Pessoa {
    return pessoas.maxByOrNull { it.idade }!!
}

fun main() {
    val scanner = Scanner(System.`in`)
    val pessoas = mutableListOf<Pessoa>()

    // Solicitar informações das pessoas ao usuário
    println("Digite o número de pessoas:")
    val numeroPessoas = scanner.nextInt()

    for (i in 1..numeroPessoas) {
        println("Digite o nome da pessoa $i:")
        val nome = scanner.next()

        println("Digite a idade da pessoa $i:")
        val idade = scanner.nextInt()

        pessoas.add(Pessoa(nome, idade))
    }

    // Ordenar a lista de pessoas por idade
    val pessoasOrdenadas = ordenarPessoasPorIdade(pessoas)

    // Calcular a média de idade das pessoas
    val mediaIdade = calcularMediaIdade(pessoasOrdenadas)

    // Encontrar a pessoa mais velha
    val pessoaMaisVelha = encontrarPessoaMaisVelha(pessoasOrdenadas)

    // Imprimir os resultados
    println("Pessoas ordenadas por idade:")
    pessoasOrdenadas.forEach { println("${it.nome} - ${it.idade} anos") }

    println("Média de idade: $mediaIdade anos")
    println("Pessoa mais velha: ${pessoaMaisVelha.nome} - ${pessoaMaisVelha.idade} anos")
}
```

Neste código, temos uma classe `Pessoa` que representa uma pessoa com nome e idade. Em seguida, temos três funções:

- `ordenarPessoasPorIdade`: recebe uma lista de pessoas e retorna uma nova lista ordenada por idade.
- `calcularMediaIdade`: recebe uma lista de pessoas e retorna a média de idade.
- `encontrarPessoaMaisVelha`: recebe uma lista de pessoas e retorna a pessoa mais velha.

No `main`, o código solicita ao usuário o número de pessoas a serem inseridas, em seguida, solicita o nome e a idade de cada pessoa. As informações são armazenadas em uma lista de pessoas. 

Depois disso, a lista de pessoas é ordenada por idade, calcula-se a média de idade e encontra-se a pessoa mais velha. Por fim, os resultados são impressos no console.