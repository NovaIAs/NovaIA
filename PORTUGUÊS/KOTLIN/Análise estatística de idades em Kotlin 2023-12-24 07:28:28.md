Claro! Aqui está um exemplo de código em Kotlin que utiliza vários conceitos avançados e é bastante complexo:

```kotlin
import kotlin.math.pow

fun main() {
    val listaPessoas = listOf(
        Pessoa("João", 25),
        Pessoa("Maria", 30),
        Pessoa("Pedro", 28),
        Pessoa("Ana", 26),
        Pessoa("Lucas", 32)
    )

    val mediaIdades = listaPessoas
        .map { it.idade }
        .average()

    val desvioPadrao = listaPessoas
        .map { (it.idade - mediaIdades).pow(2) }
        .sum()
        .div(listaPessoas.size)
        .let { Math.sqrt(it) }

    val pessoaMaisNova = listaPessoas.minByOrNull { it.idade }
    val pessoaMaisVelha = listaPessoas.maxByOrNull { it.idade }

    println("Média das idades: $mediaIdades")
    println("Desvio padrão das idades: $desvioPadrao")
    println("Pessoa mais nova: ${pessoaMaisNova?.nome}")
    println("Pessoa mais velha: ${pessoaMaisVelha?.nome}")
}

data class Pessoa(val nome: String, val idade: Int)
```

Neste código, temos uma lista de pessoas representadas pela classe `Pessoa`, contendo nome e idade. A lista é inicializada com 5 pessoas.

Em seguida, utilizamos algumas operações de manipulação de coleções para calcular a média das idades, o desvio padrão das idades, e encontrar a pessoa mais nova e mais velha da lista.

A função `average()` é utilizada para calcular a média das idades, enquanto a função `sum()` é utilizada para somar os quadrados das diferenças entre cada idade e a média. Em seguida, dividimos o resultado pelo tamanho da lista e aplicamos a função `sqrt()` para calcular a raiz quadrada, obtendo assim o desvio padrão.

Finalmente, imprimimos os resultados na tela.

Espero que este código atenda às suas expectativas de complexidade!