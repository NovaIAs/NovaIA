Claro! Aqui está um exemplo de código complexo em Kotlin:

```kotlin
// Classe para representar uma pessoa
data class Pessoa(val nome: String, val idade: Int)

// Função principal
fun main() {
    val listaPessoas = mutableListOf<Pessoa>()

    // Preenche a lista com dados de pessoas
    preencherListaPessoas(listaPessoas)

    // Filtra pessoas maiores de idade
    val pessoasMaioresIdade = filtrarMaioresIdade(listaPessoas)

    // Ordena as pessoas por idade de forma decrescente
    val pessoasOrdenadas = ordenarPorIdadeDecrescente(pessoasMaioresIdade)

    // Imprime as pessoas ordenadas
    imprimirPessoas(pessoasOrdenadas)
}

// Função para preencher a lista de pessoas com dados fictícios
fun preencherListaPessoas(lista: MutableList<Pessoa>) {
    lista.add(Pessoa("João", 20))
    lista.add(Pessoa("Maria", 18))
    lista.add(Pessoa("Pedro", 25))
    lista.add(Pessoa("Ana", 30))
    lista.add(Pessoa("Lucas", 16))
}

// Função para filtrar pessoas maiores de idade
fun filtrarMaioresIdade(lista: List<Pessoa>): List<Pessoa> {
    return lista.filter { it.idade >= 18 }
}

// Função para ordenar as pessoas por idade de forma decrescente
fun ordenarPorIdadeDecrescente(lista: List<Pessoa>): List<Pessoa> {
    return lista.sortedByDescending { it.idade }
}

// Função para imprimir as pessoas
fun imprimirPessoas(lista: List<Pessoa>) {
    println("Lista de pessoas:")
    lista.forEach { pessoa ->
        println("Nome: ${pessoa.nome}, Idade: ${pessoa.idade}")
    }
}
```

Explicação do código:
1. Começamos pela definição da classe `Pessoa`, que possui as propriedades `nome` e `idade`. Utilizamos a diretiva `data class` para automaticamente gerar métodos como `equals`, `hashCode`, `toString`, entre outros.
2. Na função `main`, criamos uma lista mutável de `Pessoa` chamada `listaPessoas`.
3. A função `preencherListaPessoas` é responsável por preencher a lista com dados fictícios de pessoas.
4. A função `filtrarMaioresIdade` recebe a lista de pessoas e retorna uma nova lista contendo apenas as pessoas maiores de idade (idade maior ou igual a 18).
5. A função `ordenarPorIdadeDecrescente` recebe a lista de pessoas maiores de idade e retorna uma nova lista ordenada por idade de forma decrescente.
6. A função `imprimirPessoas` recebe a lista de pessoas ordenadas e imprime os nomes e idades de cada pessoa.
7. Por fim, na função `main`, chamamos as funções em sequência para preencher a lista, filtrar as pessoas maiores de idade, ordenar por idade decrescente e, por fim, imprimir as pessoas ordenadas.

Este código é apenas um exemplo de como estruturar um programa em Kotlin utilizando classes, funções e manipulação de listas. Claro que em um contexto real, as implementações podem variar de acordo com as necessidades específicas do projeto.