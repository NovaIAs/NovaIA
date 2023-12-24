Claro! Aqui está um exemplo de um código complexo em Kotlin, que envolve a criação de uma estrutura de dados personalizada e a implementação de algoritmos de ordenação:

```kotlin
// Definindo a estrutura de dados personalizada
class Pessoa(val nome: String, val idade: Int)

// Função para ordenar uma lista de pessoas por idade usando o algoritmo Bubble Sort
fun bubbleSortPorIdade(pessoas: MutableList<Pessoa>): MutableList<Pessoa> {
    val n = pessoas.size
    for (i in 0 until n - 1) {
        for (j in 0 until n - i - 1) {
            if (pessoas[j].idade > pessoas[j + 1].idade) {
                val temp = pessoas[j]
                pessoas[j] = pessoas[j + 1]
                pessoas[j + 1] = temp
            }
        }
    }
    return pessoas
}

// Função para imprimir a lista de pessoas
fun imprimirPessoas(pessoas: List<Pessoa>) {
    for (pessoa in pessoas) {
        println("Nome: ${pessoa.nome}, Idade: ${pessoa.idade}")
    }
}

fun main() {
    // Criando uma lista de pessoas desordenada
    val pessoas = mutableListOf(
        Pessoa("João", 25),
        Pessoa("Maria", 20),
        Pessoa("Pedro", 18),
        Pessoa("Ana", 30),
        Pessoa("Carlos", 22)
    )

    println("Lista de pessoas desordenada:")
    imprimirPessoas(pessoas)

    // Ordenando a lista de pessoas por idade
    val pessoasOrdenadas = bubbleSortPorIdade(pessoas)

    println("Lista de pessoas ordenada por idade:")
    imprimirPessoas(pessoasOrdenadas)
}
```

Neste exemplo, definimos uma classe `Pessoa` com os atributos `nome` e `idade`. Em seguida, implementamos uma função `bubbleSortPorIdade` que ordena uma lista de pessoas com base na idade usando o algoritmo Bubble Sort.

Na função principal `main`, criamos uma lista de pessoas desordenada e a exibimos. Em seguida, chamamos a função `bubbleSortPorIdade` para ordenar a lista de pessoas por idade e, por fim, exibimos a lista ordenada.

Esse código ilustra um exemplo simples de como criar uma estrutura de dados personalizada e implementar um algoritmo de ordenação em Kotlin. Lembre-se de que existem várias outras maneiras de realizar a mesma tarefa, e a escolha do algoritmo e da estrutura de dados depende do contexto e dos requisitos do projeto.