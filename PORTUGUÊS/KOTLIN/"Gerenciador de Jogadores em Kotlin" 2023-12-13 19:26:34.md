Claro! Aqui está um exemplo de um código complexo em Kotlin:

```kotlin
import java.util.*

class Jogador(val nome: String, val idade: Int)

fun main() {
    val jogadores = mutableListOf<Jogador>() // Lista de jogadores

    // Função para adicionar jogadores à lista
    fun adicionarJogador() {
        println("Digite o nome do jogador:")
        val nome = readLine() ?: ""
        println("Digite a idade do jogador:")
        val idade = readLine()?.toIntOrNull() ?: 0

        jogadores.add(Jogador(nome, idade))
    }

    // Função para exibir a lista de jogadores
    fun exibirJogadores() {
        println("--- Jogadores ---")
        jogadores.forEach { jogador ->
            println("Nome: ${jogador.nome}, Idade: ${jogador.idade}")
        }
        println("-----------------")
    }

    // Função para remover jogador da lista
    fun removerJogador() {
        println("Digite o índice do jogador que deseja remover:")
        val indice = readLine()?.toIntOrNull() ?: -1

        if (indice in 0 until jogadores.size) {
            jogadores.removeAt(indice)
            println("Jogador removido com sucesso!")
        } else {
            println("Índice inválido!")
        }
    }

    // Loop principal do programa
    loop@ while (true) {
        println("1 - Adicionar jogador")
        println("2 - Exibir jogadores")
        println("3 - Remover jogador")
        println("4 - Sair")

        val opcao = readLine()?.toIntOrNull() ?: 0

        when (opcao) {
            1 -> adicionarJogador()
            2 -> exibirJogadores()
            3 -> removerJogador()
            4 -> {
                println("Saindo...")
                break@loop
            }
            else -> println("Opção inválida!")
        }

        println() // Linha em branco para separar as opções
    }
}
```

Neste exemplo, criamos um programa simples em Kotlin que permite ao usuário adicionar jogadores a uma lista, exibir os jogadores adicionados e remover jogadores da lista. 

A classe `Jogador` é utilizada para representar os jogadores, com atributos como nome e idade. A lista de jogadores é armazenada em uma variável `jogadores` do tipo `MutableList<Jogador>`.

Dentro da função `main()`, temos as funções `adicionarJogador()`, `exibirJogadores()` e `removerJogador()`, que são responsáveis por adicionar, exibir e remover jogadores, respectivamente.

No loop principal do programa, apresentamos um menu com opções numeradas. Dependendo da opção escolhida pelo usuário, chamamos a função correspondente. A opção 4 encerra o programa.

Este código utiliza recursos do Kotlin, como nullable types (`?`) e ranges (`in 0 until jogadores.size`). Ele também usa a função `readLine()` para ler entradas do usuário e a função `forEach` para percorrer a lista de jogadores e exibi-los.

Espero que este código atenda às suas expectativas!