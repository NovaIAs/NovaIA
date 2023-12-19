Claro! Aqui está um exemplo de um código complexo em Kotlin que envolve vários conceitos e funcionalidades:

```kotlin
import java.util.*

fun main() {
    val scanner = Scanner(System.`in`)
    val listaNomes = mutableListOf<String>()
    
    println("Bem-vindo ao programa de cadastro de nomes!")
    
    var opcao: Int
    do {
        println("\nSelecione uma opção:")
        println("1. Cadastrar nome")
        println("2. Listar nomes cadastrados")
        println("3. Remover nome")
        println("4. Sair")
        opcao = scanner.nextInt()
        
        when (opcao) {
            1 -> cadastrarNome(listaNomes)
            2 -> listarNomes(listaNomes)
            3 -> removerNome(listaNomes)
            4 -> println("Encerrando o programa...")
            else -> println("Opção inválida!")
        }
    } while (opcao != 4)
}

fun cadastrarNome(listaNomes: MutableList<String>) {
    val scanner = Scanner(System.`in`)
    println("Digite o nome que deseja cadastrar:")
    val nome = scanner.nextLine()
    listaNomes.add(nome)
    println("Nome cadastrado com sucesso!")
}

fun listarNomes(listaNomes: MutableList<String>) {
    if (listaNomes.isEmpty()) {
        println("Nenhum nome cadastrado.")
    } else {
        println("Nomes cadastrados:")
        for (nome in listaNomes) {
            println(nome)
        }
    }
}

fun removerNome(listaNomes: MutableList<String>) {
    val scanner = Scanner(System.`in`)
    println("Digite o nome que deseja remover:")
    val nome = scanner.nextLine()
    val removido = listaNomes.remove(nome)
    if (removido) {
        println("Nome removido com sucesso!")
    } else {
        println("Nome não encontrado.")
    }
}
```

Neste código, criamos um programa de cadastro de nomes em Kotlin. Através de um loop principal `do-while`, o usuário pode selecionar uma das opções disponíveis: cadastrar um nome, listar os nomes cadastrados, remover um nome ou sair do programa.

A função `cadastrarNome` permite que o usuário insira um nome que será adicionado a uma lista de nomes. A função `listarNomes` verifica se a lista está vazia e imprime os nomes cadastrados. A função `removerNome` solicita ao usuário o nome que deseja remover e verifica se o nome existe na lista para removê-lo.

O código utiliza a classe `Scanner` para obter a entrada do usuário e a função `main` é o ponto de entrada do programa.