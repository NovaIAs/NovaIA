Claro! Abaixo está um exemplo de um código complexo em Kotlin que usa a biblioteca padrão para criar uma aplicação de lista de tarefas:

```
import java.util.*

data class Tarefa(val id: Int, val descricao: String, var concluida: Boolean)

class ListaDeTarefas {
    private val tarefas = mutableListOf<Tarefa>()
    private var proximoId = 1
    
    fun adicionarTarefa(descricao: String) {
        val novaTarefa = Tarefa(proximoId, descricao, false)
        tarefas.add(novaTarefa)
        proximoId++
        println("Tarefa adicionada: $descricao")
    }
    
    fun concluirTarefa(id: Int) {
        val tarefa = tarefas.find { it.id == id }
        if (tarefa != null) {
            tarefa.concluida = true
            println("Tarefa $id concluída")
        } else {
            println("Tarefa $id não encontrada")
        }
    }
    
    fun removerTarefa(id: Int) {
        val tarefa = tarefas.find { it.id == id }
        if (tarefa != null) {
            tarefas.remove(tarefa)
            println("Tarefa $id removida")
        } else {
            println("Tarefa $id não encontrada")
        }
    }
    
    fun exibirTarefas() {
        println("Lista de Tarefas:")
        if (tarefas.isEmpty()) {
            println("Nenhuma tarefa encontrada")
        } else {
            for (tarefa in tarefas) {
                val status = if (tarefa.concluida) "[Concluída]" else "[Pendente]"
                println("${tarefa.id}. ${tarefa.descricao} $status")
            }
        }
    }
}

fun main() {
    val listaDeTarefas = ListaDeTarefas()
    val scanner = Scanner(System.`in`)
    
    loop@ while (true) {
        println("\nO que deseja fazer?")
        println("1. Adicionar uma tarefa")
        println("2. Concluir uma tarefa")
        println("3. Remover uma tarefa")
        println("4. Exibir todas as tarefas")
        println("0. Sair")
        
        when (scanner.nextInt()) {
            1 -> {
                println("Digite a descrição da tarefa:")
                val descricao = scanner.nextLine()
                listaDeTarefas.adicionarTarefa(descricao)
            }
            2 -> {
                println("Digite o ID da tarefa:")
                val id = scanner.nextInt()
                listaDeTarefas.concluirTarefa(id)
            }
            3 -> {
                println("Digite o ID da tarefa:")
                val id = scanner.nextInt()
                listaDeTarefas.removerTarefa(id)
            }
            4 -> listaDeTarefas.exibirTarefas()
            0 -> break@loop
            else -> println("Opção inválida")
        }
    }
    
    scanner.close()
}
```

Este código cria uma lista de tarefas que permite adicionar, concluir, remover e exibir tarefas. A classe `Tarefa` é um modelo de dados que representa uma tarefa com um ID, descrição e status de conclusão. A classe `ListaDeTarefas` contém uma lista de tarefas e métodos para realizar as operações mencionadas.

No método `main`, é criada uma instância da classe `ListaDeTarefas` e um objeto `Scanner` para receber a entrada do usuário. Em um loop infinito, o programa exibe um menu com opções para o usuário escolher. Dependendo da opção selecionada, o programa chama os métodos correspondentes da classe `ListaDeTarefas`.

O usuário pode adicionar uma nova tarefa digitando sua descrição, concluir uma tarefa fornecendo seu ID, remover uma tarefa especificando seu ID ou exibir todas as tarefas da lista. Ao digitar 0, o programa sai do loop e encerra a execução.

Espero que este exemplo atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais ajuda, estou à disposição.