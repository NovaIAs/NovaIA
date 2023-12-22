Claro! Aqui está um código complexo em Kotlin que utiliza conceitos avançados de programação, como classes, herança, interfaces e funções de extensão. O código é um sistema de gerenciamento de tarefas, onde é possível criar, editar, excluir e visualizar tarefas. 

```kotlin
import java.time.LocalDate

// Classe base para representar uma tarefa
open class Tarefa(val titulo: String, val dataLimite: LocalDate) {
    var concluida: Boolean = false

    open fun exibirDetalhes() {
        println("Título: $titulo")
        println("Data Limite: $dataLimite")
        println("Concluída: ${if (concluida) "Sim" else "Não"}")
    }
}

// Classe que representa uma tarefa com prioridade
class TarefaComPrioridade(titulo: String, dataLimite: LocalDate, val prioridade: Int) : Tarefa(titulo, dataLimite) {
    override fun exibirDetalhes() {
        super.exibirDetalhes()
        println("Prioridade: $prioridade")
    }
}

// Interface para definir as operações de gerenciamento de tarefas
interface GerenciadorTarefas {
    fun criarTarefa(titulo: String, dataLimite: LocalDate)
    fun editarTarefa(tituloAntigo: String, novoTitulo: String, novaDataLimite: LocalDate)
    fun excluirTarefa(titulo: String)
    fun visualizarTarefas()
}

// Implementação do gerenciador de tarefas
class GerenciadorTarefasImpl : GerenciadorTarefas {
    private val tarefas: MutableList<Tarefa> = mutableListOf()

    override fun criarTarefa(titulo: String, dataLimite: LocalDate) {
        val novaTarefa = Tarefa(titulo, dataLimite)
        tarefas.add(novaTarefa)
        println("Tarefa criada com sucesso!")
    }

    override fun editarTarefa(tituloAntigo: String, novoTitulo: String, novaDataLimite: LocalDate) {
        val tarefa = tarefas.find { it.titulo == tituloAntigo }
        if (tarefa != null) {
            tarefa.titulo = novoTitulo
            tarefa.dataLimite = novaDataLimite
            println("Tarefa editada com sucesso!")
        } else {
            println("Tarefa não encontrada!")
        }
    }

    override fun excluirTarefa(titulo: String) {
        val tarefa = tarefas.find { it.titulo == titulo }
        if (tarefa != null) {
            tarefas.remove(tarefa)
            println("Tarefa excluída com sucesso!")
        } else {
            println("Tarefa não encontrada!")
        }
    }

    override fun visualizarTarefas() {
        for (tarefa in tarefas) {
            tarefa.exibirDetalhes()
            println("-----------------")
        }
    }
}

// Função de extensão para facilitar a criação de tarefas com prioridade
fun GerenciadorTarefas.criarTarefaComPrioridade(titulo: String, dataLimite: LocalDate, prioridade: Int) {
    val novaTarefa = TarefaComPrioridade(titulo, dataLimite, prioridade)
    tarefas.add(novaTarefa)
    println("Tarefa com prioridade criada com sucesso!")
}

fun main() {
    val gerenciadorTarefas: GerenciadorTarefas = GerenciadorTarefasImpl()

    gerenciadorTarefas.criarTarefa("Comprar leite", LocalDate.now().plusDays(1))
    gerenciadorTarefas.criarTarefaComPrioridade("Estudar Kotlin", LocalDate.now().plusWeeks(2), 5)
    gerenciadorTarefas.visualizarTarefas()

    gerenciadorTarefas.editarTarefa("Comprar leite", "Comprar pão", LocalDate.now().plusDays(2))
    gerenciadorTarefas.visualizarTarefas()

    gerenciadorTarefas.excluirTarefa("Estudar Kotlin")
    gerenciadorTarefas.visualizarTarefas()
}
```

Neste código, temos a classe `Tarefa`, que representa uma tarefa básica com um título, uma data limite e um status de conclusão. Temos também a classe `TarefaComPrioridade`, que herda da classe `Tarefa` e adiciona um campo de prioridade. Ambas as classes possuem um método `exibirDetalhes()` para mostrar as informações da tarefa.

A interface `GerenciadorTarefas` define as operações básicas de gerenciamento de tarefas, como criar, editar, excluir e visualizar. A classe `GerenciadorTarefasImpl` implementa essa interface e mantém uma lista de tarefas. Ela também tem uma função de extensão `criarTarefaComPrioridade()` para facilitar a criação de tarefas com prioridade.

No `main()`, criamos uma instância do `GerenciadorTarefasImpl` e usamos seus métodos para criar algumas tarefas, editar uma tarefa existente, excluir uma tarefa e visualizar todas as tarefas.

Esse código é apenas um exemplo complexo de sistema de gerenciamento de tarefas em Kotlin. Pode ser adaptado e expandido de várias maneiras, dependendo dos requisitos do projeto.