Claro! Aqui está um exemplo de código em Kotlin que cria um programa de gerenciamento de tarefas:

```kotlin
import java.time.LocalDate

class Tarefa(val descricao: String, val dataLimite: LocalDate)

class GerenciadorTarefas {
    private val listaTarefas = mutableListOf<Tarefa>()

    fun adicionarTarefa(tarefa: Tarefa) {
        listaTarefas.add(tarefa)
    }

    fun removerTarefa(descricao: String) {
        val tarefa = listaTarefas.find { it.descricao == descricao }
        tarefa?.let { listaTarefas.remove(it) }
    }

    fun exibirTarefas() {
        if (listaTarefas.isEmpty()) {
            println("Não há tarefas cadastradas.")
        } else {
            println("Tarefas:")
            listaTarefas.forEachIndexed { index, tarefa ->
                println("${index + 1}. ${tarefa.descricao} (${tarefa.dataLimite})")
            }
        }
    }
}

fun main() {
    val gerenciador = GerenciadorTarefas()

    // Adicionar algumas tarefas
    gerenciador.adicionarTarefa(Tarefa("Estudar Kotlin", LocalDate.parse("2022-01-31")))
    gerenciador.adicionarTarefa(Tarefa("Fazer exercícios de programação", LocalDate.parse("2022-02-15")))
    gerenciador.adicionarTarefa(Tarefa("Preparar apresentação", LocalDate.parse("2022-02-28")))

    // Exibir as tarefas
    gerenciador.exibirTarefas()

    // Remover uma tarefa
    gerenciador.removerTarefa("Estudar Kotlin")

    // Exibir as tarefas novamente
    gerenciador.exibirTarefas()
}
```

Neste código, temos uma classe `Tarefa` que representa uma tarefa a ser realizada. Ela possui uma descrição e uma data limite. 

Em seguida, temos a classe `GerenciadorTarefas`, que é responsável por armazenar e manipular as tarefas. Ela possui uma lista mutável de tarefas e métodos para adicionar, remover e exibir as tarefas.

No método `main()`, criamos uma instância do `GerenciadorTarefas` e adicionamos algumas tarefas utilizando o método `adicionarTarefa()`. Em seguida, exibimos as tarefas utilizando o método `exibirTarefas()`. Depois, removemos uma tarefa específica utilizando o método `removerTarefa()` e exibimos as tarefas restantes novamente.

Esse código em Kotlin permite que você gerencie suas tarefas de forma simples e organizada. Você pode adicionar, remover e exibir tarefas de acordo com suas necessidades. Espero que isso seja útil para você!