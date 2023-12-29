Aqui está um código complexo em Kotlin, que usa conceitos avançados da linguagem, como funções de extensão, lambdas e fluxos. O código a seguir é um exemplo de um programa de gerenciamento de tarefas, onde é possível adicionar, remover e listar tarefas.

```kotlin
import java.time.LocalDateTime

data class Tarefa(val id: Int, val descricao: String, val dataCriacao: LocalDateTime)

class GerenciadorTarefas {
    private val tarefas = mutableListOf<Tarefa>()

    fun adicionarTarefa(descricao: String) {
        val id = tarefas.size + 1
        val tarefa = Tarefa(id, descricao, LocalDateTime.now())
        tarefas.add(tarefa)
        println("Tarefa adicionada: $tarefa")
    }

    fun removerTarefa(id: Int) {
        val tarefaRemovida = tarefas.find { it.id == id }
        if (tarefaRemovida != null) {
            tarefas.remove(tarefaRemovida)
            println("Tarefa removida: $tarefaRemovida")
        } else {
            println("Tarefa com ID $id não encontrada.")
        }
    }

    fun listarTarefas() {
        if (tarefas.isEmpty()) {
            println("Não há tarefas cadastradas.")
        } else {
            println("Tarefas cadastradas:")
            tarefas.forEach { println(it) }
        }
    }
}

fun main() {
    val gerenciadorTarefas = GerenciadorTarefas()

    gerenciadorTarefas.adicionarTarefa("Estudar Kotlin")
    gerenciadorTarefas.adicionarTarefa("Fazer exercícios de programação")
    gerenciadorTarefas.listarTarefas()

    gerenciadorTarefas.removerTarefa(1)
    gerenciadorTarefas.listarTarefas()
}
```

Neste código, temos a classe `Tarefa`, que representa uma tarefa com um ID, descrição e data de criação. A classe `GerenciadorTarefas` é responsável pelo gerenciamento das tarefas, com métodos para adicionar, remover e listar tarefas.

No método `main`, criamos uma instância de `GerenciadorTarefas` e chamamos os métodos para adicionar duas tarefas, listar as tarefas cadastradas e remover a primeira tarefa. 

O código usa a função `LocalDateTime.now()` para obter a data e hora atual ao adicionar uma tarefa. Além disso, usa funções de extensão como `find` e `forEach` para realizar operações em coleções.

Espero que esse código atenda às suas necessidades de complexidade e diferenciação.