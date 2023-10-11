Certamente! Aqui está um exemplo de um código complexo em Kotlin que implementa uma aplicação de gerenciamento de tarefas:

```kotlin
import java.util.*

data class Tarefa(val id: Int, val descricao: String, val dataLimite: Date, var concluida: Boolean)

class GerenciadorTarefas {
    private val tarefas = mutableListOf<Tarefa>()

    fun adicionarTarefa(tarefa: Tarefa) {
        tarefas.add(tarefa)
    }

    fun removerTarefa(id: Int) {
        val tarefa = obterTarefaPorId(id)
        if (tarefa != null) {
            tarefas.remove(tarefa)
        }
    }

    fun marcarTarefaComoConcluida(id: Int) {
        val tarefa = obterTarefaPorId(id)
        if (tarefa != null) {
            tarefa.concluida = true
        }
    }

    fun listarTarefas() {
        println("Tarefas:")
        for (tarefa in tarefas) {
            val status = if (tarefa.concluida) "Concluída" else "Pendente"
            println("${tarefa.id}: ${tarefa.descricao} - $status")
        }
    }

    private fun obterTarefaPorId(id: Int): Tarefa? {
        return tarefas.find { it.id == id }
    }
}

fun main() {
    val gerenciador = GerenciadorTarefas()

    val tarefa1 = Tarefa(1, "Estudar para a prova", Date(), false)
    val tarefa2 = Tarefa(2, "Fazer exercícios de matemática", Date(), false)
    val tarefa3 = Tarefa(3, "Enviar relatório ao chefe", Date(), false)

    gerenciador.adicionarTarefa(tarefa1)
    gerenciador.adicionarTarefa(tarefa2)
    gerenciador.adicionarTarefa(tarefa3)

    gerenciador.marcarTarefaComoConcluida(2)

    gerenciador.listarTarefas()

    gerenciador.removerTarefa(3)

    gerenciador.listarTarefas()
}
```

Neste código, temos uma classe `Tarefa` que representa uma tarefa a ser realizada, com atributos como `id`, `descricao`, `dataLimite` e `concluida`. A classe `GerenciadorTarefas` é responsável por adicionar, remover, marcar como concluída e listar as tarefas.

Na função `main`, criamos algumas instâncias da classe `Tarefa` e as adicionamos ao `GerenciadorTarefas`. Em seguida, marcamos a segunda tarefa como concluída e listamos todas as tarefas. Depois, removemos a terceira tarefa e listamos novamente.

Esse código demonstra um exemplo básico de como implementar um sistema de gerenciamento de tarefas. É importante notar que ele pode ser expandido e aprimorado de diversas maneiras, dependendo dos requisitos específicos do projeto.