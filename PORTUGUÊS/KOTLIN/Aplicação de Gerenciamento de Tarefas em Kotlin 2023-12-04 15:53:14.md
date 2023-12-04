Claro! Aqui está um código complexo em Kotlin que utiliza recursos avançados da linguagem para criar uma aplicação de gerenciamento de tarefas.

```kotlin
import java.time.LocalDate

class Tarefa(val descricao: String, val dataLimite: LocalDate)

class GerenciadorTarefas {
    private val tarefas: MutableList<Tarefa> = mutableListOf()

    fun adicionarTarefa(descricao: String, dataLimite: LocalDate) {
        val tarefa = Tarefa(descricao, dataLimite)
        tarefas.add(tarefa)
    }

    fun removerTarefa(descricao: String) {
        val tarefa = tarefas.firstOrNull { it.descricao == descricao }
        tarefa?.let { tarefas.remove(it) }
    }

    fun listarTarefas() {
        println("Tarefas:")
        for (tarefa in tarefas) {
            println("Descrição: ${tarefa.descricao}")
            println("Data Limite: ${tarefa.dataLimite}")
            println("------------------")
        }
    }
}

fun main() {
    val gerenciador = GerenciadorTarefas()

    gerenciador.adicionarTarefa("Estudar Kotlin", LocalDate.of(2022, 9, 30))
    gerenciador.adicionarTarefa("Fazer exercícios", LocalDate.of(2022, 10, 10))
    gerenciador.adicionarTarefa("Preparar apresentação", LocalDate.of(2022, 10, 15))

    gerenciador.listarTarefas()

    gerenciador.removerTarefa("Fazer exercícios")

    gerenciador.listarTarefas()
}
```

Neste código, temos duas classes principais: `Tarefa` e `GerenciadorTarefas`.

A classe `Tarefa` representa uma tarefa a ser realizada, contendo uma descrição e uma data limite. O construtor da classe recebe esses dois parâmetros.

A classe `GerenciadorTarefas` é responsável por gerenciar as tarefas. Ela possui uma lista mutável de tarefas e fornece métodos para adicionar, remover e listar tarefas.

No método `main`, criamos uma instância do `GerenciadorTarefas` e adicionamos algumas tarefas utilizando o método `adicionarTarefa`. Em seguida, chamamos o método `listarTarefas` para exibir todas as tarefas cadastradas.

Depois, removemos uma tarefa utilizando o método `removerTarefa` e chamamos novamente o método `listarTarefas` para verificar as tarefas restantes.

Este código é um exemplo básico de uma aplicação de gerenciamento de tarefas, utilizando recursos como classes, métodos, listas e datas em Kotlin.