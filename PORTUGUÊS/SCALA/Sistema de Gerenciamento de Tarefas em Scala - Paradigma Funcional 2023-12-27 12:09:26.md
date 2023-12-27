Claro! Aqui está um código complexo em Scala que cria um sistema de gerenciamento de tarefas utilizando o paradigma de programação funcional.

```scala
import scala.collection.mutable.ArrayBuffer

case class Tarefa(id: Int, descricao: String, concluida: Boolean)

object GerenciadorTarefas {
  private val tarefas: ArrayBuffer[Tarefa] = ArrayBuffer[Tarefa]()

  def adicionarTarefa(descricao: String): Unit = {
    val id = gerarIdUnico()
    val tarefa = Tarefa(id, descricao, concluida = false)
    tarefas += tarefa
    println(s"Tarefa $id adicionada com sucesso!")
  }

  def concluirTarefa(id: Int): Unit = {
    tarefas.find(_.id == id) match {
      case Some(tarefa) =>
        val tarefaConcluida = tarefa.copy(concluida = true)
        tarefas -= tarefa
        tarefas += tarefaConcluida
        println(s"Tarefa $id concluída com sucesso!")
      case None =>
        println(s"Tarefa com id $id não encontrada.")
    }
  }

  def exibirTarefas(): Unit = {
    if (tarefas.isEmpty) {
      println("Nenhuma tarefa encontrada.")
    } else {
      println("Lista de tarefas:")
      tarefas.foreach(tarefa => println(s"${tarefa.id} - ${tarefa.descricao} [${if (tarefa.concluida) "Concluída" else "Pendente"}]"))
    }
  }

  private def gerarIdUnico(): Int = {
    if (tarefas.isEmpty) 1
    else tarefas.map(_.id).max + 1
  }
}

object Main extends App {
  GerenciadorTarefas.adicionarTarefa("Fazer compras")
  GerenciadorTarefas.adicionarTarefa("Estudar para a prova")
  GerenciadorTarefas.adicionarTarefa("Limpar a casa")
  GerenciadorTarefas.exibirTarefas()
  GerenciadorTarefas.concluirTarefa(2)
  GerenciadorTarefas.exibirTarefas()
}
```

Neste código, temos a definição da classe `Tarefa` que representa uma tarefa com atributos como `id`, `descricao` e `concluida`. Em seguida, temos o objeto `GerenciadorTarefas`, que é responsável por gerenciar as tarefas.

O `GerenciadorTarefas` possui uma lista mutável chamada `tarefas`, que armazena todas as tarefas adicionadas. A função `adicionarTarefa` permite adicionar uma nova tarefa à lista, gerando um ID único automaticamente.

A função `concluirTarefa` recebe o ID de uma tarefa e a marca como concluída, atualizando o estado da tarefa na lista.

A função `exibirTarefas` exibe todas as tarefas presentes na lista, mostrando o ID, descrição e status (concluída ou pendente).

Por fim, temos o objeto `Main` que realiza algumas operações de exemplo, adicionando algumas tarefas, exibindo a lista de tarefas, concluindo uma tarefa e exibindo a lista novamente.

Esse código exemplifica o uso de classes, objetos, funções e estruturas de controle em Scala, além de demonstrar o paradigma de programação funcional.