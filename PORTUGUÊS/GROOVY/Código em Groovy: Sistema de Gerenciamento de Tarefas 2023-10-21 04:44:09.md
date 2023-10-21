Claro! Aqui está um código complexo em Groovy que cria um sistema de gerenciamento de tarefas:

```groovy
import java.time.LocalDate
import java.util.ArrayList

class Tarefa {
    String descricao
    LocalDate dataLimite
    boolean concluida

    Tarefa(String descricao, LocalDate dataLimite) {
        this.descricao = descricao
        this.dataLimite = dataLimite
        this.concluida = false
    }

    String toString() {
        def status = concluida ? "[Concluída]" : "[Pendente]"
        return "${status} ${descricao} - ${dataLimite}"
    }
}

class GerenciadorTarefas {
    ArrayList<Tarefa> tarefas

    GerenciadorTarefas() {
        this.tarefas = new ArrayList<Tarefa>()
    }

    void adicionarTarefa(String descricao, LocalDate dataLimite) {
        Tarefa novaTarefa = new Tarefa(descricao, dataLimite)
        tarefas.add(novaTarefa)
    }

    void concluirTarefa(int indice) {
        if (indice >= 0 && indice < tarefas.size()) {
            Tarefa tarefa = tarefas.get(indice)
            tarefa.concluida = true
        } else {
            println("Índice inválido!")
        }
    }

    void exibirTarefas() {
        println("------ Tarefas ------")
        if (tarefas.isEmpty()) {
            println("Nenhuma tarefa encontrada.")
        } else {
            tarefas.eachWithIndex { tarefa, index ->
                println("${index}. ${tarefa}")
            }
        }
        println("---------------------")
    }
}

// Exemplo de uso do sistema de gerenciamento de tarefas

def gerenciador = new GerenciadorTarefas()

gerenciador.adicionarTarefa("Implementar autenticação no sistema", LocalDate.parse("2022-01-31"))
gerenciador.adicionarTarefa("Atualizar documentação", LocalDate.parse("2022-02-15"))
gerenciador.adicionarTarefa("Testar funcionalidades", LocalDate.parse("2022-02-28"))

gerenciador.exibirTarefas()

gerenciador.concluirTarefa(0)
gerenciador.concluirTarefa(1)

gerenciador.exibirTarefas()
```

Neste código, temos duas classes principais: `Tarefa` e `GerenciadorTarefas`. A classe `Tarefa` representa uma tarefa com uma descrição, uma data limite e um status de conclusão. A classe `GerenciadorTarefas` é responsável por adicionar tarefas à lista, marcar tarefas como concluídas e exibir todas as tarefas.

No exemplo de uso no final do código, criamos um objeto `GerenciadorTarefas`, adicionamos três tarefas e as exibimos. Em seguida, marcamos as duas primeiras tarefas como concluídas e exibimos novamente a lista de tarefas para verificar as alterações.

Esse código demonstra um sistema básico de gerenciamento de tarefas em Groovy, permitindo a adição, conclusão e exibição das tarefas. Você pode expandir esse código adicionando funcionalidades adicionais, como exclusão de tarefas, busca por descrição, entre outras.