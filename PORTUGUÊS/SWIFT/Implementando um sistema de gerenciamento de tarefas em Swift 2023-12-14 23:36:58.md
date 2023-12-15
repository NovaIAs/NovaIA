Certamente! Aqui está um exemplo de código complexo em Swift, que implementa um sistema de gerenciamento de tarefas em um aplicativo de lista de afazeres. O código está em português e está bastante detalhado:

```swift
// Definição da estrutura da tarefa
struct Tarefa {
    var titulo: String
    var descricao: String
    var dataLimite: Date
    var concluida: Bool = false
    
    init(titulo: String, descricao: String, dataLimite: Date) {
        self.titulo = titulo
        self.descricao = descricao
        self.dataLimite = dataLimite
    }
}

// Definição da classe de gerenciamento de tarefas
class GerenciadorTarefas {
    var tarefas: [Tarefa] = []
    
    // Adicionar uma nova tarefa
    func adicionarTarefa(tarefa: Tarefa) {
        tarefas.append(tarefa)
    }
    
    // Remover uma tarefa existente
    func removerTarefa(tarefa: Tarefa) {
        if let index = tarefas.firstIndex(where: { $0.titulo == tarefa.titulo }) {
            tarefas.remove(at: index)
        }
    }
    
    // Marcar uma tarefa como concluída
    func marcarTarefaConcluida(tarefa: Tarefa) {
        if let index = tarefas.firstIndex(where: { $0.titulo == tarefa.titulo }) {
            tarefas[index].concluida = true
        }
    }
    
    // Listar todas as tarefas
    func listarTarefas() {
        for tarefa in tarefas {
            print("Título: \(tarefa.titulo)")
            print("Descrição: \(tarefa.descricao)")
            print("Data Limite: \(tarefa.dataLimite)")
            print("Concluída: \(tarefa.concluida ? "Sim" : "Não")")
            print("----------------------------------")
        }
    }
}

// Uso do sistema de gerenciamento de tarefas
let gerenciador = GerenciadorTarefas()

// Criar algumas tarefas
let dataAtual = Date()
let tarefa1 = Tarefa(titulo: "Comprar mantimentos", descricao: "Ir ao supermercado e comprar comida", dataLimite: dataAtual)
let tarefa2 = Tarefa(titulo: "Estudar programação", descricao: "Revisar conceitos de Swift", dataLimite: dataAtual.addingTimeInterval(86400)) // Adiciona 1 dia

// Adicionar as tarefas ao gerenciador
gerenciador.adicionarTarefa(tarefa: tarefa1)
gerenciador.adicionarTarefa(tarefa: tarefa2)

// Listar as tarefas
gerenciador.listarTarefas()

// Marcar a primeira tarefa como concluída
gerenciador.marcarTarefaConcluida(tarefa: tarefa1)

// Listar as tarefas novamente para verificar a tarefa concluída
gerenciador.listarTarefas()

// Remover a segunda tarefa
gerenciador.removerTarefa(tarefa: tarefa2)

// Listar as tarefas restantes
gerenciador.listarTarefas()
```

Este código implementa um sistema de gerenciamento de tarefas simples. A estrutura `Tarefa` representa uma única tarefa, com propriedades como título, descrição, data limite e um indicador de conclusão. A classe `GerenciadorTarefas` é responsável por adicionar, remover, marcar como concluída e listar as tarefas.

No exemplo acima, duas tarefas são criadas e adicionadas ao gerenciador. Em seguida, as tarefas são listadas, e a primeira tarefa é marcada como concluída. Em seguida, as tarefas são listadas novamente para verificar a tarefa concluída. Por fim, a segunda tarefa é removida do gerenciador e as tarefas restantes são listadas novamente.