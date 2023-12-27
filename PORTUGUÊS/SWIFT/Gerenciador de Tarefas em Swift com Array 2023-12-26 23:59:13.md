Certamente! Aqui está um exemplo de um código complexo em Swift, que utiliza várias funcionalidades da linguagem para criar uma aplicação de gerenciamento de tarefas:

```swift
import UIKit

class Tarefa {
    var descricao: String
    var concluida: Bool
    
    init(descricao: String) {
        self.descricao = descricao
        self.concluida = false
    }
}

class ListaTarefas {
    var tarefas: [Tarefa] = []
    
    func adicionarTarefa(descricao: String) {
        let tarefa = Tarefa(descricao: descricao)
        tarefas.append(tarefa)
    }
    
    func removerTarefa(at index: Int) {
        if index >= 0 && index < tarefas.count {
            tarefas.remove(at: index)
        }
    }
    
    func concluirTarefa(at index: Int) {
        if index >= 0 && index < tarefas.count {
            tarefas[index].concluida = true
        }
    }
    
    func exibirTarefas() {
        for (index, tarefa) in tarefas.enumerated() {
            let status = tarefa.concluida ? "Concluída" : "Pendente"
            print("\(index + 1). \(tarefa.descricao) - Status: \(status)")
        }
    }
}

// Exemplo de uso da aplicação

let lista = ListaTarefas()

lista.adicionarTarefa(descricao: "Comprar leite")
lista.adicionarTarefa(descricao: "Pagar contas")
lista.adicionarTarefa(descricao: "Estudar Swift")

lista.exibirTarefas()

lista.concluirTarefa(at: 0)

print("\nLista de tarefas após concluir a primeira tarefa:\n")

lista.exibirTarefas()

lista.removerTarefa(at: 1)

print("\nLista de tarefas após remover a segunda tarefa:\n")

lista.exibirTarefas()
```

Nesse código, criamos duas classes: `Tarefa` e `ListaTarefas`. A classe `Tarefa` representa uma tarefa individual, com propriedades como `descricao` (a descrição da tarefa) e `concluida` (um booleano que indica se a tarefa foi concluída).

A classe `ListaTarefas` é responsável por gerenciar um conjunto de tarefas. Ela possui um array de tarefas, onde podemos adicionar, remover e concluir tarefas. Além disso, também temos um método `exibirTarefas` que mostra todas as tarefas da lista, juntamente com seu status (concluída ou pendente).

No exemplo de uso da aplicação, criamos uma instância da classe `ListaTarefas`, adicionamos algumas tarefas, exibimos a lista, concluímos a primeira tarefa, exibimos a lista novamente e, por fim, removemos a segunda tarefa e exibimos a lista mais uma vez.

Esse código utiliza conceitos como classes, propriedades, inicializadores, métodos, arrays e iteração com `for...in`.