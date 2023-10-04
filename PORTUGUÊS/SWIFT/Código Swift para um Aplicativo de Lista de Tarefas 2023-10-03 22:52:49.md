Certamente! Aqui está um código complexo em Swift que usa uma combinação de estruturas de controle, funções e classes para criar um aplicativo de lista de tarefas:

```swift
import Foundation

// Definição da classe Item para representar cada tarefa
class Item {
    var nome: String
    var concluido: Bool
    
    init(nome: String) {
        self.nome = nome
        self.concluido = false
    }
}

// Definição da classe ListaDeTarefas para gerenciar a lista de tarefas
class ListaDeTarefas {
    var tarefas: [Item]
    
    init() {
        self.tarefas = []
    }
    
    func adicionarTarefa(nome: String) {
        let novaTarefa = Item(nome: nome)
        tarefas.append(novaTarefa)
    }
    
    func marcarTarefaConcluida(indice: Int) {
        if indice >= 0 && indice < tarefas.count {
            tarefas[indice].concluido = true
        }
    }
    
    func removerTarefa(indice: Int) {
        if indice >= 0 && indice < tarefas.count {
            tarefas.remove(at: indice)
        }
    }
    
    func exibirTarefas() {
        if tarefas.isEmpty {
            print("A lista de tarefas está vazia.")
        } else {
            for (indice, tarefa) in tarefas.enumerated() {
                let status = tarefa.concluido ? "[X]" : "[ ]"
                print("\(indice + 1). \(status) \(tarefa.nome)")
            }
        }
    }
}

// Função principal
func main() {
    let lista = ListaDeTarefas()
    
    lista.exibirTarefas()
    
    lista.adicionarTarefa(nome: "Estudar Swift")
    lista.adicionarTarefa(nome: "Fazer exercícios")
    lista.adicionarTarefa(nome: "Ler um livro")
    
    lista.exibirTarefas()
    
    lista.marcarTarefaConcluida(indice: 0)
    
    lista.exibirTarefas()
    
    lista.removerTarefa(indice: 1)
    
    lista.exibirTarefas()
}

main()
```

Neste código, temos duas classes principais: `Item` representa cada tarefa com um nome e um status de conclusão, e `ListaDeTarefas` gerencia a lista de tarefas, permitindo adicionar, marcar como concluídas ou remover tarefas, bem como exibir a lista atualizada.

A função `main` é onde a execução começa. Aqui, criamos uma instância da classe `ListaDeTarefas`, adicionamos algumas tarefas, marcamos a primeira como concluída, removemos a segunda e, finalmente, exibimos a lista atualizada após cada operação.

Isso é apenas um exemplo, você pode expandir e personalizar esse código de acordo com suas necessidades, adicionando mais funcionalidades ou interfaces de usuário.