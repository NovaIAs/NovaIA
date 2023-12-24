Certamente! Aqui está um exemplo de um código complexo em Swift, que envolve o uso de várias classes e recursos avançados. Este código consiste em um aplicativo de lista de tarefas que permite adicionar, marcar como concluído e excluir tarefas. 

```swift
import UIKit

class Tarefa {
    var nome: String
    var concluido: Bool
    
    init(nome: String) {
        self.nome = nome
        self.concluido = false
    }
}

class ListaTarefas {
    var tarefas: [Tarefa]
    
    init() {
        self.tarefas = []
    }
    
    func adicionarTarefa(_ tarefa: Tarefa) {
        tarefas.append(tarefa)
    }
    
    func marcarTarefaConcluida(_ index: Int) {
        guard index >= 0 && index < tarefas.count else {
            return
        }
        tarefas[index].concluido = true
    }
    
    func excluirTarefa(_ index: Int) {
        guard index >= 0 && index < tarefas.count else {
            return
        }
        tarefas.remove(at: index)
    }
    
    func exibirLista() {
        for (index, tarefa) in tarefas.enumerated() {
            let status = tarefa.concluido ? "[Concluído]" : "[Pendente]"
            print("\(index + 1). \(status) \(tarefa.nome)")
        }
    }
}

class ViewController: UIViewController {
    var listaTarefas: ListaTarefas
    
    init(listaTarefas: ListaTarefas) {
        self.listaTarefas = listaTarefas
        super.init(nibName: nil, bundle: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        // Simulando a adição de algumas tarefas
        listaTarefas.adicionarTarefa(Tarefa(nome: "Fazer compras"))
        listaTarefas.adicionarTarefa(Tarefa(nome: "Estudar Swift"))
        listaTarefas.adicionarTarefa(Tarefa(nome: "Ir à academia"))
        
        // Marcar a primeira tarefa como concluída
        listaTarefas.marcarTarefaConcluida(0)
        
        // Exibir a lista de tarefas
        listaTarefas.exibirLista()
        
        // Excluir a segunda tarefa
        listaTarefas.excluirTarefa(1)
        
        // Exibir a lista de tarefas novamente
        listaTarefas.exibirLista()
    }
}

let listaTarefas = ListaTarefas()
let viewController = ViewController(listaTarefas: listaTarefas)
viewController.viewDidLoad()
```

Neste código, temos três classes principais: `Tarefa`, `ListaTarefas` e `ViewController`. 

A classe `Tarefa` representa uma tarefa individual e possui duas propriedades: `nome` para armazenar o nome da tarefa e `concluido` para indicar se a tarefa foi concluída ou não.

A classe `ListaTarefas` é responsável por gerenciar a lista de tarefas. Ela possui um array de `Tarefa` chamado `tarefas` e métodos para adicionar, marcar como concluída, excluir e exibir a lista de tarefas.

A classe `ViewController` é uma subclasse de `UIViewController` e é responsável por exibir a lista de tarefas na interface do aplicativo. Ela recebe uma instância de `ListaTarefas` no inicializador e, no método `viewDidLoad`, simula a adição de algumas tarefas, marca uma tarefa como concluída, exibe a lista de tarefas e exclui uma tarefa.

No final, criamos uma instância de `ListaTarefas`, uma instância de `ViewController` e chamamos o método `viewDidLoad()` para simular a execução do aplicativo.

Espero que este exemplo complexo tenha atendido às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, sinta-se à vontade para perguntar.