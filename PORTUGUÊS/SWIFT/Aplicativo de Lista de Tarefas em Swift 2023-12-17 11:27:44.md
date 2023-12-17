Claro! Aqui está um exemplo de um código complexo em Swift que utiliza várias funcionalidades da linguagem. O código é um aplicativo de lista de tarefas (To-Do List) que permite adicionar, excluir e marcar tarefas como concluídas. Vou explicar o código em detalhes abaixo:

```swift
import UIKit

struct Tarefa {
    var nome: String
    var concluida: Bool
    
    init(nome: String) {
        self.nome = nome
        self.concluida = false
    }
}

class ListaTarefasViewController: UIViewController {
    
    var tarefas: [Tarefa] = []
    
    @IBOutlet weak var tableView: UITableView!
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        tableView.delegate = self
        tableView.dataSource = self
    }
    
    @IBAction func adicionarTarefa(_ sender: UIButton) {
        let alert = UIAlertController(title: "Adicionar Tarefa", message: nil, preferredStyle: .alert)
        alert.addTextField { textField in
            textField.placeholder = "Digite o nome da tarefa"
        }
        let addAction = UIAlertAction(title: "Adicionar", style: .default) { _ in
            if let nome = alert.textFields?.first?.text {
                let novaTarefa = Tarefa(nome: nome)
                self.tarefas.append(novaTarefa)
                self.tableView.reloadData()
            }
        }
        let cancelAction = UIAlertAction(title: "Cancelar", style: .cancel, handler: nil)
        
        alert.addAction(addAction)
        alert.addAction(cancelAction)
        
        present(alert, animated: true, completion: nil)
    }
    
    func excluirTarefa(at indexPath: IndexPath) {
        tarefas.remove(at: indexPath.row)
        tableView.deleteRows(at: [indexPath], with: .fade)
    }
    
    func marcarTarefaConcluida(at indexPath: IndexPath) {
        tarefas[indexPath.row].concluida = true
        tableView.reloadRows(at: [indexPath], with: .automatic)
    }
}

extension ListaTarefasViewController: UITableViewDataSource, UITableViewDelegate {
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return tarefas.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "TarefaCell", for: indexPath)
        
        let tarefa = tarefas[indexPath.row]
        cell.textLabel?.text = tarefa.nome
        cell.accessoryType = tarefa.concluida ? .checkmark : .none
        
        return cell
    }
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        let tarefa = tarefas[indexPath.row]
        
        if tarefa.concluida {
            marcarTarefaConcluida(at: indexPath)
        } else {
            let alert = UIAlertController(title: "Tarefa Concluída", message: "Deseja marcar a tarefa como concluída?", preferredStyle: .alert)
            let confirmAction = UIAlertAction(title: "Confirmar", style: .default) { _ in
                self.marcarTarefaConcluida(at: indexPath)
            }
            let cancelAction = UIAlertAction(title: "Cancelar", style: .cancel, handler: nil)
            
            alert.addAction(confirmAction)
            alert.addAction(cancelAction)
            
            present(alert, animated: true, completion: nil)
        }
        
        tableView.deselectRow(at: indexPath, animated: true)
    }
    
    func tableView(_ tableView: UITableView, commit editingStyle: UITableViewCell.EditingStyle, forRowAt indexPath: IndexPath) {
        if editingStyle == .delete {
            excluirTarefa(at: indexPath)
        }
    }
}

class AppDelegate: UIResponder, UIApplicationDelegate {
    var window: UIWindow?
    
    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?) -> Bool {
        return true
    }
}

let appDelegate = AppDelegate()
UIApplicationMain(CommandLine.argc, CommandLine.unsafeArgv, nil, NSStringFromClass(AppDelegate.self))
```

Neste código, começamos importando o framework UIKit para utilizar componentes de interface do usuário. Em seguida, definimos uma estrutura `Tarefa` que representa uma tarefa da lista. Cada tarefa possui um nome e uma propriedade `concluida` que indica se ela foi concluída ou não.

A seguir, temos a classe `ListaTarefasViewController`, que herda de `UIViewController` e é responsável por gerenciar a lista de tarefas. Ela possui uma propriedade `tarefas` que é um array de `Tarefa`, e também uma referência a uma `UITableView` que será utilizada para exibir as tarefas.

Dentro do método `viewDidLoad`, configuramos o `delegate` e o `dataSource` da tabela para a própria instância da classe `ListaTarefasViewController`.

A função `adicionarTarefa` é chamada quando o botão de adicionar tarefa é pressionado. Ela exibe um alerta de texto que permite ao usuário digitar o nome da nova tarefa. Quando o usuário confirma a adição, uma nova instância de `Tarefa` é criada com o nome digitado e adicionada ao array `tarefas`. Em seguida, a tabela é recarregada para exibir a nova tarefa.

As funções `excluirTarefa` e `marcarTarefaConcluida` são utilizadas para excluir e marcar uma tarefa como concluída, respectivamente.

A extensão `ListaTarefasViewController` implementa os métodos do protocolo `UITableViewDataSource` e `UITableViewDelegate` para configurar e manipular a tabela de tarefas. Ela determina o número de linhas na tabela, configura as células com as informações das tarefas e trata os eventos de seleção e exclusão de tarefas.

Por fim, temos a classe `AppDelegate` que implementa o protocolo `UIApplicationDelegate`. Neste exemplo, ela é apenas uma classe vazia, pois não é necessário adicionar nenhuma lógica adicional.

No final do código, criamos uma instância de `AppDelegate` e chamamos a função `UIApplicationMain` para iniciar a aplicação.

Espero que esse código atenda às suas expectativas!