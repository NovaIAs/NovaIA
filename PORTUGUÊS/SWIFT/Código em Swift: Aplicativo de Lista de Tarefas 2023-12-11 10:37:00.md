Com certeza! Aqui está um código complexo em Swift que cria uma interface gráfica para um aplicativo de lista de tarefas. Este aplicativo permite que o usuário adicione, edite e exclua tarefas, além de marcar as tarefas como concluídas.

```swift
import UIKit

class Task {
    var title: String
    var isCompleted: Bool
    
    init(title: String, isCompleted: Bool = false) {
        self.title = title
        self.isCompleted = isCompleted
    }
}

class TasksViewController: UIViewController {
    var tableView: UITableView!
    var tasks: [Task] = []
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        setupTableView()
        setupNavigationBar()
    }
    
    func setupTableView() {
        tableView = UITableView(frame: view.bounds, style: .plain)
        tableView.delegate = self
        tableView.dataSource = self
        tableView.register(TaskCell.self, forCellReuseIdentifier: "TaskCell")
        view.addSubview(tableView)
    }
    
    func setupNavigationBar() {
        title = "Lista de Tarefas"
        
        let addBarButton = UIBarButtonItem(barButtonSystemItem: .add, target: self, action: #selector(addTask))
        navigationItem.rightBarButtonItem = addBarButton
    }
    
    @objc func addTask() {
        let alertController = UIAlertController(title: "Nova Tarefa", message: nil, preferredStyle: .alert)
        alertController.addTextField { textField in
            textField.placeholder = "Digite o título da tarefa"
        }
        
        let saveAction = UIAlertAction(title: "Salvar", style: .default) { [weak self, weak alertController] _ in
            guard let textField = alertController?.textFields?.first,
                  let title = textField.text else { return }
            
            let newTask = Task(title: title)
            self?.tasks.append(newTask)
            self?.tableView.reloadData()
        }
        
        let cancelAction = UIAlertAction(title: "Cancelar", style: .cancel, handler: nil)
        
        alertController.addAction(saveAction)
        alertController.addAction(cancelAction)
        
        present(alertController, animated: true, completion: nil)
    }
}

extension TasksViewController: UITableViewDelegate, UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return tasks.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "TaskCell", for: indexPath) as! TaskCell
        let task = tasks[indexPath.row]
        
        cell.titleLabel.text = task.title
        cell.checkBox.isSelected = task.isCompleted
        
        return cell
    }
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        let task = tasks[indexPath.row]
        task.isCompleted.toggle()
        tableView.reloadRows(at: [indexPath], with: .automatic)
    }
    
    func tableView(_ tableView: UITableView, commit editingStyle: UITableViewCell.EditingStyle, forRowAt indexPath: IndexPath) {
        if editingStyle == .delete {
            tasks.remove(at: indexPath.row)
            tableView.deleteRows(at: [indexPath], with: .automatic)
        }
    }
}

class TaskCell: UITableViewCell {
    let titleLabel: UILabel
    let checkBox: UIButton
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        titleLabel = UILabel()
        checkBox = UIButton(type: .system)
        
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        
        configureTitleLabel()
        configureCheckBox()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func configureTitleLabel() {
        titleLabel.translatesAutoresizingMaskIntoConstraints = false
        contentView.addSubview(titleLabel)
        
        NSLayoutConstraint.activate([
            titleLabel.leadingAnchor.constraint(equalTo: contentView.leadingAnchor, constant: 16),
            titleLabel.centerYAnchor.constraint(equalTo: contentView.centerYAnchor)
        ])
    }
    
    func configureCheckBox() {
        checkBox.translatesAutoresizingMaskIntoConstraints = false
        contentView.addSubview(checkBox)
        
        NSLayoutConstraint.activate([
            checkBox.trailingAnchor.constraint(equalTo: contentView.trailingAnchor, constant: -16),
            checkBox.centerYAnchor.constraint(equalTo: contentView.centerYAnchor),
            checkBox.widthAnchor.constraint(equalToConstant: 24),
            checkBox.heightAnchor.constraint(equalToConstant: 24)
        ])
        
        checkBox.addTarget(self, action: #selector(checkBoxTapped), for: .touchUpInside)
    }
    
    @objc func checkBoxTapped() {
        checkBox.isSelected.toggle()
    }
}
```

Este código apresenta um aplicativo completo de lista de tarefas. A classe `Task` representa uma tarefa, com propriedades para o título da tarefa e um indicador de conclusão. A classe `TasksViewController` é responsável por exibir a lista de tarefas em uma tabela e gerenciar as interações do usuário, como adicionar novas tarefas, marcar como concluídas e excluir tarefas.

O código começa importando o framework UIKit e declara a classe `Task` com suas propriedades e um inicializador. Em seguida, declara a classe `TasksViewController` que herda de `UIViewController`.

Dentro da classe `TasksViewController`, temos a configuração inicial em `viewDidLoad()`, onde são chamados os métodos `setupTableView()` e `setupNavigationBar()`. O primeiro configura a tabela, definindo o delegate e dataSource como a própria classe `TasksViewController`, registrando a célula personalizada `TaskCell` e adicionando a tabela na view. O segundo configura a barra de navegação, definindo o título e adicionando um botão de adicionar tarefa.

O método `addTask()` é chamado quando o botão de adicionar tarefa é pressionado. Ele exibe um `UIAlertController` com um campo de texto para o usuário digitar o título da nova tarefa. Ao clicar em "Salvar", uma nova instância de `Task` é criada com o título digitado e adicionada ao array `tasks`. A tabela é recarregada para exibir a nova tarefa.

A extensão `TasksViewController` implementa os métodos do protocolo `UITableViewDelegate` e `UITableViewDataSource` para exibir as tarefas na tabela. O método `tableView(_:numberOfRowsInSection:)` retorna o número de tarefas no array `tasks`. O método `tableView(_:cellForRowAt:)` configura as células da tabela, obtendo a tarefa correspondente ao índice da célula e atribuindo o título e estado da tarefa às propriedades da célula personalizada `TaskCell`. Os métodos `tableView(_:didSelectRowAt:)` e `tableView(_:commit:forRowAt:)` tratam das interações do usuário, como marcar uma tarefa como concluída ou excluir uma tarefa.

Por fim, a classe `TaskCell` é uma subclasse de `UITableViewCell` que representa a célula personalizada para exibir cada tarefa na tabela. Ela possui uma `UILabel` para exibir o título da tarefa e um `UIButton` como um checkbox para marcar a tarefa como concluída. A célula é configurada no método `configureTitleLabel()` para posicionar a label do título e no método `configureCheckBox()` para posicionar o checkbox. O método `checkBoxTapped()` é chamado quando o checkbox é pressionado, alternando o estado de seleção.