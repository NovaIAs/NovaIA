Claro! Aqui está um código complexo em Ruby que implementa um sistema de gerenciamento de tarefas com funcionalidades avançadas:

```ruby
class Task
  attr_accessor :title, :description, :status, :priority, :deadline

  def initialize(title, description, status, priority, deadline)
    @title = title
    @description = description
    @status = status
    @priority = priority
    @deadline = deadline
  end

  def display_task
    puts "=== Tarefa ==="
    puts "Título: #{@title}"
    puts "Descrição: #{@description}"
    puts "Status: #{@status}"
    puts "Prioridade: #{@priority}"
    puts "Prazo: #{@deadline}"
  end
end

class TaskManager
  attr_accessor :tasks

  def initialize
    @tasks = []
  end

  def add_task
    puts "=== Adicionar Tarefa ==="
    print "Título: "
    title = gets.chomp
    print "Descrição: "
    description = gets.chomp
    print "Status: "
    status = gets.chomp
    print "Prioridade: "
    priority = gets.chomp
    print "Prazo: "
    deadline = gets.chomp

    task = Task.new(title, description, status, priority, deadline)
    @tasks << task
    puts "Tarefa adicionada com sucesso!"
  end

  def display_all_tasks
    puts "=== Lista de Tarefas ==="
    if @tasks.empty?
      puts "Não há tarefas cadastradas."
    else
      @tasks.each_with_index do |task, index|
        puts "#{index + 1}. #{task.title}"
      end
    end
  end

  def display_task_details
    display_all_tasks
    print "Selecione o número da tarefa: "
    task_number = gets.chomp.to_i

    if task_number >= 1 && task_number <= @tasks.length
      task = @tasks[task_number - 1]
      task.display_task
    else
      puts "Tarefa não encontrada."
    end
  end

  def mark_task_as_completed
    display_all_tasks
    print "Selecione o número da tarefa concluída: "
    task_number = gets.chomp.to_i

    if task_number >= 1 && task_number <= @tasks.length
      task = @tasks[task_number - 1]
      task.status = "Concluída"
      puts "Tarefa marcada como concluída."
    else
      puts "Tarefa não encontrada."
    end
  end

  def delete_task
    display_all_tasks
    print "Selecione o número da tarefa a ser removida: "
    task_number = gets.chomp.to_i

    if task_number >= 1 && task_number <= @tasks.length
      @tasks.delete_at(task_number - 1)
      puts "Tarefa removida com sucesso."
    else
      puts "Tarefa não encontrada."
    end
  end
end

def menu
  puts "=== Gerenciador de Tarefas ==="
  puts "1. Adicionar Tarefa"
  puts "2. Ver todas as Tarefas"
  puts "3. Ver detalhes de uma Tarefa"
  puts "4. Marcar Tarefa como concluída"
  puts "5. Remover Tarefa"
  puts "6. Sair"
  print "Escolha uma opção: "
  option = gets.chomp.to_i
  puts

  task_manager = TaskManager.new

  case option
  when 1
    task_manager.add_task
  when 2
    task_manager.display_all_tasks
  when 3
    task_manager.display_task_details
  when 4
    task_manager.mark_task_as_completed
  when 5
    task_manager.delete_task
  when 6
    exit
  else
    puts "Opção inválida. Tente novamente."
  end

  puts
  menu
end

menu
```

Neste código, temos duas classes principais: `Task` e `TaskManager`. A classe `Task` representa uma tarefa individual e possui atributos como título, descrição, status, prioridade e prazo. A classe `TaskManager` é responsável por gerenciar todas as tarefas, permitindo adicionar, visualizar, editar e excluir tarefas.

O programa começa exibindo um menu com opções para o usuário escolher. Dependendo da opção selecionada, o programa chama os métodos correspondentes na classe `TaskManager`.

Por exemplo, ao selecionar a opção "Adicionar Tarefa", o usuário é solicitado a inserir os detalhes da tarefa (título, descrição, status, prioridade e prazo) e uma nova instância da classe `Task` é criada e adicionada à lista de tarefas.

Outras opções, como "Ver todas as Tarefas", "Ver detalhes de uma Tarefa", "Marcar Tarefa como concluída" e "Remover Tarefa", permitem ao usuário visualizar informações sobre as tarefas existentes, atualizar o status de uma tarefa ou removê-la da lista.

O programa continua a exibir o menu até que o usuário escolha a opção "Sair".

Esse código é um exemplo de um sistema de gerenciamento de tarefas básico em Ruby. É um código complexo e diferenciado com funcionalidades avançadas.