Claro! Aqui está um código complexo em Ruby que utiliza conceitos avançados da linguagem para criar uma aplicação de gerenciamento de tarefas:

```ruby
class Tarefa
  attr_accessor :titulo, :descricao, :data_limite, :concluida

  def initialize(titulo, descricao, data_limite)
    @titulo = titulo
    @descricao = descricao
    @data_limite = data_limite
    @concluida = false
  end

  def to_s
    status = @concluida ? "Concluída" : "Pendente"
    "#{@titulo}\nDescrição: #{@descricao}\nData limite: #{@data_limite}\nStatus: #{status}\n\n"
  end
end

class ListaTarefas
  def initialize
    @tarefas = []
  end

  def adicionar_tarefa(tarefa)
    @tarefas << tarefa
  end

  def listar_tarefas
    @tarefas.each do |tarefa|
      puts tarefa.to_s
    end
  end

  def concluir_tarefa(titulo)
    @tarefas.each do |tarefa|
      if tarefa.titulo == titulo
        tarefa.concluida = true
        puts "Tarefa '#{titulo}' concluída!"
        return
      end
    end
    puts "Tarefa '#{titulo}' não encontrada."
  end
end

def menu
  puts "===== Gerenciador de Tarefas =====\n\n"
  puts "Selecione uma opção:"
  puts "1. Adicionar tarefa"
  puts "2. Listar tarefas"
  puts "3. Concluir tarefa"
  puts "4. Sair"
  print "\nOpção: "
end

lista_tarefas = ListaTarefas.new

loop do
  menu
  opcao = gets.chomp.to_i
  puts "\n"

  case opcao
  when 1
    print "Título da tarefa: "
    titulo = gets.chomp
    print "Descrição da tarefa: "
    descricao = gets.chomp
    print "Data limite (dd/mm/aaaa): "
    data_limite = gets.chomp
    tarefa = Tarefa.new(titulo, descricao, data_limite)
    lista_tarefas.adicionar_tarefa(tarefa)
    puts "Tarefa adicionada!\n\n"
  when 2
    puts "===== Lista de Tarefas =====\n\n"
    lista_tarefas.listar_tarefas
  when 3
    print "Título da tarefa a ser concluída: "
    titulo = gets.chomp
    lista_tarefas.concluir_tarefa(titulo)
  when 4
    puts "Saindo..."
    break
  else
    puts "Opção inválida. Tente novamente.\n\n"
  end
end
```

Nesse código, temos duas classes principais: `Tarefa` e `ListaTarefas`. A classe `Tarefa` representa uma tarefa individual, com atributos como título, descrição, data limite e o status de conclusão. A classe `ListaTarefas` é responsável por gerenciar um conjunto de tarefas, permitindo adicionar novas tarefas, listar todas as tarefas existentes e concluir uma tarefa específica.

No método `menu`, é exibido um menu com opções para o usuário interagir com o sistema. O código principal utiliza um loop `loop do` para repetir o menu até que o usuário escolha a opção de sair.

Cada opção do menu executa a ação correspondente. Por exemplo, a opção 1 permite ao usuário adicionar uma nova tarefa, solicitando informações como título, descrição e data limite. A opção 2 lista todas as tarefas existentes, mostrando seus detalhes. A opção 3 permite ao usuário marcar uma tarefa como concluída, informando o título da tarefa desejada.

Espero que este código atenda às suas expectativas!