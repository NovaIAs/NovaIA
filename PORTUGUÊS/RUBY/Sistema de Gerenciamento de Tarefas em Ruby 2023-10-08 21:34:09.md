Claro, vou criar um código complexo em Ruby para você. O código a seguir é um exemplo de um sistema de gerenciamento de tarefas:

```ruby
class Tarefa
  attr_accessor :titulo, :descricao, :prioridade, :data_limite, :concluida
  
  def initialize(titulo, descricao, prioridade, data_limite)
    @titulo = titulo
    @descricao = descricao
    @prioridade = prioridade
    @data_limite = data_limite
    @concluida = false
  end
  
  def concluir
    @concluida = true
  end
  
  def to_s
    status = @concluida ? "[Concluída]" : "[Em andamento]"
    "#{status} #{@titulo}\nDescrição: #{@descricao}\nPrioridade: #{@prioridade}\nData limite: #{@data_limite}"
  end
end

class GerenciadorTarefas
  def initialize
    @tarefas = []
  end
  
  def adicionar_tarefa(tarefa)
    @tarefas << tarefa
  end
  
  def listar_tarefas
    if @tarefas.empty?
      puts "Não há tarefas cadastradas."
    else
      puts "Lista de Tarefas:"
      @tarefas.each_with_index do |tarefa, index|
        puts "#{index + 1}. #{tarefa}"
      end
    end
  end
  
  def concluir_tarefa(indice)
    if indice < 1 || indice > @tarefas.length
      puts "Tarefa não encontrada."
    else
      tarefa = @tarefas[indice - 1]
      tarefa.concluir
      puts "Tarefa '#{tarefa.titulo}' concluída."
    end
  end
end

gerenciador = GerenciadorTarefas.new

tarefa1 = Tarefa.new("Comprar mantimentos", "Ir ao supermercado e comprar alimentos para a semana", "Alta", "2022-12-31")
tarefa2 = Tarefa.new("Fazer exercícios", "Praticar atividade física por 30 minutos", "Média", "2022-01-15")
tarefa3 = Tarefa.new("Estudar Ruby", "Dedicar 1 hora de estudo sobre Ruby", "Baixa", "2022-02-28")

gerenciador.adicionar_tarefa(tarefa1)
gerenciador.adicionar_tarefa(tarefa2)
gerenciador.adicionar_tarefa(tarefa3)

gerenciador.listar_tarefas

gerenciador.concluir_tarefa(2)

gerenciador.listar_tarefas
```

Neste código, temos duas classes: `Tarefa` e `GerenciadorTarefas`. A classe `Tarefa` representa uma tarefa com atributos como título, descrição, prioridade e data limite. Ela também possui métodos para concluir a tarefa e exibir suas informações.

A classe `GerenciadorTarefas` é responsável por gerenciar as tarefas. Ela possui um array para armazenar as tarefas adicionadas, métodos para adicionar tarefas, listar tarefas e concluir tarefas. 

No exemplo, criamos três objetos `Tarefa` e os adicionamos ao `GerenciadorTarefas`. Em seguida, listamos as tarefas e concluímos a segunda tarefa. Por fim, listamos novamente as tarefas para verificar a alteração.

Espero que esse código complexo em Ruby atenda às suas expectativas!