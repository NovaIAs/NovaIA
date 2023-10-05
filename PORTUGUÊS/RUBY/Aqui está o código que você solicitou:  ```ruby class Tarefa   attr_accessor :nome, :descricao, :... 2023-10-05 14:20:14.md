Claro, vou gerar um código complexo em Ruby para você. Este código consiste em um sistema de gerenciamento de tarefas que permite aos usuários adicionar, visualizar, editar e excluir tarefas. Vamos começar:

```ruby
class Tarefa
  attr_accessor :nome, :descricao, :data

  def initialize(nome, descricao, data)
    @nome = nome
    @descricao = descricao
    @data = data
  end
end

class ListaTarefas
  attr_accessor :tarefas

  def initialize
    @tarefas = []
  end

  def adicionar_tarefa(nome, descricao, data)
    tarefa = Tarefa.new(nome, descricao, data)
    tarefas << tarefa
  end

  def visualizar_tarefas
    if tarefas.empty?
      puts "Não há tarefas cadastradas."
    else
      puts "Lista de tarefas:"
      tarefas.each_with_index do |tarefa, index|
        puts "#{index + 1}) #{tarefa.nome} - #{tarefa.descricao} (Data: #{tarefa.data})"
      end
    end
  end

  def editar_tarefa(indice, nome, descricao, data)
    if indice >= 0 && indice < tarefas.length
      tarefa = tarefas[indice]
      tarefa.nome = nome
      tarefa.descricao = descricao
      tarefa.data = data
      puts "Tarefa atualizada com sucesso!"
    else
      puts "Índice inválido!"
    end
  end

  def excluir_tarefa(indice)
    if indice >= 0 && indice < tarefas.length
      tarefas.delete_at(indice)
      puts "Tarefa excluída com sucesso!"
    else
      puts "Índice inválido!"
    end
  end
end

# Exemplo de utilização do sistema

lista = ListaTarefas.new

lista.visualizar_tarefas

lista.adicionar_tarefa("Comprar mantimentos", "Ir ao mercado e comprar mantimentos para a semana.", "01/01/2022")
lista.adicionar_tarefa("Estudar Ruby", "Dedicar 2 horas de estudo diariamente.", "05/01/2022")

lista.visualizar_tarefas

lista.editar_tarefa(1, "Estudar Ruby", "Dedicar 3 horas de estudo diariamente.", "06/01/2022")

lista.visualizar_tarefas

lista.excluir_tarefa(0)

lista.visualizar_tarefas
```

Neste código, temos duas classes principais: `Tarefa` e `ListaTarefas`. A classe `Tarefa` representa uma tarefa individual e possui os atributos `nome`, `descricao` e `data`. A classe `ListaTarefas` é responsável por gerenciar uma lista de tarefas e possui os métodos `adicionar_tarefa`, `visualizar_tarefas`, `editar_tarefa` e `excluir_tarefa`.

No exemplo de utilização do sistema, criamos uma nova lista de tarefas, exibimos a lista vazia, adicionamos duas tarefas, exibimos a lista novamente, editamos a segunda tarefa, exibimos a lista novamente e, por fim, excluímos a primeira tarefa e exibimos a lista uma última vez.

Espero que este código atenda às suas expectativas. Se tiver alguma dúvida ou desejar alguma modificação, estou à disposição.