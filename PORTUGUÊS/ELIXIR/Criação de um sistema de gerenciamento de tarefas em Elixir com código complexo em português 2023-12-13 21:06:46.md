Aqui está um exemplo de um código complexo em Elixir que envolve a criação de um sistema de gerenciamento de tarefas. O código foi escrito em português e possui uma estrutura diferenciada:

```elixir
defmodule GerenciadorTarefas do
  defstruct nome: "", descricao: "", status: :pendente, data_criacao: DateTime.utc_now()

  def adicionar_tarefa(tarefa, lista_tarefas \\ []) do
    nova_lista = lista_tarefas ++ [tarefa]
    IO.puts "Tarefa adicionada com sucesso!"
    nova_lista
  end

  def listar_tarefas(lista_tarefas) do
    Enum.each(lista_tarefas, fn tarefa ->
      IO.puts "Nome: #{tarefa.nome}"
      IO.puts "Descrição: #{tarefa.descricao}"
      IO.puts "Status: #{tarefa.status}"
      IO.puts "Data de criação: #{DateTime.to_iso8601(tarefa.data_criacao)}"
      IO.puts "-----------------------"
    end)
  end

  def alterar_status_tarefa(nome, novo_status, lista_tarefas) do
    Enum.map(lista_tarefas, fn tarefa ->
      if tarefa.nome == nome do
        nova_tarefa = %GerenciadorTarefas{tarefa | status: novo_status}
        IO.puts "Status da tarefa '#{nome}' alterado para '#{novo_status}'"
        nova_tarefa
      else
        tarefa
      end
    end)
  end

  def remover_tarefa(nome, lista_tarefas) do
    Enum.filter(lista_tarefas, fn tarefa ->
      tarefa.nome != nome
    end)
  end
end

# Exemplo de uso do GerenciadorTarefas

tarefa1 = %GerenciadorTarefas{nome: "Fazer compras", descricao: "Comprar alimentos para a semana"}
tarefa2 = %GerenciadorTarefas{nome: "Estudar Elixir", descricao: "Estudar os conceitos básicos do Elixir"}

lista_tarefas = GerenciadorTarefas.adicionar_tarefa(tarefa1)
lista_tarefas = GerenciadorTarefas.adicionar_tarefa(tarefa2)

GerenciadorTarefas.listar_tarefas(lista_tarefas)

lista_tarefas = GerenciadorTarefas.alterar_status_tarefa("Fazer compras", :concluida, lista_tarefas)

GerenciadorTarefas.listar_tarefas(lista_tarefas)

lista_tarefas = GerenciadorTarefas.remover_tarefa("Estudar Elixir", lista_tarefas)

GerenciadorTarefas.listar_tarefas(lista_tarefas)
```

Neste código, foi criado um módulo chamado "GerenciadorTarefas" que possui funções para adicionar, listar, alterar status e remover tarefas. Foi utilizado um struct para representar uma tarefa, com campos como nome, descrição, status e data de criação.

A função `adicionar_tarefa` permite adicionar uma nova tarefa à lista de tarefas fornecida como argumento. A função `listar_tarefas` percorre a lista de tarefas e imprime as informações de cada uma delas.

A função `alterar_status_tarefa` recebe o nome de uma tarefa, um novo status e a lista de tarefas. Ela percorre a lista e, caso encontre a tarefa com o nome informado, cria uma nova tarefa com o status atualizado. A função `remover_tarefa` remove uma tarefa da lista, filtrando as tarefas com um nome diferente do fornecido.

No exemplo de uso, são criadas duas tarefas e adicionadas à lista de tarefas. Em seguida, a lista é exibida. Depois, é alterado o status da primeira tarefa para "concluída" e a lista é exibida novamente. Por fim, a segunda tarefa é removida e a lista é exibida mais uma vez.