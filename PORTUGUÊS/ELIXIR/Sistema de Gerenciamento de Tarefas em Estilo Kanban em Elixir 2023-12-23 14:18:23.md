Claro! Aqui está um exemplo complexo de código em Elixir que implementa um sistema de gerenciamento de tarefas em estilo Kanban:

```elixir
defmodule Kanban do
  @moduledoc """
  Sistema de Gerenciamento de Tarefas em Estilo Kanban
  """

  defstruct [
    backlog: [],
    doing: [],
    done: []
  ]

  @doc """
  Adiciona uma tarefa à lista Backlog
  """
  def add_task(%{backlog: backlog} = board, task) do
    new_backlog = [task | backlog]
    %{board | backlog: new_backlog}
  end

  @doc """
  Move uma tarefa da lista Backlog para Doing
  """
  def move_to_doing(%{backlog: backlog, doing: doing} = board, task) do
    new_backlog = List.delete(backlog, task)
    new_doing = [task | doing]
    %{board | backlog: new_backlog, doing: new_doing}
  end

  @doc """
  Move uma tarefa da lista Doing para Done
  """
  def move_to_done(%{doing: doing, done: done} = board, task) do
    new_doing = List.delete(doing, task)
    new_done = [task | done]
    %{board | doing: new_doing, done: new_done}
  end

  @doc """
  Remove uma tarefa da lista Done
  """
  def remove_task(%{done: done} = board, task) do
    new_done = List.delete(done, task)
    %{board | done: new_done}
  end

  @doc """
  Exibe todas as tarefas em cada lista do quadro
  """
  def show_board(%{backlog: backlog, doing: doing, done: done}) do
    IO.puts "Backlog:"
    Enum.each(backlog, fn task -> IO.puts "- #{task}" end)

    IO.puts "\nDoing:"
    Enum.each(doing, fn task -> IO.puts "- #{task}" end)

    IO.puts "\nDone:"
    Enum.each(done, fn task -> IO.puts "- #{task}" end)
  end
end

# Exemplo de uso
board = %Kanban{}
board = Kanban.add_task(board, "Implementar autenticação de usuários")
board = Kanban.add_task(board, "Corrigir bug no sistema de notificações")
board = Kanban.move_to_doing(board, "Implementar autenticação de usuários")
board = Kanban.move_to_done(board, "Corrigir bug no sistema de notificações")
board = Kanban.remove_task(board, "Corrigir bug no sistema de notificações")
Kanban.show_board(board)
```

Neste exemplo, a estrutura `Kanban` representa um quadro Kanban com três listas: backlog, doing e done. Cada lista é inicializada como uma lista vazia.

As funções `add_task/2`, `move_to_doing/2`, `move_to_done/2` e `remove_task/2` são responsáveis por adicionar, mover e remover tarefas em cada uma das listas. A função `show_board/1` exibe todas as tarefas em cada lista do quadro.

No exemplo de uso, criamos um novo quadro `board` e adicionamos duas tarefas ao backlog. Em seguida, movemos uma tarefa do backlog para a lista doing e, em seguida, para a lista done. Por fim, removemos uma tarefa da lista done e exibimos todas as tarefas em cada lista do quadro.