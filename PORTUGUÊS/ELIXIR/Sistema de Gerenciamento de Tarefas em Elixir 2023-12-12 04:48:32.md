Claro! Aqui está um código em Elixir que envolve a criação de um sistema de gerenciamento de tarefas. Ele é bastante complexo e contém um conjunto de módulos interconectados para realizar diferentes funcionalidades. Vou explicar brevemente o que cada um faz:

```elixir
defmodule GerenciadorTarefas do
  def start() do
    TaskSupervisor.start_link(name: __MODULE__)
  end
end
```
O módulo `GerenciadorTarefas` é responsável por iniciar o sistema de gerenciamento de tarefas. Ele inicia um supervisor de tarefas.

```elixir
defmodule Tarefa do
  defstruct nome: "", descricao: "", concluida: false

  def novo(nome, descricao \\ "") do
    %__MODULE__{nome: nome, descricao: descricao}
  end

  def atualizar_descricao(tarefa, nova_descricao) do
    %Tarefa{tarefa | descricao: nova_descricao}
  end

  def marcar_como_concluida(tarefa) do
    %Tarefa{tarefa | concluida: true}
  end
end
```
O módulo `Tarefa` define uma estrutura para representar cada tarefa com os campos `nome`, `descricao` e `concluida`. Ele possui métodos para criar uma nova tarefa, atualizar a descrição da tarefa e marcar a tarefa como concluída.

```elixir
defmodule ListaTarefas do
  defstruct tarefas: []

  def novo() do
    %__MODULE__{}
  end

  def adicionar_tarefa(lista, tarefa) do
    %ListaTarefas{lista | tarefas: [tarefa | lista.tarefas]}
  end

  def remover_tarefa(lista, tarefa) do
    %ListaTarefas{lista | tarefas: List.delete(lista.tarefas, tarefa)}
  end

  def listar_tarefas(lista) do
    lista.tarefas
  end
end
```
O módulo `ListaTarefas` define uma estrutura para representar uma lista de tarefas. Ele possui métodos para adicionar tarefas à lista, remover tarefas da lista e listar todas as tarefas da lista.

```elixir
defmodule TaskSupervisor do
  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def init(nil) do
    children = [
      worker(GerenciadorTarefas, []),
      supervisor(TaskSupervisor.Task, [[]])
    ]

    supervise(children, strategy: :one_for_one)
  end
end
```
O módulo `TaskSupervisor` é um supervisor que gerencia os processos do sistema. Ele inicia um supervisor para o módulo `GerenciadorTarefas` e um supervisor para o módulo `TaskSupervisor.Task`.

```elixir
defmodule TaskSupervisor.Task do
  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(args) do
    children = [
      worker(TaskSupervisor.TaskHandler, [args])
    ]

    supervise(children, strategy: :one_for_all)
  end
end
```
O módulo `TaskSupervisor.Task` é outro supervisor que gerencia os processos relacionados às tarefas. Ele inicia um supervisor para o módulo `TaskSupervisor.TaskHandler`.

```elixir
defmodule TaskSupervisor.TaskHandler do
  use GenServer

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(args) do
    tarefas = ListaTarefas.novo()
    {:ok, tarefas}
  end

  def handle_call({:adicionar_tarefa, tarefa}, _from, tarefas) do
    nova_lista = ListaTarefas.adicionar_tarefa(tarefas, tarefa)
    {:reply, :ok, nova_lista}
  end

  def handle_call({:remover_tarefa, tarefa}, _from, tarefas) do
    nova_lista = ListaTarefas.remover_tarefa(tarefas, tarefa)
    {:reply, :ok, nova_lista}
  end

  def handle_call(:listar_tarefas, _from, tarefas) do
    {:reply, ListaTarefas.listar_tarefas(tarefas), tarefas}
  end
end
```
O módulo `TaskSupervisor.TaskHandler` é um processo GenServer que lida com as chamadas relacionadas às tarefas. Ele possui handlers para adicionar tarefas à lista, remover tarefas da lista e listar todas as tarefas da lista.

Espero que esse código atenda às suas expectativas!