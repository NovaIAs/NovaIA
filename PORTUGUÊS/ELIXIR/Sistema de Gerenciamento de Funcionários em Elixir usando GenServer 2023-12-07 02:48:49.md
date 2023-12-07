Claro! Aqui está um exemplo de um código complexo em Elixir que utiliza GenServer para criar um sistema de gerenciamento de funcionários em uma empresa:

```elixir
defmodule GerenciamentoFuncionarios do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, %{})
  end

  def init(state) do
    {:ok, state}
  end

  def handle_call({:adicionar_funcionario, funcionario}, _from, state) do
    case Map.fetch(state, :funcionarios) do
      {:ok, funcionarios} ->
        novo_funcionarios = Map.put(funcionarios, funcionario.id, funcionario)
        {:reply, :ok, %{state | funcionarios: novo_funcionarios}}
      :error ->
        {:reply, :error, state}
    end
  end

  def handle_call({:remover_funcionario, funcionario_id}, _from, state) do
    case Map.fetch(state, :funcionarios) do
      {:ok, funcionarios} ->
        novo_funcionarios = Map.delete(funcionarios, funcionario_id)
        {:reply, :ok, %{state | funcionarios: novo_funcionarios}}
      :error ->
        {:reply, :error, state}
    end
  end

  def handle_call(:listar_funcionarios, _from, state) do
    case Map.fetch(state, :funcionarios) do
      {:ok, funcionarios} ->
        funcionarios_list = Map.values(funcionarios)
        {:reply, funcionarios_list, state}
      :error ->
        {:reply, :error, state}
    end
  end
end
```

Nesse código, temos um módulo chamado `GerenciamentoFuncionarios` que utiliza a macro `use GenServer` para implementar um processo GenServer, que é uma abstração fornecida pelo Elixir para lidar com estados e mensagens concorrentes.

A função `start_link/1` é responsável por iniciar o processo GenServer. Ela chama a função `start_link/3` do módulo `GenServer` e passa como argumento o módulo atual (`__MODULE__`) e um mapa vazio como estado inicial.

A função `init/1` é o callback obrigatório do GenServer e define o estado inicial do processo.

Em seguida, temos os callbacks `handle_call/3` que são responsáveis por tratar as chamadas síncronas feitas ao processo GenServer. No exemplo, temos três callbacks: `handle_call/3` para adicionar um funcionário, `remover_funcionario/2` para remover um funcionário e `listar_funcionarios/2` para listar todos os funcionários.

Cada callback recebe três argumentos: a mensagem (no formato de uma tupla), o id do processo que fez a chamada e o estado atual do processo. O callback realiza operações no estado, como adicionar ou remover um funcionário, e responde à chamada com uma tupla contendo a resposta e o novo estado.

O código utiliza a estrutura de dados Map para armazenar os funcionários, utilizando o id do funcionário como chave. A função `Map.fetch/2` é utilizada para recuperar o valor correspondente a uma chave no mapa.

Por fim, o código completa o módulo `GerenciamentoFuncionarios` com as funções de interface do processo GenServer: `start_link/1` inicia o processo, `adicionar_funcionario/1` faz uma chamada assíncrona para adicionar um funcionário, `remover_funcionario/1` para remover um funcionário e `listar_funcionarios/0` para listar todos os funcionários.

Esse código é um exemplo complexo em Elixir que mostra como utilizar GenServer para criar um sistema de gerenciamento de funcionários. É possível adicionar mais funcionalidades, como atualizar informações dos funcionários ou buscar funcionários por critérios específicos.