Claro! Aqui está um exemplo de um código complexo em Elixir, que consiste em um sistema de gerenciamento de tarefas:

```elixir
defmodule GerenciadorTarefas do
  @moduledoc """
  Módulo responsável por gerenciar as tarefas.
  """

  defstruct nome: "", descricao: "", concluida?: false

  @doc """
  Cria uma nova tarefa.
  
  ## Parâmetros
  
    * `nome` - O nome da tarefa.
    * `descricao` - A descrição da tarefa.
    
  ## Retorno
  
  Uma struct representando a nova tarefa.
  """
  def criar_tarefa(nome, descricao) do
    %GerenciadorTarefas{nome: nome, descricao: descricao}
  end

  @doc """
  Marca uma tarefa como concluída.
  
  ## Parâmetros
  
    * `tarefa` - A tarefa a ser marcada como concluída.
    
  ## Retorno
  
  A tarefa com o campo `concluida?` atualizado para `true`.
  """
  def marcar_tarefa_concluida(tarefa) do
    %{tarefa | concluida?: true}
  end

  @doc """
  Lista todas as tarefas.
  
  ## Parâmetros
  
    Nenhum.
    
  ## Retorno
  
  Uma lista com todas as tarefas.
  """
  def listar_tarefas() do
    # Simulação de uma lista de tarefas
    tarefas = [
      criar_tarefa("Tarefa 1", "Descrição da tarefa 1"),
      criar_tarefa("Tarefa 2", "Descrição da tarefa 2"),
      criar_tarefa("Tarefa 3", "Descrição da tarefa 3")
    ]
    
    Enum.map(tarefas, fn tarefa ->
      %GerenciadorTarefas{tarefa | descricao: tarefa.descricao <> " (Atualizada)"}
    end)
  end
end
```

Explicação do código:

1. Começamos definindo um módulo chamado `GerenciadorTarefas` usando a macro `defmodule`.
2. A documentação do módulo é definida usando a diretiva `@moduledoc`.
3. Em seguida, definimos a struct `GerenciadorTarefas` usando a macro `defstruct`, que possui os campos `nome`, `descricao` e `concluida?`.
4. Em seguida, definimos a função `criar_tarefa/2` que cria uma nova tarefa recebendo o nome e a descrição como parâmetros. A função retorna uma struct `GerenciadorTarefas` com os valores fornecidos.
5. A função `marcar_tarefa_concluida/1` recebe uma tarefa como parâmetro e retorna a tarefa com o campo `concluida?` atualizado para `true`.
6. A função `listar_tarefas/0` lista todas as tarefas disponíveis. Neste exemplo, simulamos uma lista de tarefas fixa usando uma lista de tarefas criadas com a função `criar_tarefa/2`. Em seguida, utilizamos a função `Enum.map` para atualizar a descrição de cada tarefa adicionando " (Atualizada)" ao final.

Espero que isso atenda às suas expectativas! Se você tiver alguma outra solicitação, estou aqui para ajudar.