Claro! Aqui está um exemplo de um código complexo em Elixir que envolve a criação de uma aplicação de lista de tarefas com funcionalidades avançadas:

```elixir
defmodule ListaTarefas do
  defstruct [:id, :nome, :descricao, :data_limite, :concluida]

  def criar_tarefa(nome, descricao, data_limite) do
    %ListaTarefas{
      id: :uuid.generate(),
      nome: nome,
      descricao: descricao,
      data_limite: data_limite,
      concluida: false
    }
  end

  def atualizar_tarefa(tarefa, atributos) do
    Map.merge(tarefa, atributos)
  end

  def concluir_tarefa(tarefa) do
    atualizar_tarefa(tarefa, %{concluida: true})
  end

  def filtrar_tarefas(lista, filtro) do
    lista
    |> Enum.filter(fn tarefa -> apply(filtro, [tarefa]) end)
  end

  def listar_tarefas_por_data(lista) do
    lista
    |> Enum.sort_by(fn tarefa -> tarefa.data_limite end)
  end
end

defmodule ListaTarefasApp do
  def start() do
    lista = []

    tarefa1 = ListaTarefas.criar_tarefa("Comprar mantimentos", "Ir ao mercado comprar comida", ~D[2022-01-10])
    tarefa2 = ListaTarefas.criar_tarefa("Fazer exercícios", "Correr por 30 minutos", ~D[2022-01-15])
    tarefa3 = ListaTarefas.criar_tarefa("Estudar programação", "Aprender Elixir", ~D[2022-01-20])

    lista = [tarefa1, tarefa2, tarefa3]

    IO.puts("Lista de tarefas:")
    IO.inspect(lista)

    tarefa2_concluida = ListaTarefas.concluir_tarefa(tarefa2)
    lista_atualizada = ListaTarefas.atualizar_tarefa(lista, %{1 => tarefa2_concluida})

    IO.puts("Lista de tarefas atualizada:")
    IO.inspect(lista_atualizada)

    tarefas_concluidas = ListaTarefas.filtrar_tarefas(lista_atualizada, &(&1.concluida))
    IO.puts("Tarefas concluídas:")
    IO.inspect(tarefas_concluidas)

    tarefas_por_data = ListaTarefas.listar_tarefas_por_data(lista_atualizada)
    IO.puts("Tarefas ordenadas por data:")
    IO.inspect(tarefas_por_data)
  end
end

ListaTarefasApp.start()
```

Neste código, temos duas definições de módulos: `ListaTarefas` e `ListaTarefasApp`. O módulo `ListaTarefas` contém todas as funções relacionadas à manipulação de tarefas, como criar, atualizar, concluir, filtrar e listar tarefas. O módulo `ListaTarefasApp` é responsável por iniciar a aplicação e demonstrar o uso das funções do módulo `ListaTarefas`.

A função `criar_tarefa/3` cria uma nova tarefa com um ID gerado aleatoriamente, nome, descrição e data limite. A função `atualizar_tarefa/2` recebe uma tarefa existente e atributos atualizados, fundindo-os em um novo mapa. A função `concluir_tarefa/1` marca uma tarefa como concluída, atualizando o atributo `concluida` para `true`.

A função `filtrar_tarefas/2` recebe uma lista de tarefas e uma função de filtro personalizada, aplicando-a a cada tarefa e retornando apenas as tarefas que satisfazem o critério do filtro. A função `listar_tarefas_por_data/1` ordena as tarefas por data limite.

No módulo `ListaTarefasApp`, a função `start/0` inicia a aplicação. Neste exemplo, criamos uma lista vazia e três tarefas. Demonstramos como criar, atualizar e filtrar as tarefas, bem como listar as tarefas ordenadas por data.

Espero que este exemplo de código em Elixir atenda às suas expectativas!