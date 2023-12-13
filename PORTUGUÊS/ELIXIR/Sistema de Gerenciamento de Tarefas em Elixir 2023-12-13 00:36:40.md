Claro! Aqui está um exemplo de um código complexo em Elixir, que envolve a criação de um sistema de gerenciamento de tarefas:

```elixir
defmodule GerenciadorTarefas do
  
  defstruct nome: "", descricao: "", prazo: "", concluida?: false
  
  def criar_tarefa(nome, descricao, prazo) do
    %GerenciadorTarefas{nome: nome, descricao: descricao, prazo: prazo}
  end
  
  def editar_tarefa(tarefa, nome, descricao, prazo) do
    %GerenciadorTarefas{tarefa | nome: nome, descricao: descricao, prazo: prazo}
  end
  
  def marcar_como_concluida(tarefa) do
    %GerenciadorTarefas{tarefa | concluida?: true}
  end
  
  def listar_tarefas(tarefas) do
    Enum.each(tarefas, fn tarefa ->
      IO.puts "Tarefa: #{tarefa.nome}"
      IO.puts "Descrição: #{tarefa.descricao}"
      IO.puts "Prazo: #{tarefa.prazo}"
      IO.puts "Concluída: #{tarefa.concluida?}\n"
    end)
  end
  
end

defmodule Main do
  
  def start do
    t1 = GerenciadorTarefas.criar_tarefa("Comprar leite", "Ir ao mercado e comprar leite", "10/05/2022")
    t2 = GerenciadorTarefas.criar_tarefa("Pagar contas", "Pagar as contas de luz, água e telefone", "15/05/2022")
    
    tarefas = [t1, t2]
    
    tarefas = tarefas 
    |> GerenciadorTarefas.editar_tarefa("Comprar leite", "Comprar leite desnatado", "12/05/2022")
    |> GerenciadorTarefas.marcar_como_concluida
    
    GerenciadorTarefas.listar_tarefas(tarefas)
  end
  
end

Main.start()
```

Neste código, temos a definição de um módulo chamado `GerenciadorTarefas`, que é responsável pelas operações relacionadas a criação, edição, marcação de conclusão e listagem de tarefas. O módulo possui uma estrutura de dados `defstruct` que define o formato de cada tarefa, contendo os campos `nome`, `descricao`, `prazo` e `concluida?`.

A função `criar_tarefa/3` é responsável por criar uma nova tarefa, recebendo como parâmetros o nome, a descrição e o prazo. Ela retorna a tarefa criada.

A função `editar_tarefa/4` recebe uma tarefa existente, e os novos valores para nome, descrição e prazo, e retorna uma nova tarefa com as alterações realizadas.

A função `marcar_como_concluida/1` recebe uma tarefa e retorna uma nova tarefa com o campo `concluida?` definido como `true`.

A função `listar_tarefas/1` recebe uma lista de tarefas e exibe na tela as informações de cada tarefa, como nome, descrição, prazo e se foi concluída ou não.

O módulo `Main` é responsável por iniciar o programa. No exemplo, são criadas duas tarefas, editada a primeira tarefa, marcada como concluída e, por fim, são listadas todas as tarefas.

Espero que este código atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais ajuda, estou à disposição.