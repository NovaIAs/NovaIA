Claro! Aqui está um exemplo de código complexo em Elixir que utiliza ETS (Erlang Term Storage) para armazenar dados e manipulá-los de maneiras diferentes:

```elixir
defmodule MeuCodigoComplexo do

  # Cria uma tabela na ETS para armazenar os dados
  :ets.new(:minha_tabela, [:ordered_set, :public, :named_table])

  # Função para adicionar um item à tabela
  def adicionar_item(chave, valor) do
    :ets.insert(:minha_tabela, {chave, valor})
  end

  # Função para buscar um item na tabela por chave
  def buscar_por_chave(chave) do
    :ets.lookup(:minha_tabela, chave)
  end

  # Função para buscar um item na tabela por valor
  def buscar_por_valor(valor) do
    :ets.select(:minha_tabela, fn({_, v}) -> v == valor end)
  end

  # Função para verificar se a tabela está vazia
  def tabela_vazia? do
    :ets.info(:minha_tabela)[:size] == 0
  end

  # Função para percorrer e imprimir todos os itens da tabela
  def imprimir_todos_itens do
    :ets.tab2list(:minha_tabela)
    |> Enum.each(fn {chave, valor} -> IO.puts "#{chave}: #{valor}" end)
  end

end

# Exemplo de utilização das funções do módulo

# Adiciona alguns dados à tabela
MeuCodigoComplexo.adicionar_item(:chave1, "valor1")
MeuCodigoComplexo.adicionar_item(:chave2, "valor2")
MeuCodigoComplexo.adicionar_item(:chave3, "valor3")

# Busca um item na tabela por chave
resultado = MeuCodigoComplexo.buscar_por_chave(:chave2)
IO.inspect resultado

# Busca todos os itens da tabela por valor
resultado = MeuCodigoComplexo.buscar_por_valor("valor3")
IO.inspect resultado

# Verifica se a tabela está vazia
resultado = MeuCodigoComplexo.tabela_vazia?
IO.inspect resultado

# Imprime todos os itens da tabela
MeuCodigoComplexo.imprimir_todos_itens
```

Neste código, temos um módulo chamado `MeuCodigoComplexo` que utiliza a biblioteca `ETS` para criar uma tabela de armazenamento de dados em memória. A tabela é inicializada utilizando a função `:ets.new/2`. Em seguida, temos diferentes funções para interagir com a tabela: `adicionar_item/2` (adicionar um item à tabela), `buscar_por_chave/1` (buscar um item na tabela por chave), `buscar_por_valor/1` (buscar um item na tabela por valor), `tabela_vazia?/0` (verificar se a tabela está vazia) e `imprimir_todos_itens/0` (percorrer e imprimir todos os itens da tabela). 

No exemplo de utilização das funções, são adicionados alguns itens à tabela utilizando `MeuCodigoComplexo.adicionar_item/2`. Em seguida, são realizadas buscas na tabela utilizando `MeuCodigoComplexo.buscar_por_chave/1` e `MeuCodigoComplexo.buscar_por_valor/1`. São feitas também verificações de tabela vazia utilizando `MeuCodigoComplexo.tabela_vazia?/0` e a impressão de todos os itens da tabela utilizando `MeuCodigoComplexo.imprimir_todos_itens/0`.