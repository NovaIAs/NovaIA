Claro! Aqui está um código complexo em Elixir que utiliza conceitos avançados da linguagem para criar uma estrutura de dados personalizada.

```elixir
defmodule MinhaEstruturaDeDados do
  @moduledoc """
  Essa é a minha estrutura de dados personalizada.
  """

  defstruct [:lista, :mapa, :conjunto]

  @doc """
  Cria uma nova instância da estrutura de dados.
  
  ## Exemplos
  
      iex> estrutura = MinhaEstruturaDeDados.new()
      %MinhaEstruturaDeDados{lista: [], mapa: %{}, conjunto: MapSet.new()}
  """
  def new() do
    %MinhaEstruturaDeDados{}
  end

  @doc """
  Adiciona um elemento à lista da estrutura de dados.
  
  ## Parâmetros
  
    * `elemento` - O elemento a ser adicionado.
  
  ## Exemplos
  
      iex> estrutura = MinhaEstruturaDeDados.new() |> MinhaEstruturaDeDados.adicionar_elemento("exemplo")
      %MinhaEstruturaDeDados{lista: ["exemplo"], mapa: %{}, conjunto: MapSet.new()}
  """
  def adicionar_elemento(estrutura, elemento) do
    %MinhaEstruturaDeDados{estrutura | lista: [elemento | estrutura.lista]}
  end

  @doc """
  Adiciona um par chave-valor ao mapa da estrutura de dados.
  
  ## Parâmetros
  
    * `chave` - A chave do par chave-valor.
    * `valor` - O valor do par chave-valor.
  
  ## Exemplos
  
      iex> estrutura = MinhaEstruturaDeDados.new() |> MinhaEstruturaDeDados.adicionar_par("chave", "valor")
      %MinhaEstruturaDeDados{lista: [], mapa: %{"chave" => "valor"}, conjunto: MapSet.new()}
  """
  def adicionar_par(estrutura, chave, valor) do
    %MinhaEstruturaDeDados{estrutura | mapa: Map.put(estrutura.mapa, chave, valor)}
  end

  @doc """
  Adiciona um elemento ao conjunto da estrutura de dados.
  
  ## Parâmetros
  
    * `elemento` - O elemento a ser adicionado.
  
  ## Exemplos
  
      iex> estrutura = MinhaEstruturaDeDados.new() |> MinhaEstruturaDeDados.adicionar_elemento_ao_conjunto("exemplo")
      %MinhaEstruturaDeDados{lista: [], mapa: %{}, conjunto: MapSet.new(~w(exemplo))}
  """
  def adicionar_elemento_ao_conjunto(estrutura, elemento) do
    %MinhaEstruturaDeDados{estrutura | conjunto: MapSet.put(estrutura.conjunto, elemento)}
  end
end
```

Neste código, definimos um módulo chamado `MinhaEstruturaDeDados` que representa uma estrutura de dados personalizada. A estrutura possui três campos: `lista`, `mapa` e `conjunto`.

O método `new/0` cria uma nova instância da estrutura de dados, inicializando os campos com valores vazios.

Os métodos `adicionar_elemento/2`, `adicionar_par/3` e `adicionar_elemento_ao_conjunto/2` permitem adicionar elementos à lista, mapa e conjunto, respectivamente. Eles utilizam a sintaxe de atualização de registros do Elixir para criar uma nova instância da estrutura, mantendo os campos existentes e atualizando apenas o campo relevante.

Os exemplos fornecidos demonstram como usar os métodos para adicionar elementos à estrutura de dados.

Este código é apenas um exemplo de como criar uma estrutura de dados personalizada em Elixir. Você pode expandi-lo adicionando mais métodos e funcionalidades, de acordo com suas necessidades.