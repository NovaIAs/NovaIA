Claro! Aqui está um exemplo de um código complexo em Elixir:

```elixir
defmodule ExemploComplexo do
  @moduledoc """
  Este módulo contém um exemplo complexo de código em Elixir.
  """

  # Função principal
  def run() do
    IO.puts("Bem-vindo ao ExemploComplexo!")
    IO.puts("Este código demonstra algumas funcionalidades avançadas da linguagem Elixir.")

    # Exemplo de lista de usuários
    usuarios = [
      %{"nome" => "João", "idade" => 25, "email" => "joao@example.com"},
      %{"nome" => "Maria", "idade" => 30, "email" => "maria@example.com"},
      %{"nome" => "Carlos", "idade" => 35, "email" => "carlos@example.com"}
    ]

    # Filtrar usuários com menos de 30 anos
    usuarios_jovens = Enum.filter(usuarios, fn usuario -> usuario["idade"] < 30 end)

    # Mapear lista de usuários para uma lista de mensagens personalizadas
    mensagens = Enum.map(usuarios_jovens, fn usuario ->
      "Olá, #{usuario["nome"]}! Você tem #{usuario["idade"]} anos."
    end)

    # Imprimir as mensagens na tela
    Enum.each(mensagens, &IO.puts/1)

    # Calcular a média de idade dos usuários
    media_idade = calcular_media_idade(usuarios)

    IO.puts("A média de idade dos usuários é: #{media_idade}")
  end

  # Função auxiliar para calcular a média de idade
  defp calcular_media_idade(usuarios) do
    soma_idades = Enum.reduce(usuarios, 0, fn usuario, acc -> acc + usuario["idade"] end)
    total_usuarios = length(usuarios)

    soma_idades / total_usuarios
  end
end

# Executar o exemplo
ExemploComplexo.run()
```

Este código em Elixir é um exemplo complexo que demonstra algumas funcionalidades avançadas da linguagem. Ele começa definindo um módulo chamado `ExemploComplexo` com uma documentação explicativa.

A função `run/0` é a função principal, que é chamada para executar o exemplo. Dentro dela, temos um exemplo de uma lista de usuários representados como mapas. Em seguida, utilizamos a função `Enum.filter/2` para filtrar apenas os usuários com menos de 30 anos. A função `Enum.map/2` é usada para mapear a lista de usuários filtrados em uma lista de mensagens personalizadas.

Depois, utilizamos a função `Enum.each/2` para imprimir as mensagens na tela. Em seguida, temos a função auxiliar `calcular_media_idade/1`, que calcula a média de idade dos usuários utilizando as funções `Enum.reduce/3` e `length/1`.

Por fim, chamamos a função `run/0` para executar o exemplo. Ao executar este código, ele imprimirá as mensagens personalizadas na tela e também a média de idade dos usuários.

Este exemplo demonstra o uso de funções de alta ordem, manipulação de listas, mapas e algumas outras funcionalidades avançadas da linguagem Elixir.