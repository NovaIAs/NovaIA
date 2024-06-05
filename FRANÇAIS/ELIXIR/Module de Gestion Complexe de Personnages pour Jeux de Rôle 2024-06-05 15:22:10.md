**Module de gestion de personnages complexe pour un jeu de rôle**

```elixir
defmodule Personagem do
  defstruct [
    :nome,
    :classe,
    :nivel,
    :atributos,
    :pericias,
    :itens,
    :historico,
    :experiencia,
    :tendencias_morais
  ]

  @enforce_keys [:nome, :classe, :nivel, :atributos, :pericias]

  def new(nome, classe, nivel, atributos, pericias) do
    %Personagem{
      nome: nome,
      classe: classe,
      nivel: nivel,
      atributos: atributos,
      pericias: pericias,
      itens: [],
      historico: [],
      experiencia: 0,
      tendencias_morais: %{}
    }
  end

  def add_item(personagem, item) do
    {:ok, Map.put(personagem, :itens, personagem.itens ++ [item])}
  end

  def remove_item(personagem, item) do
    {:ok, Map.put(personagem, :itens, Enum.filter(personagem.itens, fn i -> i != item end))}
  end

  def add_historico(personagem, evento) do
    {:ok, Map.put(personagem, :historico, personagem.historico ++ [evento])}
  end

  def add_experiencia(personagem, experiencia) do
    {:ok, Map.put(personagem, :experiencia, personagem.experiencia + experiencia)}
  end

  def add_tendencia_moral(personagem, tendencia, valor) do
    {:ok, Map.put(personagem, :tendencias_morais, Map.put(personagem.tendencias_morais, tendencia, valor))}
  end
end
```

**Exemple d'utilisation**

```elixir
personagem = Personagem.new("Aragorn", "Guerreiro", 5, %{forca: 16, destreza: 14, constituicao: 14, inteligencia: 12, sabedoria: 10, carisma: 14}, %{combate: 4, furtividade: 2})

Personagem.add_item(personagem, "Espada Curta")
Personagem.add_historico(personagem, "Derrotou o Orc-Chefe")
Personagem.add_experiencia(personagem, 100)

IO.inspect(personagem)

# Affiche les informations sur le personnage
# %Personagem{
#   nome: "Aragorn",
#   classe: "Guerreiro",
#   nivel: 5,
#   atributos: %{forca: 16, destreza: 14, constituicao: 14, inteligencia: 12, sabedoria: 10, carisma: 14},
#   pericias: %{combate: 4, furtividade: 2},
#   itens: ["Espada Curta"],
#   historico: ["Derrotou o Orc-Chefe"],
#   experiencia: 100,
#   tendencias_morais: %{}
# }
```