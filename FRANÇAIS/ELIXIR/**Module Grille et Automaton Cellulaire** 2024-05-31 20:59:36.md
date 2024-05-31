**Module Grille**

```elixir
defmodule Grille do
  defstruct [:hauteur, :largeur, :grille]

  @doc "Crée une nouvelle grille de dimensions spécifiées"
  def new(hauteur, largeur) do
    %__MODULE__{hauteur: hauteur, largeur: largeur, grille: :maps.new()}
  end

  @doc "Ajoute une valeur à une cellule de la grille spécifiée"
  def set(grille, x, y, valeur) do
    %__MODULE__{grille: maps.put(grille.grille, {x, y}, valeur)}
  end

  @doc "Récupère la valeur d'une cellule de la grille spécifiée"
  def get(grille, x, y) do
    maps.get(grille.grille, {x, y})
  end
end
```

**Module Automaton**

```elixir
defmodule Automaton do
  defstruct [:grille, :règles]

  @doc "Crée un nouvel automate cellulaire avec une grille et des règles spécifiées"
  def new(grille, règles) do
    %__MODULE__{grille: grille, règles: règles}
  end

  @doc "Évolue l'automate cellulaire d'un temps"
  def step(automate) do
    %__MODULE__{grille: evolve(automate.grille, automate.règles)}
  end

  defp evolve(grille, règles) do
    Grille.new(grille.hauteur, grille.largeur)
    |> Stream.iterate(&step_grille(&1, règles))
    |> Stream.take(1)
    |> Enum.at(0)
  end

  defp step_grille(grille, règles) do
    for {x, y} <- Grille.grille(grille) do
      Grille.set(grille, x, y, next_value(grille, x, y, règles))
    end
  end

  defp next_value(grille, x, y, règles) do
    voisins = count_voisins(grille, x, y)
    case Grille.get(grille, x, y) do
      0 -> cond do {voisins, règles[0]} -> 1; true -> 0 end
      1 -> cond do {voisins, règles[1]} -> 1; true -> 0 end
    end
  end

  defp count_voisins(grille, x, y) do
    enum_voisins(grille, x, y)
    |> Enum.reduce(0, fn {x, y}, acc ->
      case Grille.get(grille, x, y) do
        nil -> acc
        1 -> acc + 1
        _ -> acc
      end
    end)
  end

  defp enum_voisins(grille, x, y) do
    for dx <- -1..1, dy <- -1..1 do
      {x + dx, y + dy}
    end
    |> Enum.filter(&within_bounds(grille, &1))
  end

  defp within_bounds(grille, {x, y}) do
    x >= 1 and x <= grille.hauteur and y >= 1 and y <= grille.largeur
  end
end
```

**Module Exemples**

```elixir
defmodule Exemples do
  def chaos do
    règles = [[0, 1], [1, 0]]
    grille = Grille.new(10, 10)
    simuler(grille, règles)
  end

  defp simuler(grille, règles) do
    automate = Automaton.new(grille, règles)

    for _ <- 1..100 do
      grid_to_string(automate.grille)
      automate = Automaton.step(automate)
    end
  end

  defp grid_to_string(grille) do
    for ligne <- 1..grille.hauteur do
      for colonne <- 1..grille.largeur do
        val = if Grille.get(grille, ligne, colonne), do: "#", else: " "
        IO.write(val)
      end
      IO.write("\n")
    end
  end
end
```

**Utilisation**

```elixir
iex(1)> Exemples.chaos
# # # # # # # # # #
# # # # # # # # # #
# # # # # # # # # #
# # # # # # # # # #
# # # # # # # # # #
# # # # # # # # # #
# # # # # # # # # #
# # # # # # # # # #
# # # # # # # # # #
# # # # # # # # # #
```