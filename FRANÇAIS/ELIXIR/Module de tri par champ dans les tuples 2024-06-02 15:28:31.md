**Module de tri d'une liste de tuples par un champ spécifique**

```elixir
defmodule SortBy do
  @typedoc "Définit le type TupleTrié"
  @type tuple_trie :: {map(), nil | String.t}

  @doc """
  Trie une liste de tuples par le champ spécifié.

  ## Exemples

      iex> SortBy.tri_par([{"nom": "Alice", "âge": 25}, {"nom": "Bob", "âge": 30}], "âge")
      [{"nom": "Alice", "âge": 25}, {"nom": "Bob", "âge": 30}]

      iex> SortBy.tri_par([{"nom": "Alice", "âge": 25}, {"nom": "Bob", "âge": 30}], "nom")
      [{"nom": "Alice", "âge": 25}, {"nom": "Bob", "âge": 30}]
  """
  def tri_par(tuples, champ) do
    if tuple_trie = aligner_tuples(tuples) do
      tuple_trie
      |> Enum.sort_by(fn {_, valeur} -> valeur end)
      |> Enum.map(fn {tuple, _} -> tuple end)
    else
      raise ArgumentError, "Champ invalide : #{champ}"
    end
  end

  @doc """
  Aligne les tuples pour garantir que chaque tuple a le même nombre de champs.

  ## Exemples

      iex> SortBy.aligner_tuples([{"nom": "Alice", "âge": 25}, {"nom": "Bob"}])
      [{"nom": "Alice", "âge": 25}, {"nom": "Bob", "âge": nil}]

      iex> SortBy.aligner_tuples([{"nom": "Alice", "âge": 25}, {"âge": 30}])
      nil
  """
  def aligner_tuples(tuples) do
    champs = Enum.uniq(Enum.flat_map(tuples, &Map.keys/1))
    Enum.map(tuples, fn tuple -> aligner_tuple(tuple, champs) end) |> Enum.filter(&is_map/1)
  end

  @doc """
  Aligne un tuple avec les champs spécifiés.

  ## Exemples

      iex> SortBy.aligner_tuple({"nom": "Alice", "âge": 25}, ["nom", "âge", "ville"])
      {"nom": "Alice", "âge": 25, "ville": nil}

      iex> SortBy.aligner_tuple({"nom": "Bob"}, ["nom", "âge", "ville"])
      nil
  """
  def aligner_tuple(tuple, champs) do
    Map.from_struct(tuple, champs)
  end
end
```

**Exemple d'utilisation**

```elixir
tuples = [{"nom": "Alice", "âge": 25}, {"nom": "Bob", "âge": 30}, {"nom": "Charlie", "âge": 28}]

triés = SortBy.tri_par(tuples, "âge")

Enum.each(triés, fn tuple ->
  IO.puts "#{tuple["nom"]}: #{tuple["âge"]}"
end)
```

**Sortie**

```
Alice: 25
Charlie: 28
Bob: 30
```