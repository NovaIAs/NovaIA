```elixir
# Module pour gérer les structures de données complexes en Elixir

defmodule DataStructures do
  @moduledoc """
  Ce module fournit des fonctions pour manipuler des structures de données complexes en Elixir.
  """

  defstruct [:nom, :age, :salaire]

  # Créer une nouvelle structure de données
  def new(nom, age, salaire) do
    %__MODULE__{nom: nom, age: age, salaire: salaire}
  end

  # Obtenir un champ de la structure de données
  def get_field(structure, champ) do
    Map.get(structure, champ)
  end

  # Définir un champ de la structure de données
  def set_field(structure, champ, valeur) do
    Map.put(structure, champ, valeur)
  end

  # Afficher la structure de données sous forme de chaîne
  def to_string(structure) do
    "Nom: #{structure.nom}, Âge: #{structure.age}, Salaire: #{structure.salaire}"
  end
end

# Exemple d'utilisation
structure = DataStructures.new("Jean", 30, 1000.0)
nom = DataStructures.get_field(structure, :nom)
salaire_augmenté = DataStructures.set_field(structure, :salaire, 1200.0)
IO.puts(DataStructures.to_string(salaire_augmenté))

```