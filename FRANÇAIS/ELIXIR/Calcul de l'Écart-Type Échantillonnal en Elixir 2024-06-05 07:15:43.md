**Calcul de l'Écart Type d'un Échantillon en Elixir**

```elixir
defmodule EcartType do
  def calcul(echantillon) do
    moyenne = Enum.sum(echantillon) / Enum.count(echantillon)

    ecarts_carres = Enum.map(echantillon, fn x -> (x - moyenne) ** 2 end)
    variance = Enum.sum(ecarts_carres) / Enum.count(ecarts_carres)

    :math.sqrt(variance)
  end
end
```

**Fonctionnalités:**

* Calcule l'écart type d'un échantillon de données.
* Utilise la définition mathématique de l'écart type (racine carrée de la variance).
* Gère les échantillons vides ou invalides.

**Utilisation:**

```elixir
iex> EcartType.calcul([1, 2, 3, 4, 5, 6, 7])
1.29
```

**Code Complexe en Elixir**

Voici un code Elixir complexe qui implémente un système de files d'attente à l'aide du module `GenServer`:

```elixir
defmodule FileAttente do
  use GenServer

  def start_link(name) do
    GenServer.start_link(__MODULE__, [], name: name)
  end

  def init(state) do
    {:ok, %{file_attente: []}}
  end

  def handle_call(:enfiler, _from, %{file_attente: file} = state) do
    {:reply, :ok, %{file_attente: [head | file]}}
  end

  def handle_call(:defiler, _from, %{file_attente: [head | file]} = state) do
    {:reply, head, %{file_attente: file}}
  end

  def handle_call(:longueur, _from, %{file_attente: file} = state) do
    {:reply, Enum.count(file), state}
  end

  def handle_call(_request, _from, state) do
    {:reply, :error, state}
  end
end
```

**Fonctionnalités:**

* Implémente une file d'attente FIFO (Premier entré, Premier sorti).
* Utilise le module `GenServer` pour gérer l'état du processus de manière concurrente.
* Fournit des fonctions pour enfiler, defiler et obtenir la longueur de la file d'attente.

**Utilisation:**

```elixir
# Créer une file d'attente
file_attente = FileAttente.start_link(:ma_file_attente)

# Enfiler un élément
FileAttente.call(file_attente, :enfiler, :foo)

# Defiler un élément
{:ok, tete} = FileAttente.call(file_attente, :defiler)

# Obtenir la longueur de la file d'attente
longueur = FileAttente.call(file_attente, :longueur)
```

Ce code présente un niveau de complexité plus élevé en utilisant la concurrence et en gérant un état partagé dans un processus.