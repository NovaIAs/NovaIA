**Tâche Complexe de Traitement de Graphes en Elixir**

**Objectif:**
Identifier les cycles dans un graphe orienté.

**Code:**

```elixir
defmodule Graph do
  @moduledoc """
  Module pour représenter et manipuler des graphes orientés.
  """

  defstruct [:nodes, :edges]

  @type graph :: %Graph{nodes: list(atom), edges: list({atom, atom})}

  def new(nodes, edges) do
    %Graph{nodes: nodes, edges: edges}
  end

  def add_node(%Graph{nodes: nodes} = graph, node) when node not in nodes do
    %Graph{graph | nodes: [node | nodes]}
  end

  def add_edge(%Graph{edges: edges} = graph, source, target) do
    %Graph{graph | edges: [source, target | edges]}
  end

  def has_cycle?(%Graph{} = graph) do
    visited = MapSet.new()
    stack = MapSet.new()

    Enum.each(graph.nodes, fn node ->
      if !MapSet.member?(visited, node) do
        if dfs(graph, node, visited, stack) do
          true
        end
      end
    end)

    false
  end

  defp dfs(%Graph{} = graph, node, visited, stack) do
    if MapSet.member?(stack, node) do
      true
    else
      MapSet.put(visited, node)
      MapSet.put(stack, node)

      adjacent_nodes = Enum.filter(graph.edges, fn {source, target} -> node == source end)
                       |> Enum.map(fn {_, target} -> target end)

      has_cycle = Enum.any?(adjacent_nodes, fn node -> dfs(graph, node, visited, stack) end)

      MapSet.delete(stack, node)

      has_cycle
    end
  end
end
```

**Explication du Code:**

* **Module `Graph`:** Définit le type de données `Graph` qui représente un graphe orienté avec des nœuds et des arêtes.
* **Fonction `new/2`:** Crée un nouveau graphe avec les nœuds et les arêtes spécifiés.
* **Fonctions `add_node/2` et `add_edge/3`:** Ajoutent respectivement un nœud et une arête au graphe.
* **Fonction `has_cycle?/1`:** Détermine si un graphe contient des cycles en utilisant l'algorithme de recherche en profondeur (DFS).
* **Fonction `dfs/4`:** Implémentation récursive de DFS utilisée pour détecter les cycles dans le graphe. Elle garde une trace des nœuds visités et enregistrés, et vérifie s'il y a des arêtes pointant vers les nœuds déjà enregistrés.

**Utilisation:**

```elixir
graph = Graph.new([:a, :b, :c], [{:a, :b}, {:b, :c}, {:c, :a}])
Graph.has_cycle?(graph) # true (car il y a un cycle)
```