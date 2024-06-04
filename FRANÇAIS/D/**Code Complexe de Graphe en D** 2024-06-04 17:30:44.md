**Code complexe en D**

**Objectif :** Implémentation d'un graphe complexe avec des algorithmes de recherche et de tri avancés.

**Importations de modules :**

```d
import std.traits;
```

**Définition du type de graphe :**

```d
immutable struct Graphe {
    immutable entries = Entries.empty;
    Graphe(Entries e) : this(e) {}

     Entries entries;
    immutable edges = Edges.empty;
    immutable Edge iterEdges() pure const nothrow;

    mutable Edges edges;

    void addEdge(Edge edge) {
        this.edges ~= edge;
    }

    Graphe addEdges(Edges edges) {
        Graphe(this.entries, this.edges ~= edges);
    }

    immutable Edge connect(Node src, Node dst) {
        addEdge(Edge(src, dst));
    }

    immutable Edge get(Node src, Node dst) {
        in this.entries {
            auto edge = this.edges[src][dst];
            if (edge is! Edge)
                throw "Pas d'arête entre $src et $dst";
            return edge;
        }
    }
}
```

**Type d'arête :**

```d
immutable struct Edge {
    immutable Node src;
    immutable Node dst;
    Edge(Node src, Node dst) : this(src, dst) {}
}
```

**Types de nœuds et d'entrées :**

```d
immutable trait Node {}
immutable interface Entries {
    immutable N entries;
}
immutable interface Edges {
    immutable N entries;
}
```

**Algorithme de recherche en profondeur (DFS) :**

```d
immutable struct DFS {
    void run(Graphe graphe, Node start) {
        auto visited = std.container.HashSet(graphe.entries.nodes);

        visitor = foreach (Node<Graphe> node) {
            if (visited.has(node))
                return;
            visited ~= node;
            yield node;
            foreach (Edge<Graphe> edge in graphe.edges[node]) {
                yield edge.dst;
            }
        };
    }

    immutable foreach(Node<Graphe>) visitor;
}
```

**Algorithme de tri topologique :**

```d
immutable struct TopoSort {
    static if (bool hasCycles) ImmutableTreeSet() else immutable array<Node>() run(Graphe graphe) {
        auto visited = std.container.HashSet(graphe.entries.nodes);
        auto result = std.container.ImmutableTreeSet(graphe.entries.nodes);

        foreach (Edge<Graphe> edge in graphe.edges) {
            if (!hasCycles)
                visit(graphe, edge.dst, result, visited);
            else
                visit(graphe, edge.dst, current, visited);
        }

        if (hasCycles)
            return ImmutableTreeSet();
        else
            return ImmutableArray(result.to!array);
    }

    static void visit(Graphe graphe, Node node, auto& result, auto& visited) {
        if (visited.has(node))
            return;
        visited ~= node;
        foreach (Edge<Graphe> edge in graphe.edges[node]) {
            visit(graphe, edge.dst, result, visited);
        }
        result ~= node;
    }
}
```

**Exemple d'utilisation :**

```d
auto graphe = Graphe();

graphe.addEdge(Edge(Node("A"), Node("B")));
graphe.addEdge(Edge(Node("B"), Node("C")));
graphe.addEdge(Edge(Node("C"), Node("A")));

auto dfs = DFS();
dfs.run(graphe, Node("A"));

auto topoSort = TopoSort();
auto sorted = topoSort.run(graphe);
```

**Explications :**

* Le type `Graphe` représente un graphe non pondéré et non orienté.
* Les types `Node`, `Edge` et `Entries` modélisent les éléments constitutifs du graphe.
* Le DFS parcourt le graphe en profondeur et visite tous les nœuds accessibles depuis le nœud de départ.
* Le tri topologique trie les nœuds du graphe dans un ordre tel que chaque nœud précède ses successeurs.
* L'exemple d'utilisation crée un graphe cyclique, exécute le DFS et le tri topologique, et imprime les résultats.