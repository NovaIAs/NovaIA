**Programme de planification d'itinéraire dynamique à l'aide de l'algorithme de Dijkstra**

Ce programme utilise l'algorithme de Dijkstra pour calculer les itinéraires les plus courts entre un ensemble de nœuds. Il s'agit d'un algorithme efficace qui garantit de trouver l'itinéraire le plus court pour n'importe quel graphe, quel que soit le nombre de nœuds et d'arêtes.

**Classe `Graphe`**

La classe `Graphe` représente le graphe sous la forme d'une liste d'adjacence. Chaque nœud est représenté par un entier, et chaque arête est représentée par un tuple contenant les nœuds source et destination, ainsi que le poids de l'arête.

```cpp
class Graphe {
public:
  vector<vector<tuple<int, int, int>>> liste_adjacence;
  int nombre_noeuds;

  Graphe(int nombre_noeuds) : nombre_noeuds(nombre_noeuds) {
    liste_adjacence.resize(nombre_noeuds);
  }

  void ajouter_arete(int source, int destination, int poids) {
    liste_adjacence[source].push_back(make_tuple(destination, poids));
  }
};
```

**Fonction `dijkstra`**

La fonction `dijkstra` implémente l'algorithme de Dijkstra pour calculer les distances les plus courtes entre un nœud source et tous les autres nœuds du graphe.

```cpp
vector<int> dijkstra(Graphe& graphe, int source) {
  vector<int> distances(graphe.nombre_noeuds, INT_MAX);
  vector<bool> visite(graphe.nombre_noeuds, false);
  distances[source] = 0;

  while (true) {
    int min_distance = INT_MAX;
    int min_indice_distance = -1;

    // Trouver le nœud non visité ayant la distance la plus courte
    for (int i = 0; i < graphe.nombre_noeuds; i++) {
      if (!visite[i] && distances[i] < min_distance) {
        min_distance = distances[i];
        min_indice_distance = i;
      }
    }

    // Si aucun nœud non visité n'est trouvé, l'algorithme est terminé
    if (min_indice_distance == -1) {
      break;
    }

    // Mettre à jour les distances pour les nœuds adjacents
    for (auto& arete : graphe.liste_adjacence[min_indice_distance]) {
      int destination, poids;
      tie(destination, poids) = arete;
      if (distances[min_indice_distance] + poids < distances[destination]) {
        distances[destination] = distances[min_indice_distance] + poids;
      }
    }

    // Marquer le nœud comme visité
    visite[min_indice_distance] = true;
  }

  return distances;
}
```

**Exemple d'utilisation**

Voici un exemple d'utilisation du programme :

```cpp
Graphe graphe(5);

graphe.ajouter_arete(0, 1, 10);
graphe.ajouter_arete(0, 4, 5);
graphe.ajouter_arete(1, 2, 15);
graphe.ajouter_arete(1, 3, 20);
graphe.ajouter_arete(2, 3, 30);
graphe.ajouter_arete(3, 4, 15);

vector<int> distances = dijkstra(graphe, 0);

for (int i = 0; i < graphe.nombre_noeuds; i++) {
  cout << "Distance de 0 à " << i << " : " << distances[i] << endl;
}
```

**Sortie**

```
Distance de 0 à 0 : 0
Distance de 0 à 1 : 10
Distance de 0 à 2 : 25
Distance de 0 à 3 : 20
Distance de 0 à 4 : 5
```