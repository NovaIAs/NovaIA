**Code complexe en C++**

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <utility>
#include <map>
#include <set>
#include <string>
#include <tuple>
#include <cmath>
#include <cstdlib>
#include <ctime>

using namespace std;

// Définition d'alias pour simplifier le code
using VI = vector<int>;
using VS = vector<string>;
using VPII = vector<pair<int, int>>;
using VVI = vector<VI>;
using VST = vector<set<int>>;

// Fonctions utilitaires
template <typename T>
void print(const T& container) {
    for (const auto& item : container) {
        cout << item << " ";
    }
    cout << endl;
}

template <typename T>
void print2D(const T& container) {
    for (const auto& row : container) {
        print(row);
    }
}

// Génère un nombre aléatoire dans une plage donnée
int randInt(int min, int max) {
    return (rand() % (max - min + 1)) + min;
}

// Classe représentant un graphe non dirigé pondéré
class Graph {
public:
    // Constructeur
    Graph(int numNodes) : numNodes(numNodes) {
        adjList.resize(numNodes);
    }

    // Ajoute une arête entre deux noeuds avec un poids donné
    void addEdge(int u, int v, int weight) {
        adjList[u].push_back({v, weight});
        adjList[v].push_back({u, weight});
    }

    // Calcule le plus court chemin entre deux noeuds en utilisant l'algorithme de Dijkstra
    VI dijkstra(int start, int end) {
        // Initialisation
        VI dist(numNodes, INT_MAX);
        set<int> visited;
        dist[start] = 0;

        // Boucle principale de Dijkstra
        while (!visited.empty()) {
            // Trouver le noeud non visité avec la plus courte distance
            int u = -1;
            for (int i = 0; i < numNodes; i++) {
                if (!visited.count(i) && (u == -1 || dist[i] < dist[u])) {
                    u = i;
                }
            }

            // Si tous les noeuds sont visités, arrêter
            if (u == -1) break;

            // Visiter le noeud u
            visited.insert(u);

            // Mettre à jour les distances des noeuds adjacents
            for (const auto& edge : adjList[u]) {
                int v = edge.first;
                int weight = edge.second;
                if (dist[u] + weight < dist[v]) {
                    dist[v] = dist[u] + weight;
                }
            }
        }

        // Retourner le plus court chemin de start à end
        VI path;
        if (dist[end] != INT_MAX) {
            int current = end;
            while (current != start) {
                path.push_back(current);
                for (const auto& edge : adjList[current]) {
                    if (edge.first == start) {
                        current = edge.first;
                        break;
                    }
                }
            }
            path.push_back(start);
            reverse(path.begin(), path.end());
        }
        return path;
    }

private:
    int numNodes;
    VVI adjList;  // Liste d'adjacence pour représenter le graphe
};

int main() {
    // Initialiser le générateur de nombres aléatoires
    srand(time(NULL));

    // Créer un graphe non dirigé pondéré avec 10 noeuds
    Graph graph(10);

    // Ajouter des arêtes au graphe avec des poids aléatoires
    for (int i = 0; i < 100; i++) {
        int u = randInt(0, 9);
        int v = randInt(0, 9);
        int weight = randInt(1, 10);
        graph.addEdge(u, v, weight);
    }

    // Imprimer la liste d'adjacence du graphe
    cout << "Liste d'adjacence du graphe : " << endl;
    print2D(graph.adjList);

    // Calculer le plus court chemin entre deux noeuds
    int start = 0;
    int end = 9;
    VI path = graph.dijkstra(start, end);

    // Imprimer le plus court chemin
    if (path.empty()) {
        cout << "Pas de chemin entre " << start << " et " << end << endl;
    } else {
        cout << "Plus court chemin entre " << start << " et " << end << " : ";
        print(path);
    }

    return 0;
}
```

**Explication du code**

Ce code est conçu pour simuler un graphe non dirigé pondéré et pour calculer le plus court chemin entre deux noeuds donnés en utilisant l'algorithme de Dijkstra. Voici l'explication détaillée :

* **Fonctions utilitaires** :
    * `print` et `print2D` sont des fonctions utilitaires pour imprimer des conteneurs.
    * `randInt` génère un nombre aléatoire dans une plage donnée.

* **Classe `Graph`** :
    * Cette classe représente un graphe non dirigé pondéré.
    * Elle contient une liste d'adjacence pour stocker les arêtes du graphe.
    * Elle fournit des méthodes pour :
        * Ajouter des arêtes au graphe.
        * Calculer le plus court chemin entre deux noeuds à l'aide de l'algorithme de Dijkstra.

* **Fonction `main`** :
    * Initialise le générateur de nombres aléatoires.
    * Crée un graphe avec 10 noeuds.
    * Ajoute 100 arêtes au graphe avec des poids aléatoires.
    * Imprime la liste d'adjacence du graphe.
    * Calcule et imprime le plus court chemin entre les noeuds 0 et 9.

L'algorithme de Dijkstra est un algorithme classique utilisé pour trouver le plus court chemin entre deux noeuds dans un graphe pondéré non dirigé. Il fonctionne en explorant le graphe à partir du noeud de départ et en mettant à jour itérativement les distances aux autres noeuds jusqu'à ce que le noeud d'arrivée soit atteint.