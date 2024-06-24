**Code Complex en C++**

```cpp
#include <iostream>
#include <algorithm>
#include <vector>
#include <map>
#include <set>

using namespace std;

// Structure pour représenter un graphe
struct Graphe {
    map<int, vector<int>> sommets;
    set<pair<int, int>> arêtes;
};

// Fonction pour ajouter un sommet au graphe
void ajouter_sommet(Graphe& graphe, int sommet) {
    if (!graphe.sommets.count(sommet)) {
        graphe.sommets[sommet] = vector<int>();
    }
}

// Fonction pour ajouter une arête au graphe
void ajouter_arete(Graphe& graphe, int sommet1, int sommet2) {
    if (sommet1 != sommet2) {
        graphe.sommets[sommet1].push_back(sommet2);
        graphe.arêtes.insert(make_pair(sommet1, sommet2));
    }
}

// Fonction pour effectuer un parcours en profondeur du graphe
void parcours_profondeur(const Graphe& graphe, int sommet, vector<bool>& visité) {
    visité[sommet] = true;
    cout << sommet << " ";
    for (int voisin : graphe.sommets[sommet]) {
        if (!visité[voisin]) {
            parcours_profondeur(graphe, voisin, visité);
        }
    }
}

// Fonction pour effectuer un parcours en largeur du graphe
void parcours_largeur(const Graphe& graphe, int sommet) {
    queue<int> file;
    vector<bool> visité(graphe.sommets.size(), false);

    file.push(sommet);
    visité[sommet] = true;

    while (!file.empty()) {
        int sommet_courant = file.front();
        file.pop();
        cout << sommet_courant << " ";

        for (int voisin : graphe.sommets[sommet_courant]) {
            if (!visité[voisin]) {
                file.push(voisin);
                visité[voisin] = true;
            }
        }
    }
}

// Fonction pour détecter les cycles dans un graphe
bool detecter_cycles(const Graphe& graphe) {
    vector<int> parent(graphe.sommets.size(), -1);
    vector<bool> visité(graphe.sommets.size(), false);

    for (int sommet : graphe.sommets) {
        if (!visité[sommet]) {
            if (detecter_cycles_util(graphe, sommet, parent, visité)) {
                return true;
            }
        }
    }

    return false;
}

// Fonction utilitaire pour la détection de cycles
bool detecter_cycles_util(const Graphe& graphe, int sommet, vector<int>& parent, vector<bool>& visité) {
    visité[sommet] = true;

    for (int voisin : graphe.sommets[sommet]) {
        if (!visité[voisin]) {
            parent[voisin] = sommet;
            if (detecter_cycles_util(graphe, voisin, parent, visité)) {
                return true;
            }
        } else if (parent[sommet] != voisin) {
            return true;
        }
    }

    return false;
}

// Fonction pour trouver le plus court chemin entre deux sommets
int plus_court_chemin(const Graphe& graphe, int sommet1, int sommet2) {
    vector<int> distance(graphe.sommets.size(), INT_MAX);
    queue<int> file;
    
    file.push(sommet1);
    distance[sommet1] = 0;

    while (!file.empty()) {
        int sommet_courant = file.front();
        file.pop();

        for (int voisin : graphe.sommets[sommet_courant]) {
            if (distance[sommet_courant] + 1 < distance[voisin]) {
                distance[voisin] = distance[sommet_courant] + 1;
                file.push(voisin);
            }
        }
    }

    return distance[sommet2] == INT_MAX ? -1 : distance[sommet2];
}

// Fonction pour afficher le graphe
void afficher_graphe(const Graphe& graphe) {
    cout << "Sommets : ";
    for (int sommet : graphe.sommets) {
        cout << sommet << " ";
    }
    cout << endl;

    cout << "Arêtes : ";
    for (pair<int, int> arête : graphe.arêtes) {
        cout << "(" << arête.first << ", " << arête.second << ") ";
    }
    cout << endl;
}

// Fonction principale
int main() {
    // Créer un graphe
    Graphe graphe;
    ajouter_sommet(graphe, 1);
    ajouter_sommet(graphe, 2);
    ajouter_sommet(graphe, 3);
    ajouter_sommet(graphe, 4);
    ajouter_sommet(graphe, 5);
    ajouter_arete(graphe, 1, 2);
    ajouter_arete(graphe, 1, 3);
    ajouter_arete(graphe, 2, 4);
    ajouter_arete(graphe, 3, 4);
    ajouter_arete(graphe, 3, 5);
    ajouter_arete(graphe, 4, 5);
    afficher_graphe(graphe);
    cout << endl;

    // Effectuer un parcours en profondeur du graphe
    vector<bool> visité_profondeur(graphe.sommets.size(), false);
    cout << "Parcours en profondeur : ";
    parcours_profondeur(graphe, 1, visité_profondeur);
    cout << endl;

    // Effectuer un parcours en largeur du graphe
    cout << "Parcours en largeur : ";
    parcours_largeur(graphe, 1);
    cout << endl;

    // Détecter les cycles dans le graphe
    bool cycle = detecter_cycles(graphe);
    cout << (cycle ? "Le graphe contient des cycles." : "Le graphe ne contient pas de cycles.") << endl;

    // Trouver le plus court chemin entre deux sommets
    int distance = plus_court_chemin(graphe, 1, 5);
    if (distance >= 0) {
        cout << "Le plus court chemin entre les sommets 1 et 5 est de longueur " << distance << endl;
    } else {
        cout << "Il n'existe pas de chemin entre les sommets 1 et 5." << endl;
    }

    return 0;
}
```

**Explication du Code**

Ce code C++ illustre des concepts complexes de théorie des graphes, notamment :

* **Gestion des graphes** : Le code crée un graphe en tant que structure de données qui stocke des sommets et des arêtes.
* **Parcours du graphe** : Il implémente deux algorithmes de parcours de graphes : le parcours en profondeur et le parcours en largeur.
* **Détection de cycles** : Il utilise un algorithme de profondeur pour détecter les cycles dans le graphe.
* **Plus court chemin** : Il utilise un algorithme de parcours en largeur pour trouver le plus court chemin entre deux sommets.
* **Affichage du graphe** : Il affiche les sommets et les arêtes du graphe.

En plus de ces concepts, le code utilise d'autres fonctionnalités complexes :

* **Structures de données étendues** : Il utilise des map, set et vector pour représenter le graphe et ses attributs.
* **Techniques de programmation dynamique** : L'algorithme du plus court chemin utilise une technique de programmation dynamique pour stocker les distances minimales.
* **Gestion des exceptions** : L'algorithme du plus court chemin gère le cas où il n'y a pas de chemin entre deux sommets.
* **Générer un fichier exécutable** : Le code est conçu pour être compilé et exécuté comme un programme autonome.