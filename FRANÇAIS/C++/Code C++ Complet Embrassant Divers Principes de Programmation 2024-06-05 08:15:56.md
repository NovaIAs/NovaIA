Voici un code C++ complexe et complet qui couvre divers aspects de la programmation :

```cpp
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <functional>
#include <thread>
#include <mutex>

using namespace std;

// Fonction récursive de Fibonacci
int fibonacci(int n) {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

// Fonction pour trier un tableau d'entiers
void triFusion(int* tableau, int debut, int fin) {
    if (fin - debut <= 1) {
        return;
    }
    int milieu = (debut + fin) / 2;
    triFusion(tableau, debut, milieu);
    triFusion(tableau, milieu, fin);
    // Fusionner les deux sous-tableaux triés
    int* tableauFusionne = new int[fin - debut];
    int i1 = debut;
    int i2 = milieu;
    int i = 0;
    while (i1 < milieu && i2 < fin) {
        if (tableau[i1] < tableau[i2]) {
            tableauFusionne[i] = tableau[i1];
            i1++;
        } else {
            tableauFusionne[i] = tableau[i2];
            i2++;
        }
        i++;
    }
    while (i1 < milieu) {
        tableauFusionne[i] = tableau[i1];
        i1++;
        i++;
    }
    while (i2 < fin) {
        tableauFusionne[i] = tableau[i2];
        i2++;
        i++;
    }
    for (int i = debut; i < fin; i++) {
        tableau[i] = tableauFusionne[i - debut];
    }
    delete[] tableauFusionne;
}

// Fonction pour trouver le plus grand élément d'un tableau d'entiers
int trouverMax(int* tableau, int taille) {
    int max = tableau[0];
    for (int i = 1; i < taille; i++) {
        if (tableau[i] > max) {
            max = tableau[i];
        }
    }
    return max;
}

// Fonction pour inverser une chaîne de caractères
string inverserChaine(string chaine) {
    string chaineInversee;
    for (int i = chaine.length() - 1; i >= 0; i--) {
        chaineInversee += chaine[i];
    }
    return chaineInversee;
}

// Fonction pour calculer le produit scalaire de deux vecteurs
double produitScalaire(vector<double> vecteur1, vector<double> vecteur2) {
    if (vecteur1.size() != vecteur2.size()) {
        throw invalid_argument("Les vecteurs doivent avoir la même taille.");
    }
    double produit = 0;
    for (int i = 0; i < vecteur1.size(); i++) {
        produit += vecteur1[i] * vecteur2[i];
    }
    return produit;
}

// Fonction pour trouver la valeur maximale d'une map
pair<string, int> trouverMaxMap(map<string, int> map) {
    pair<string, int> max;
    for (auto it = map.begin(); it != map.end(); it++) {
        if (it->second > max.second) {
            max = *it;
        }
    }
    return max;
}

// Fonction pour exécuter une tâche en parallèle
void tacheParallele(function<void()> tache) {
    static mutex m;
    thread t([&tache] () {
        lock_guard<mutex> lock(m);
        tache();
    });
    t.detach();
}

int main() {
    // Exemple utilisant la fonction de Fibonacci
    cout << "Fibonacci de 10 : " << fibonacci(10) << endl;

    // Exemple utilisant l'algorithme de tri fusion
    int tableau[] = {5, 3, 1, 2, 4};
    int tailleTableau = sizeof(tableau) / sizeof(tableau[0]);
    triFusion(tableau, 0, tailleTableau);
    cout << "Tableau trié : ";
    for (int i = 0; i < tailleTableau; i++) {
        cout << tableau[i] << " ";
    }
    cout << endl;

    // Exemple utilisant la fonction pour trouver le plus grand élément
    int maxTableau = trouverMax(tableau, tailleTableau);
    cout << "Plus grand élément du tableau : " << maxTableau << endl;

    // Exemple utilisant la fonction pour inverser une chaîne
    string chaine = "Bonjour";
    string chaineInversee = inverserChaine(chaine);
    cout << "Chaîne inversée : " << chaineInversee << endl;

    // Exemple utilisant la fonction pour calculer le produit scalaire
    vector<double> vecteur1 = {1, 2, 3};
    vector<double> vecteur2 = {4, 5, 6};
    double produitScalaireVecteurs = produitScalaire(vecteur1, vecteur2);
    cout << "Produit scalaire des vecteurs : " << produitScalaireVecteurs << endl;

    // Exemple utilisant la fonction pour trouver la valeur maximale d'une map
    map<string, int> map = {{"Clé 1", 10}, {"Clé 2", 5}, {"Clé 3", 15}};
    pair<string, int> maxMap = trouverMaxMap(map);
    cout << "La clé avec la valeur maximale dans la map : " << maxMap.first << endl;

    // Exemple utilisant la fonction pour exécuter une tâche en parallèle
    tacheParallele([] () {
        cout << "Tâche exécutée en parallèle" << endl;
    });

    return 0;
}
```

Ce code couvre les aspects suivants :

* **Récursivité (fonction de Fibonacci)**
* **Tri (tri fusion)**
* **Recherche (trouver le plus grand élément)**
* **Manipulation de chaînes (inverser une chaîne)**
* **Algèbre linéaire (calcul du produit scalaire)**
* **Structures de données (map)**
* **Programmation parallèle (exécuter une tâche en parallèle)**

Il illustre également l'utilisation de fonctions, de tableaux, de vecteurs, de maps et de threads.