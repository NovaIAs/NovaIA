**Code**

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <functional>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>

using namespace std;

struct ArbreBinaire {
    int valeur;
    unique_ptr<ArbreBinaire> gauche;
    unique_ptr<ArbreBinaire> droite;

    ArbreBinaire(int valeur, unique_ptr<ArbreBinaire> gauche, unique_ptr<ArbreBinaire> droite)
        : valeur(valeur), gauche(move(gauche)), droite(move(droite)) {}

    ArbreBinaire(int valeur)
        : valeur(valeur), gauche(nullptr), droite(nullptr) {}

    ~ArbreBinaire() {
        gauche.reset();
        droite.reset();
    }

    bool estVide() const {
        return gauche == nullptr && droite == nullptr;
    }

    bool estFeuille() const {
        return gauche == nullptr && droite == nullptr;
    }

    int hauteur() const {
        if (estVide()) {
            return 0;
        } else if (estFeuille()) {
            return 1;
        } else {
            return max(gauche->hauteur(), droite->hauteur()) + 1;
        }
    }

    void insérer(int valeur) {
        if (estVide()) {
            *this = ArbreBinaire(valeur);
        } else if (valeur < this->valeur) {
            if (gauche == nullptr) {
                gauche = make_unique<ArbreBinaire>(valeur);
            } else {
                gauche->insérer(valeur);
            }
        } else if (valeur > this->valeur) {
            if (droite == nullptr) {
                droite = make_unique<ArbreBinaire>(valeur);
            } else {
                droite->insérer(valeur);
            }
        }
    }

    bool rechercher(int valeur) const {
        if (estVide()) {
            return false;
        } else if (this->valeur == valeur) {
            return true;
        } else if (valeur < this->valeur) {
            if (gauche == nullptr) {
                return false;
            } else {
                return gauche->rechercher(valeur);
            }
        } else if (valeur > this->valeur) {
            if (droite == nullptr) {
                return false;
            } else {
                return droite->rechercher(valeur);
            }
        }
    }

    void afficher() const {
        if (estVide()) {
            return;
        }

        cout << this->valeur << " ";

        if (gauche != nullptr) {
            gauche->afficher();
        }

        if (droite != nullptr) {
            droite->afficher();
        }
    }
};

struct Graphe {
    vector<vector<int>> sommetsAdjacents;
    vector<bool> sommetsVisités;

    Graphe(int nombreDeSommets)
        : sommetsAdjacents(nombreDeSommets), sommetsVisités(nombreDeSommets, false) {}

    void ajouterArête(int sommet1, int sommet2) {
        sommetsAdjacents[sommet1].push_back(sommet2);
        sommetsAdjacents[sommet2].push_back(sommet1);
    }

    void parcoursLargeur(int sommetDépart) {
        queue<int> file;
        sommetsVisités[sommetDépart] = true;
        file.push(sommetDépart);

        while (!file.empty()) {
            int sommetActuel = file.front();
            file.pop();

            cout << sommetActuel << " ";

            for (int sommetAdjacent : sommetsAdjacents[sommetActuel]) {
                if (!sommetsVisités[sommetAdjacent]) {
                    sommetsVisités[sommetAdjacent] = true;
                    file.push(sommetAdjacent);
                }
            }
        }
    }

    void parcoursProfondeur(int sommetDépart) {
        pile<int> pile;
        sommetsVisités[sommetDépart] = true;
        pile.push(sommetDépart);

        while (!pile.empty()) {
            int sommetActuel = pile.top();
            pile.pop();

            cout << sommetActuel << " ";

            for (int sommetAdjacent : sommetsAdjacents[sommetActuel]) {
                if (!sommetsVisités[sommetAdjacent]) {
                    sommetsVisités[sommetAdjacent] = true;
                    pile.push(sommetAdjacent);
                }
            }
        }
    }

    void plusCourtChemin(int sommetDépart, int sommetDestination) {
        map<int, int> distances;
        set<int> sommetsVisités;

        distances[sommetDépart] = 0;
        sommetsVisités.insert(sommetDépart);

        while (!sommetsVisités.count(sommetDestination)) {
            int sommetActuel = *min_element(sommetsVisités.begin(), sommetsVisités.end(),
                [&distances](int a, int b) { return distances[a] < distances[b]; });

            for (int sommetAdjacent : sommetsAdjacents[sommetActuel]) {
                if (!sommetsVisités.count(sommetAdjacent)) {
                    distances[sommetAdjacent] = distances[sommetActuel] + 1;
                    sommetsVisités.insert(sommetAdjacent);
                }
            }
        }

        cout << distances[sommetDestination] << endl;
    }
};

class TableHachage {
public:
    vector<list<pair<string, string>>> table;
    int taille;

    TableHachage(int taille)
        : table(taille), taille(taille) {}

    int fonctionHachage(const string& clé) {
        int somme = 0;

        for (char c : clé) {
            somme += c;
        }

        return somme % taille;
    }

    void insérer(const string& clé, const string& valeur) {
        int indexHachage = fonctionHachage(clé);

        table[indexHachage].push_back({clé, valeur});
    }

    bool rechercher(const string& clé) {
        int indexHachage = fonctionHachage(clé);

        for (const auto& [cléTable, valeurTable] : table[indexHachage]) {
            if (cléTable == clé) {
                return true;
            }
        }

        return false;
    }

    void supprimer(const string& clé) {
        int indexHachage = fonctionHachage(clé);

        for (auto it = table[indexHachage].begin(); it != table[indexHachage].end(); ++it) {
            if (it->first == clé) {
                table[indexHachage].erase(it);
                return;
            }
        }
    }

    void afficher() {
        for (int i = 0; i < taille; ++i) {
            cout << "Index " << i << ": ";

            for (const auto& [clé, valeur] : table[i]) {
                cout << "(" << clé << ", " << valeur << ") ";
            }

            cout << endl;
        }
    }
};

int main() {
    // Arbre binaire
    unique_ptr<ArbreBinaire> arbre = make_unique<ArbreBinaire>(10);
    arbre->insérer(5);
    arbre->insérer(15);
    arbre->insérer(3);
    arbre->insérer(7);
    arbre->insérer(12);
    arbre->insérer(20);
    cout << "Arbre binaire : "; arbre->afficher(); cout << endl;
    cout << "Hauteur de l'arbre : " << arbre->hauteur() << endl;
    cout << "L'arbre contient le nombre 7 : " << (arbre->rechercher(7) ? "true" : "false") << endl;
    cout << endl;

    // Graphe
    Graphe graphe(6);
    graphe.ajouterArête(0, 1);
    graphe.ajouterArête(0, 2);
    graphe.ajouterArête(1, 2);
    graphe.ajouterArête(1, 3);
    graphe.ajouterArête(2, 4);
    graphe.ajouterArête(3, 4);
    graphe.ajouterArête(3, 5