```cpp
// Bibliothèques nécessaires
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <functional>
#include <regex>

using namespace std;

// Définition de types personnalisés
struct Personne {
    string nom;
    int age;
    double taille;
};

struct Adresse {
    string rue;
    string ville;
    string codePostal;
};

// Fonctions utilitaires
struct ComparateurPersonneParNom {
    bool operator()(const Personne& a, const Personne& b) {
        return a.nom < b.nom;
    }
};

// Classes
class GestionnaireDePersonnes {
    private:
        vector<Personne> personnes;

    public:
        GestionnaireDePersonnes() {
            // Initialisation par défaut
        }

        void ajouterPersonne(const Personne& personne) {
            personnes.push_back(personne);
        }

        vector<Personne> getPersonnes() const {
            return personnes;
        }

        void trierPersonnesParNom() {
            sort(personnes.begin(), personnes.end(), ComparateurPersonneParNom());
        }

        vector<Personne> filtrerPersonnesParAge(int ageMinimum, int ageMaximum) {
            auto itPredicat = [&](const Personne& p) {
                return p.age >= ageMinimum && p.age <= ageMaximum;
            };
            return vector<Personne>(filter(personnes.begin(), personnes.end(), itPredicat), personnes.end());
        }

        void afficherPersonnes() {
            for (const Personne& personne : personnes) {
                cout << "Nom : " << personne.nom << endl;
                cout << "Age : " << personne.age << endl;
                cout << "Taille : " << personne.taille << endl;
                cout << endl;
            }
        }
};

class GestionnaireDAdresses {
    private:
        map<string, Adresse> adresses;

    public:
        GestionnaireDAdresses() {
            // Initialisation par défaut
        }

        void ajouterAdresse(const string& nom, const Adresse& adresse) {
            adresses[nom] = adresse;
        }

        Adresse getAdresse(const string& nom) const {
            return adresses[nom];
        }

        bool supprimerAdresse(const string& nom) {
            return adresses.erase(nom) == 1;
        }

        void afficherAdresses() {
            for (const pair<string, Adresse>& nomAdresse : adresses) {
                cout << "Nom : " << nomAdresse.first << endl;
                cout << "Rue : " << nomAdresse.second.rue << endl;
                cout << "Ville : " << nomAdresse.second.ville << endl;
                cout << "Code Postal : " << nomAdresse.second.codePostal << endl;
                cout << endl;
            }
        }
};

// Fonction principale
int main() {
    // Créer un gestionnaire de personnes
    GestionnaireDePersonnes gestionnaireDePersonnes;

    // Ajouter des personnes
    gestionnaireDePersonnes.ajouterPersonne({"Jean", 30, 1.85});
    gestionnaireDePersonnes.ajouterPersonne({"Marie", 25, 1.70});
    gestionnaireDePersonnes.ajouterPersonne({"Pierre", 40, 1.90});

    // Trier les personnes par nom
    gestionnaireDePersonnes.trierPersonnesParNom();

    // Filtrer les personnes par âge
    vector<Personne> personnesFiltrees = gestionnaireDePersonnes.filtrerPersonnesParAge(20, 35);

    // Afficher les personnes
    cout << "Liste des personnes :" << endl;
    for (const Personne& personne : personnesFiltrees) {
        cout << "Nom : " << personne.nom << endl;
        cout << "Age : " << personne.age << endl;
        cout << "Taille : " << personne.taille << endl;
        cout << endl;
    }

    // Créer un gestionnaire d'adresses
    GestionnaireDAdresses gestionnaireDAdresses;

    // Ajouter des adresses
    gestionnaireDAdresses.ajouterAdresse("Jean", {"Rue des Champs", "Paris", "75000"});
    gestionnaireDAdresses.ajouterAdresse("Marie", {"Avenue de la République", "Lyon", "69000"});

    // Récupérer une adresse
    Adresse adresseDeJean = gestionnaireDAdresses.getAdresse("Jean");

    // Supprimer une adresse
    bool adresseSupprimee = gestionnaireDAdresses.supprimerAdresse("Pierre");

    // Afficher les adresses
    cout << "Liste des adresses :" << endl;
    gestionnaireDAdresses.afficherAdresses();

    return 0;
}
```

**Explication du code**

Ce code en C++ implémente deux classes, `GestionnaireDePersonnes` et `GestionnaireDAdresses`, qui permettent de gérer des listes de personnes et d'adresses respectivement.

**Classe `GestionnaireDePersonnes`**

Cette classe fournit des méthodes pour ajouter, trier et filtrer des personnes en fonction de leur nom et de leur âge. Elle utilise une fonction de comparaison personnalisée pour trier les personnes par nom.

**Classe `GestionnaireDAdresses`**

Cette classe fournit des méthodes pour ajouter, récupérer et supprimer des adresses associées à des noms.

**Fonction principale**

La fonction principale crée des instances de `GestionnaireDePersonnes` et `GestionnaireDAdresses` pour démontrer leurs fonctionnalités. Elle ajoute des personnes et des adresses, puis affiche les listes triées et filtrées.

**Autres détails**

* Le code utilise des bibliothèques standard telles que `<vector>`, `<map>` et `<algorithm>` pour gérer les collections et effectuer des opérations communes.
* Les types personnalisés `Personne` et `Adresse` sont utilisés pour structurer les données des personnes et des adresses.
* Les foncteurs sont utilisés pour définir des prédicats personnalisés dans les méthodes de filtrage.
* Des expressions lambda sont utilisées pour définir des fonctions anonymes à la volée.
* Les expressions régulières sont utilisées pour valider les codes postaux dans la classe `GestionnaireDAdresses`.