**Système de gestion de bibliothèque en C++ avec fonctions avancées**

Ce code implémente un système de gestion de bibliothèque complet avec des fonctionnalités avancées telles que la recherche, l'emprunt, le retour et la gestion des membres.

**En-tête principal :**

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <fstream>
#include <sstream>
```

**Classe Livre :**

```cpp
class Livre {
public:
    int id;
    std::string titre;
    std::string auteur;
    int quantite;
    bool emprunte;

    Livre(int id, std::string titre, std::string auteur, int quantite) :
        id(id), titre(titre), auteur(auteur), quantite(quantite), emprunte(false) {}
};
```

**Classe Membre :**

```cpp
class Membre {
public:
    int id;
    std::string nom;
    std::string prenom;
    std::map<int, Livre*> livresEmpruntes;

    Membre(int id, std::string nom, std::string prenom) :
        id(id), nom(nom), prenom(prenom) {}

    void emprunterLivre(Livre* livre) {
        livresEmpruntes[livre->id] = livre;
        livre->quantite--;
        livre->emprunte = true;
    }

    void rendreLivre(Livre* livre) {
        livresEmpruntes.erase(livre->id);
        livre->quantite++;
        livre->emprunte = false;
    }
};
```

**Fonctions principales :**

```cpp
// Créer un livre
Livre* creerLivre(int id, std::string titre, std::string auteur, int quantite) {
    return new Livre(id, titre, auteur, quantite);
}

// Créer un membre
Membre* creerMembre(int id, std::string nom, std::string prenom) {
    return new Membre(id, nom, prenom);
}

// Rechercher un livre par titre
std::vector<Livre*> rechercherLivreParTitre(std::string titre) {
    std::vector<Livre*> resultats;
    for (auto livre : livres) {
        if (livre->titre.find(titre) != std::string::npos) {
            resultats.push_back(livre);
        }
    }
    return resultats;
}

// Emprunter un livre
void emprunterLivre(Membre* membre, Livre* livre) {
    membre->emprunterLivre(livre);
    std::cout << "Le livre " << livre->titre << " a été emprunté par " << membre->nom << " " << membre->prenom << std::endl;
}

// Rendre un livre
void rendreLivre(Membre* membre, Livre* livre) {
    membre->rendreLivre(livre);
    std::cout << "Le livre " << livre->titre << " a été rendu par " << membre->nom << " " << membre->prenom << std::endl;
}
```

**Programme principal :**

```cpp
int main() {
    // Initialiser la bibliothèque
    std::vector<Livre*> livres;
    std::vector<Membre*> membres;

    // Charger les livres depuis un fichier
    std::ifstream fichier("livres.txt");
    std::string ligne;
    while (std::getline(fichier, ligne)) {
        std::stringstream ss(ligne);
        int id;
        std::string titre, auteur;
        int quantite;
        ss >> id >> titre >> auteur >> quantite;
        livres.push_back(creerLivre(id, titre, auteur, quantite));
    }
    fichier.close();

    // Charger les membres depuis un fichier
    std::ifstream fichierMembres("membres.txt");
    while (std::getline(fichierMembres, ligne)) {
        std::stringstream ss(ligne);
        int id;
        std::string nom, prenom;
        ss >> id >> nom >> prenom;
        membres.push_back(creerMembre(id, nom, prenom));
    }
    fichierMembres.close();

    // Interface utilisateur
    int choix;
    while (choix != 0) {
        std::cout << "Bienvenue dans le système de gestion de bibliothèque" << std::endl;
        std::cout << "1. Rechercher un livre" << std::endl;
        std::cout << "2. Emprunter un livre" << std::endl;
        std::cout << "3. Rendre un livre" << std::endl;
        std::cout << "0. Quitter" << std::endl;
        std::cout << "Entrez votre choix : ";
        std::cin >> choix;

        switch (choix) {
            case 1:
                std::string titreRecherche;
                std::cout << "Entrez le titre du livre à rechercher : ";
                std::cin >> titreRecherche;
                std::vector<Livre*> resultats = rechercherLivreParTitre(titreRecherche);
                if (resultats.size() == 0) {
                    std::cout << "Aucun livre trouvé avec ce titre" << std::endl;
                } else {
                    std::cout << "Livres trouvés :" << std::endl;
                    for (auto livre : resultats) {
                        std::cout << " - " << livre->titre << " (" << livre->auteur << ")" << std::endl;
                    }
                }
                break;
            case 2:
                int idLivre;
                int idMembre;
                std::cout << "Entrez l'ID du livre à emprunter : ";
                std::cin >> idLivre;
                std::cout << "Entrez l'ID du membre qui emprunte : ";
                std::cin >> idMembre;
                Livre* livre = nullptr;
                Membre* membre = nullptr;
                for (auto l : livres) {
                    if (l->id == idLivre) {
                        livre = l;
                        break;
                    }
                }
                for (auto m : membres) {
                    if (m->id == idMembre) {
                        membre = m;
                        break;
                    }
                }
                if (livre == nullptr) {
                    std::cout << "Livre introuvable" << std::endl;
                } else if (membre == nullptr) {
                    std::cout << "Membre introuvable" << std::endl;
                } else if (livre->quantite == 0) {
                    std::cout << "Livre non disponible" << std::endl;
                } else {
                    emprunterLivre(membre, livre);
                }
                break;
            case 3:
                int idLivreRendu;
                int idMembreRendu;
                std::cout << "Entrez l'ID du livre à rendre : ";
                std::cin >> idLivreRendu;
                std::cout << "Entrez l'ID du membre qui rend : ";
                std::cin >> idMembreRendu;
                Livre* livreRendu = nullptr;
                Membre* membreRendu = nullptr;
                for (auto l : livres) {
                    if (l->id == idLivreRendu) {
                        livreRendu = l;
                        break;
                    }
                }
                for (auto m : membres) {
                    if (m->id == idMembreRendu) {
                        membreRendu = m;
                        break;
                    }
                }
                if (livreRendu == nullptr) {
                    std::cout << "Livre introuvable" << std::endl;
                } else if (membreRendu == nullptr) {
                    std::cout << "Membre introuvable" << std::endl;
                } else {
                    rendreLivre(membreRendu, livreRendu);
                }
                break;
            case 0:
                std::cout << "Aurevoir !" << std::endl;
                break;
            default:
                std::cout << "Choix invalide" << std::endl;
                break;
        }
    }

    return 0;
}
```

**Explication :**

* Le programme crée des instances de `Livre` et de `Membre` pour représenter les livres et les membres de la bibliothèque.
* Les fonctions principales permettent de rechercher, emprunter et rendre des livres, ainsi que de gérer les membres.
* Le programme principal charge les données de livres et de membres à partir de fichiers texte et fournit une interface utilisateur pour interagir avec le système.