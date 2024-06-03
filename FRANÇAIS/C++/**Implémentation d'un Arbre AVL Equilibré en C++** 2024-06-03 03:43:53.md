**Implémentation d'un Arbre AVL en C++**

**Classe Noeud**

```cpp
struct Noeud {
    int valeur;
    Noeud* gauche;
    Noeud* droit;
    int hauteur;
};
```

**Classe ArbreAVL**

```cpp
class ArbreAVL {
public:
    // Constructeur par défaut
    ArbreAVL() : _racine(nullptr) {}

    // Insère une valeur dans l'arbre
    void inserer(int valeur) {
        _racine = insererRec(_racine, valeur);
    }

    // Supprime une valeur de l'arbre
    void supprimer(int valeur) {
        _racine = supprimerRec(_racine, valeur);
    }

    // Calcule le facteur d'équilibre d'un nœud
    int facteurEquilibre(Noeud* noeud) const {
        if (noeud == nullptr) {
            return 0;
        }

        return hauteur(noeud->gauche) - hauteur(noeud->droit);
    }

    // Effectue une rotation à gauche sur un nœud
    Noeud* rotationGauche(Noeud* noeud) {
        Noeud* nouveauNoeud = noeud->droit;
        noeud->droit = nouveauNoeud->gauche;
        nouveauNoeud->gauche = noeud;

        mettreAJourHauteurs(noeud, nouveauNoeud);

        return nouveauNoeud;
    }

    // Effectue une rotation à droite sur un nœud
    Noeud* rotationDroite(Noeud* noeud) {
        Noeud* nouveauNoeud = noeud->gauche;
        noeud->gauche = nouveauNoeud->droit;
        nouveauNoeud->droit = noeud;

        mettreAJourHauteurs(noeud, nouveauNoeud);

        return nouveauNoeud;
    }

    // Effectue une double rotation à gauche sur un nœud
    Noeud* doubleRotationGauche(Noeud* noeud) {
        noeud->droit = rotationDroite(noeud->droit);
        return rotationGauche(noeud);
    }

    // Effectue une double rotation à droite sur un nœud
    Noeud* doubleRotationDroite(Noeud* noeud) {
        noeud->gauche = rotationGauche(noeud->gauche);
        return rotationDroite(noeud);
    }

    // Insère une valeur dans l'arbre de manière récursive
    Noeud* insererRec(Noeud* noeud, int valeur) {
        if (noeud == nullptr) {
            return new Noeud{valeur, nullptr, nullptr, 1};
        }

        if (valeur < noeud->valeur) {
            noeud->gauche = insererRec(noeud->gauche, valeur);
        } else if (valeur > noeud->valeur) {
            noeud->droit = insererRec(noeud->droit, valeur);
        } else {
            // La valeur existe déjà dans l'arbre
            return noeud;
        }

        mettreAJourHauteurs(noeud);

        // Vérifie le facteur d'équilibre et effectue les rotations nécessaires
        int fe = facteurEquilibre(noeud);
        if (fe > 1 && facteurEquilibre(noeud->gauche) >= 0) {
            return rotationDroite(noeud);
        } else if (fe < -1 && facteurEquilibre(noeud->droit) <= 0) {
            return rotationGauche(noeud);
        } else if (fe > 1 && facteurEquilibre(noeud->gauche) < 0) {
            return doubleRotationDroite(noeud);
        } else if (fe < -1 && facteurEquilibre(noeud->droit) > 0) {
            return doubleRotationGauche(noeud);
        }

        return noeud;
    }

    // Supprime une valeur de l'arbre de manière récursive
    Noeud* supprimerRec(Noeud* noeud, int valeur) {
        if (noeud == nullptr) {
            return nullptr;
        }

        if (valeur < noeud->valeur) {
            noeud->gauche = supprimerRec(noeud->gauche, valeur);
        } else if (valeur > noeud->valeur) {
            noeud->droit = supprimerRec(noeud->droit, valeur);
        } else {
            // Trouvé le nœud à supprimer

            // Cas 1 : Le nœud n'a pas d'enfants
            if (noeud->gauche == nullptr && noeud->droit == nullptr) {
                delete noeud;
                return nullptr;
            }

            // Cas 2 : Le nœud a un seul enfant
            else if (noeud->gauche == nullptr) {
                Noeud* temporaire = noeud;
                noeud = noeud->droit;
                delete temporaire;
            } else if (noeud->droit == nullptr) {
                Noeud* temporaire = noeud;
                noeud = noeud->gauche;
                delete temporaire;
            }

            // Cas 3 : Le nœud a deux enfants
            else {
                // Trouve le prédécesseur
                Noeud* prédécesseur = noeud->gauche;
                while (prédécesseur->droit != nullptr) {
                    prédécesseur = prédécesseur->droit;
                }

                // Copie la valeur du prédécesseur dans le nœud à supprimer
                noeud->valeur = prédécesseur->valeur;

                // Supprime le prédécesseur
                noeud->gauche = supprimerRec(noeud->gauche, prédécesseur->valeur);
            }
        }

        mettreAJourHauteurs(noeud);

        // Vérifie le facteur d'équilibre et effectue les rotations nécessaires
        int fe = facteurEquilibre(noeud);
        if (fe > 1 && facteurEquilibre(noeud->gauche) >= 0) {
            return rotationDroite(noeud);
        } else if (fe < -1 && facteurEquilibre(noeud->droit) <= 0) {
            return rotationGauche(noeud);
        } else if (fe > 1 && facteurEquilibre(noeud->gauche) < 0) {
            return doubleRotationDroite(noeud);
        } else if (fe < -1 && facteurEquilibre(noeud->droit) > 0) {
            return doubleRotationGauche(noeud);
        }

        return noeud;
    }

    // Calcule la hauteur d'un nœud
    int hauteur(Noeud* noeud) const {
        if (noeud == nullptr) {
            return 0;
        }

        return noeud->hauteur;
    }

    // Met à jour la hauteur des nœuds concernés après une insertion ou une suppression
    void mettreAJourHauteurs(Noeud* noeud, Noeud* parent = nullptr) {
        if (noeud == nullptr) {
            return;
        }

        noeud->hauteur = 1 + max(hauteur(noeud->gauche), hauteur(noeud->droit));

        if (parent != nullptr) {
            mettreAJourHauteurs(parent, parent->parent);
        }
    }

private:
    Noeud* _racine;
};
```

**Exemple d'utilisation**

```cpp
#include <iostream>

int main() {
    ArbreAVL arbre;

    arbre.inserer(10);
    arbre.inserer(5);
    arbre.inserer(15);
    arbre.inserer(3);
    arbre.inserer(7);
    arbre.inserer(12);
    arbre.inserer(18);

    std::cout << "Arbre AVL après insertion :" << std::endl;
    arbre.afficherArbre();

    arbre.supprimer(10);

    std::cout << "Arbre AVL après suppression :" << std::endl;
    arbre.afficherArbre();

    return 0;
}
```

**Fonctionnalités du code**

* Implémentation d'un arbre AVL auto-équilibré en C++
* Insère et supprime des valeurs dans l'arbre tout en maintenant la propriété AVL
* Utilise des rotations à gauche, à droite et des doubles rotations pour équilibrer l'arbre
* Calcule la hauteur et les facteurs d'équilibre des nœuds
* Met à jour les hauteurs des nœuds concernés après des insertions ou des suppressions