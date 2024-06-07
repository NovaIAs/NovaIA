**Programme Cool**

```cool
Classe Principale {
    Méthode main() {
        Entier i = 0;
        Entier j = 0;

        Boucle (i < 5) {
            Affiche ("i vaut " + i);
            j = i + 1;
            Si (j == 3) {
                Continue;
            }
            Affiche ("j vaut " + j);
            i = i + 1;
        }
        Affiche ("Fin de la boucle");

        ListeEntiers maListe = [1, 2, 3, 4, 5];
        Boucle (maListe.EstVide() == Faux) {
            Entier element = maListe.Tete();
            Affiche ("Élément de la liste : " + element);
            maListe.Queue();
        }

        Si (Entier.EstPair(22)) {
            Affiche ("22 est pair");
        } Sinon {
            Affiche ("22 est impair");
        }

        Classe Voiture {
            Attribut modele : Chaîne;
            Attribut couleur : Chaîne;
            Attribut vitesse : Entier;

            Constructeur (modele : Chaîne, couleur : Chaîne, vitesse : Entier) {
                modele := modele;
                couleur := couleur;
                vitesse := vitesse;
            }

            Méthode avancer(distance : Entier) {
                vitesse := vitesse + distance;
                Affiche ("La voiture " + modele + " avance de " + distance + " km/h");
            }
        }

        Voiture maVoiture = Voiture.nouveau("Tesla", "Noir", 120);
        maVoiture.avancer(20);
    }
}
```

**Explications**

Ce code Cool est un programme complexe qui utilise diverses fonctionnalités du langage, notamment :

* **Boucle (While)** : Une boucle while qui s'exécute tant qu'une condition est vraie.
* **Boucle (For)** : Une boucle for qui parcourt une collection ou un tableau.
* **Instruction Si (If)** : Une instruction conditionnelle qui exécute du code si une condition est vraie.
* **Instruction Sinon (Else)** : Une instruction conditionnelle qui exécute du code si une condition est fausse.
* **Instruction Continue** : Une instruction qui saute le reste de l'itération actuelle d'une boucle.
* **Listes** : Des collections ordonnées d'éléments qui peuvent être parcourues et modifiées.
* **Classes** : Des définitions de types qui encapsulent des données et un comportement.
* **Constructeurs** : Des méthodes spéciales qui initialisent les instances de classe.
* **Méthodes** : Des fonctions associées à des instances de classe.