```cool

-- Classe abstraite représentant un élément de l'interface graphique
classe Composant {
    -- Méthode pour dessiner l'élément sur l'écran
    méthode dessiner();
    -- Méthode pour gérer les événements utilisateur (par exemple, clics)
    méthode gérerÉvénement(événement);
}

-- Classe représentant un bouton
classe Bouton hérité de Composant {
    -- Constructeur
    initialiser(texte : String, position : Vecteur);

    -- Redéfinition de la méthode dessiner pour dessiner un bouton
    méthode dessiner() {
        -- Code pour dessiner le bouton
    }

    -- Redéfinition de la méthode gérerÉvénement pour gérer les clics sur le bouton
    méthode gérerÉvénement(événement) {
        si (événement est un clic) {
            -- Code pour exécuter l'action associée au bouton
        }
    }
}

-- Classe représentant une zone de texte
classe ZoneTexte hérité de Composant {
    -- Constructeur
    initialiser(texte : String, position : Vecteur);

    -- Redéfinition de la méthode dessiner pour dessiner une zone de texte
    méthode dessiner() {
        -- Code pour dessiner la zone de texte
    }

    -- Méthode pour obtenir le texte contenu dans la zone de texte
    méthode obtenirTexte() : String;

    -- Méthode pour définir le texte contenu dans la zone de texte
    méthode définirTexte(texte : String);
}

-- Classe représentant une fenêtre
classe Fenêtre {
    -- Constructeur
    initialiser(titre : String, taille : Vecteur);

    -- Méthode pour ajouter un composant à la fenêtre
    méthode ajouterComposant(composant : Composant);

    -- Méthode pour afficher la fenêtre à l'écran
    méthode afficher();

    -- Méthode pour gérer les événements utilisateur (par exemple, fermeture de la fenêtre)
    méthode gérerÉvénement(événement);
}

-- Classe principale
classe Main {
    -- Méthode principale
    méthode principal() {
        -- Création d'une fenêtre
        fenêtre := Fenêtre("Ma Fenêtre", Vecteur(400, 300));

        -- Ajout de composants à la fenêtre
        bouton := Bouton("Cliquez ici", Vecteur(100, 100));
        zoneTexte := ZoneTexte("Entrez votre nom", Vecteur(100, 200));
        fenêtre.ajouterComposant(bouton);
        fenêtre.ajouterComposant(zoneTexte);

        -- Affichage de la fenêtre
        fenêtre.afficher();
    }
}

```

**Explication du code :**

Ce code définit une hiérarchie de classes représentant les éléments d'une interface graphique (GUI) dans le langage de programmation COOL.

* **Classe Composant** : classe abstraite définissant les méthodes de base pour les composants d'interface graphique, telles que le dessin et la gestion des événements.
* **Classe Bouton** : classe dérivée de Composant représentant un bouton d'interface graphique, avec des méthodes pour dessiner le bouton et gérer les événements de clic.
* **Classe ZoneTexte** : classe dérivée de Composant représentant une zone de texte d'interface graphique, avec des méthodes pour dessiner la zone de texte, obtenir et définir son texte.
* **Classe Fenêtre** : classe représentant une fenêtre d'interface graphique, avec des méthodes pour ajouter des composants, afficher la fenêtre et gérer les événements utilisateur.
* **Classe Main** : classe principale contenant la méthode principale, qui crée une fenêtre, ajoute des composants et l'affiche à l'écran.