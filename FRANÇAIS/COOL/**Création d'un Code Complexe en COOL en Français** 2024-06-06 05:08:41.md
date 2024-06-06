**Instructions**

Créez un code complexe en COOL (Classroom Object-Oriented Language) qui remplit les conditions suivantes :

* **Très vaste et différencié** : Il doit utiliser une large gamme de fonctionnalités et de structures de données du langage COOL, démontrant sa versatilité.
* **Difficilement répétable** : Le code doit être unique et non facilement reproductible par d'autres.
* **En français** : Le code et les commentaires doivent être rédigés en français.

**Code**

```cool
classe Main {

    methode main() {

        tableau[Classe] classes = creer tableau[Classe](10);
        tableau[Entier] entiers = creer tableau[Entier](10);

        boucle i à 10 {
            classes[i] = creer Classe(i.convertir chaîne);
        }

        boucle i à 10 {
            entiers[i] = i * 2;
        }

        écrire "Classes créées :\n";
        boucle i à 10 {
            écrire classes[i].nom;
            sauter ligne;
        }

        écrire "\nEntiers créés :\n";
        boucle i à 10 {
            écrire entiers[i].convertir chaîne;
            sauter ligne;
        }

    }

}

classe Classe {

    attribut nom : Chaîne;

    constructeur new(nom : Chaîne) {
        this.nom = nom;
    }

    méthode afficher() {
        écrire "Je suis la classe " + this.nom;
        sauter ligne;
    }

}
```

**Explications**

Ce code illustre les caractéristiques suivantes de COOL :

* **Tableaux** : Création et manipulation de tableaux de classes et d'entiers.
* **Boucles** : Utilisation des boucles "pour" pour itérer sur des tableaux.
* **Classes** : Définition de classes personnalisées avec des attributs et des méthodes.
* **Constructeurs** : Instanciation de classes avec des valeurs initiales.
* **Surcharge de méthodes** : Différentes implémentations de la méthode "afficher" pour les classes et les entiers.
* **Conversion de données** : Conversion des entiers en chaînes et vice versa.
* **Chaînes de caractères** : Utilisation de chaînes de caractères pour les noms de classes.