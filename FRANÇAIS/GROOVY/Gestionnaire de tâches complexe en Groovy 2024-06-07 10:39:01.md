```groovy
//Créez une classe complexe nommée GestionnaireTâches
class GestionnaireTâches {

    //Champ pour stocker la liste des tâches
    private List<Tâche> tâches = []

    //Constructeur
    GestionnaireTâches() {
        //Initialiser la liste des tâches
        tâches << [
            new Tâche(id: 1, titre: "Tâche 1", description: "Description de la tâche 1", état: Tâche.Etat.À_FAIRE),
            new Tâche(id: 2, titre: "Tâche 2", description: "Description de la tâche 2", état: Tâche.Etat.EN_COURS),
            new Tâche(id: 3, titre: "Tâche 3", description: "Description de la tâche 3", état: Tâche.Etat.TERMINÉ)
        ]
    }

    //Méthode pour ajouter une nouvelle tâche
    void ajouterTâche(Tâche tâche) {
        tâches << tâche
    }

    //Méthode pour modifier une tâche existante
    void modifierTâche(Long id, Map<String, Object> misesÀJour) {
        //Récupérer la tâche à modifier
        Tâche tâche = tâches.find { it.id == id }

        //Mettre à jour les propriétés de la tâche
        misesÀJour.each { propriété, valeur -> tâche."$propriété" = valeur }
    }

    //Méthode pour supprimer une tâche existante
    void supprimerTâche(Long id) {
        //Supprimer la tâche de la liste
        tâches.removeIf { it.id == id }
    }

    //Méthode pour récupérer toutes les tâches
    List<Tâche> récupérerToutesLesTâches() {
        return tâches
    }

    //Méthode pour récupérer une tâche par son ID
    Tâche récupérerTâcheParId(Long id) {
        return tâches.find { it.id == id }
    }

    //Méthode pour filtrer les tâches par leur état
    List<Tâche> filtrerTâchesParÉtat(Tâche.Etat état) {
        return tâches.findAll { it.état == état }
    }

    //Classe interne représentant une tâche
    static class Tâche {

        //Enumération des états possibles d'une tâche
        enum Etat {
            À_FAIRE,
            EN_COURS,
            TERMINÉ
        }

        //Propriétés de la tâche
        Long id
        String titre
        String description
        Etat état

        //Constructeur
        Tâche(Long id, String titre, String description, Etat état) {
            this.id = id
            this.titre = titre
            this.description = description
            this.état = état
        }
    }
}

//Instancier le gestionnaire de tâches
def gestionnaireTâches = new GestionnaireTâches()

//Ajouter une nouvelle tâche
gestionnaireTâches.ajouterTâche(
    new GestionnaireTâches.Tâche(id: 4, titre: "Tâche 4", description: "Description de la tâche 4", état: Tâche.Etat.À_FAIRE)
)

//Modifier une tâche existante
gestionnaireTâches.modifierTâche(2, [titre: "Tâche 2 modifiée", description: "Description modifiée de la tâche 2"])

//Supprimer une tâche existante
gestionnaireTâches.supprimerTâche(3)

//Récupérer toutes les tâches
def toutesLesTâches = gestionnaireTâches.récupérerToutesLesTâches()

//Filtrer les tâches par leur état
def tâchesEnCours = gestionnaireTâches.filtrerTâchesParÉtat(Tâche.Etat.EN_COURS)
```

**Explication du code :**

Ce code implémente un gestionnaire de tâches complexe en Groovy. Il utilise une classe interne pour représenter les tâches et fournit diverses méthodes pour gérer les tâches, telles que l'ajout, la modification, la suppression, la récupération et le filtrage.

**Voici une explication détaillée du code :**

* La classe `GestionnaireTâches` contient une liste de tâches, représentées par la classe interne `Tâche`.
* L'énumération `Tâche.Etat` définit les différents états possibles d'une tâche (`À_FAIRE`, `EN_COURS` et `TERMINÉ`).
* La classe `Tâche` possède des propriétés pour l'ID, le titre, la description et l'état de la tâche.
* Le constructeur `GestionnaireTâches` initialise la liste des tâches avec trois tâches par défaut.
* Les méthodes `ajouterTâche`, `modifierTâche` et `supprimerTâche` permettent de gérer la liste des tâches.
* Les méthodes `récupérerToutesLesTâches` et `récupérerTâcheParId` permettent de récupérer les tâches de la liste.
* La méthode `filtrerTâchesParÉtat` permet de filtrer les tâches par leur état.

**Exemple d'utilisation :**

L'exemple d'utilisation montre comment instancier le gestionnaire de tâches, ajouter une nouvelle tâche, modifier une tâche existante, supprimer une tâche, récupérer toutes les tâches et filtrer les tâches par leur état.