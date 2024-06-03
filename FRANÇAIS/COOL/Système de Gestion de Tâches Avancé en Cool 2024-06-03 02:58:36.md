**[Code COOL complexe]**

**Objectif:** Gérer un système de gestion des tâches avec des fonctionnalités avancées.

**Code:**

```cool
class Tâche {
    id: Integer;
    titre: String;
    description: String;
    créateur: User;
    assignée: User;
    dateCréation: Date;
    échéance: Date;
    priorité: String;
    état: Int;
    commentaire: StringList;
}

class ListeTâches {
    liste: List[Tâche];

    ajouter(tâche: Tâche) {
        liste.append(tâche);
    }

    supprimer(tâche: Tâche) {
        liste.remove(tâche);
    }

    trouver(critère: String): List[Tâche] {
        return liste.filter(tâche => tâche.titre.contains(critère) || tâche.description.contains(critère));
    }

    parPriorité(): List[Tâche] {
        return liste.sort_by(tâche => tâche.priorité);
    }

    parDateCréation(): List[Tâche] {
        return liste.sort_by(tâche => tâche.dateCréation);
    }
}

class GestionnaireTâches {
    listeTâches: ListeTâches;
    utilisateurs: List[Utilisateur];

    créerTâche(titre: String, description: String, créateur: User, échéance: Date, priorité: String): Tâche {
        return new Tâche(titre, description, créateur, échéance, priorité);
    }

    modifierTâche(tâche: Tâche, titre: String, description: String, échéance: Date, priorité: String) {
        tâche.titre = titre;
        tâche.description = description;
        tâche.échéance = échéance;
        tâche.priorité = priorité;
    }

    supprimerTâche(tâche: Tâche) {
        listeTâches.supprimer(tâche);
    }

    assignerTâche(tâche: Tâche, utilisateur: Utilisateur) {
        tâche.assignée = utilisateur;
    }

    ajouterCommentaire(tâche: Tâche, commentaire: String) {
        tâche.commentaire.append(commentaire);
    }

    changerStatut(tâche: Tâche, état: Int) {
        tâche.état = état;
    }

    ressourcesUtilisateur(utilisateur: Utilisateur): List[Tâche] {
        return listeTâches.liste.filter(tâche => tâche.créateur == utilisateur || tâche.assignée == utilisateur);
    }
}
```

**Explication:**

* **Classe Tâche:** Représente une tâche avec des attributs tels que le titre, la description, le créateur, etc.
* **Classe ListeTâches:** Gère une liste de tâches avec des opérations pour l'ajout, la suppression, la recherche, le tri, etc.
* **Classe GestionnaireTâches:** Interface principale pour créer, modifier, supprimer et gérer les tâches.
* **Classe Utilisateur:** (non fournie dans le code) Représente les utilisateurs du système.
* ** utilisation**

    * **Création de tâches:** `gestionnaireTâches.créerTâche()`
    * **Modification des tâches:** `gestionnaireTâches.modifierTâche()`
    * **Suppression des tâches:** `gestionnaireTâches.supprimerTâche()`
    * **Assignation des tâches:** `gestionnaireTâches.assignerTâche()`
    * **Ajout des commentaires:** `gestionnaireTâches.ajouterCommentaire()`
    * **Changement du statut:** `gestionnaireTâches.changerStatut()`
    * **Récupération des tâches de l'utilisateur:** `gestionnaireTâches.ressourcesUtilisateur()`