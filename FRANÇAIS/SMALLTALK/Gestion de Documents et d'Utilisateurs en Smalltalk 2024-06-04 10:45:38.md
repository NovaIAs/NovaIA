**Code Smalltalk pour gérer des documents et des utilisateurs**

```smalltalk
Classe Document {
    variable nom
    variable texte

    initialisation: unNom {
        nom := unNom.
        texte := chaîneVide.
    }

    accèsNom {
        ^nom.
    }

    accèsTexte {
        ^texte.
    }

    mettreTexte: nouveauTexte {
        texte := nouveauTexte.
    }
}

Classe Utilisateur {
    variable nom
    variable documents

    initialisation: unNom {
        nom := unNom.
        documents := ensembleVide.
    }

    accèsNom {
        ^nom.
    }

    estAdministrateur {
        ^false.
    }

    ajouterDocument: unDocument {
        documents ajouter: unDocument.
    }

    documents {
        ^documents.
    }
}

Classe Administrateur extends: Utilisateur {
    surdefinition estAdministrateur {
        ^true.
    }

    créerDocument: unNom {
        nouveauDocument := Document initialiser: unNom.
        ajouterDocument: nouveauDocument.
        ^nouveauDocument.
    }
}

Classe GestionnaireDocuments {
    variable utilisateurs
    variable documents

    initialisation {
        utilisateurs := ensembleVide.
        documents := ensembleVide.
    }

    accéderUtilisateur: unNom {
        utilisateurExistant := utilisateurs trouverSiAbsent: [:unUtilisateur| unUtilisateur accèsNom = unNom] avec: [
            nouvelUtilisateur := Utilisateur initialiser: unNom.
            utilisateurs ajouter: nouvelUtilisateur.
            ^nouvelUtilisateur
        ].
        ^utilisateurExistant.
    }

    accéderDocument: unNom {
        documentExistant := documents trouverSiAbsent: [:unDocument| unDocument accèsNom = unNom] avec: [^nil].
        ^documentExistant.
    }

    créerUtilisateur: unNom {
        utilisateurExistant := accéderUtilisateur: unNom.
        utilisateurExistant estAdministrateur siVrai: [^utilisateurExistant].
        nouvelAdministrateur := Administrateur initialiser: unNom.
        utilisateurs ajouter: nouvelAdministrateur.
        ^nouvelAdministrateur.
    }

    créerDocument: unNom {
        administrateur := accéderUtilisateur: unNom.
        administrateur estAdministrateur siVrai: [
            ^administrateur créerDocument: unNom
        ].
        ^nil.
    }

    modifierDocument: unDocument texte: nouveauTexte {
        accèsDocument: unDocument accèsNom siNonNil: [
            unDocument mettreTexte: nouveauTexte
        ].
    }
}
```

**Explication du code:**

Ce code Smalltalk implémente un système de gestion de documents et d'utilisateurs.

* **Classe Document:** Représente un document avec un nom et un texte.
* **Classe Utilisateur:** Représente un utilisateur avec un nom et une collection de documents.
* **Classe Administrateur:** Hérite de la classe Utilisateur et représente un administrateur avec des privilèges supplémentaires.
* **Classe GestionnaireDocuments:** Gère les utilisateurs et les documents.

**Fonctionnalités:**

* Créer et gérer des documents
* Créer et gérer des utilisateurs
* Créer des administrateurs avec des privilèges spéciaux
* Permettre aux administrateurs de créer des documents
* Modifier le texte des documents