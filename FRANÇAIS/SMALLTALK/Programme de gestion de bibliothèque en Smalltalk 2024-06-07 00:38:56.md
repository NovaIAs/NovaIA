**Objectif du code :**

Créer un système de gestion de bibliothèque en Smalltalk.

**Code :**

```smalltalk
Classe ObjetBibliothèque {

    variable bibliothèque ;
    variable emprunts ;
    variable retours ;

    Constructeur {
        bibliothèque := Dictionnaire nouveau ;
        emprunteurs := Dictionnaire nouveau ;
        returns := Dictionnaire nouveau ;
    }

    AjouterLivre : unLivre {
        bibliothèque à : (unLivre nom) mettre : unLivre ;
        renvoyer unLivre
    }

    RetirerLivre : unLivre {
        renvoyer bibliothèque supprimer : (unLivre nom)
    }

    RechercherLivre : unNom {
        renvoyer bibliothèque à : unNom
    }

    EmprunterLivre : unLivre par : unEmprunteur {
        emprunteurs à : (unLivre nom) mettre : unEmprunteur ;
        returns supprimer : (unLivre nom) ;
        renvoyer unLivre
    }

    RetournerLivre : unLivre {
        returns à : (unLivre nom) mettre : unLivre ;
        emprunteurs supprimer : (unLivre nom) ;
        renvoyer unLivre
    }

    LivresEmpruntesPar : unEmprunteur {
        renvoyer emprunteurs clés dontValeurs est : unEmprunteur
    }

    LivresDispo {
        renvoyer bibliothèque clés dontValeurs nestPas : nil
    }
}

Classe ObjetLivre {

    variable nom ;
    variable auteur ;
    variable annee ;

    Constructeur : unNom par : unAuteur en : uneAnnee {
        nom := unNom ;
        auteur := unAuteur ;
        annee := uneAnnee ;
        renvoyer self
    }

    Nom accesseur {
        renvoyer nom
    }

    Auteur accesseur {
        renvoyer auteur
    }

    Annee accesseur {
        renvoyer annee
    }
}

Classe ObjetEmprunteur {

    variable nom ;
    variable adresse ;
    variable téléphone ;

    Constructeur : unNom avecAdresse : uneAdresse avecTéléphone : unTéléphone {
        nom := unNom ;
        adresse := uneAdresse ;
        téléphone := unTéléphone ;
        renvoyer self
    }

    Nom accesseur {
        renvoyer nom
    }

    Adresse accesseur {
        renvoyer adresse
    }

    Téléphone accesseur {
        renvoyer téléphone
    }
}

Classe Main {

    méthode main {
        | bibliothèque |
        bibliothèque := ObjetBibliothèque nouveau ;

        | livre1 |
        livre1 := ObjetLivre créer : 'Le Petit Prince' par : 'Antoine de Saint-Exupéry' en : 1943 ;
        bibliothèque ajouterLivre : livre1 ;

        | livre2 |
        livre2 := ObjetLivre créer : 'Harry Potter et la Pierre philosophale' par : 'J.K. Rowling' en : 1997 ;
        bibliothèque ajouterLivre : livre2 ;

        | emprunteur1 |
        emprunteur1 := ObjetEmprunteur créer : 'Jean Dupont' avecAdresse : '12 rue des Lilas' avecTéléphone : '06 12 34 56 78' ;

        | emprunteur2 |
        emprunteur2 := ObjetEmprunteur créer : 'Marie Martin' avecAdresse : '53 avenue de la République' avecTéléphone : '06 98 76 54 32' ;

        bibliothèque emprunterLivre : livre1 par : emprunteur1 ;
        bibliothèque emprunterLivre : livre2 par : emprunteur2 ;

        bibliothèque retournerLivre : livre1 ;

        transmissionEcran afficher: 'Livres empruntés par Jean Dupont : ', bibliothèque livresEmpruntesPar : emprunteur1 nouvelleLigne ;
        transmissionEcran afficher: 'Livres disponibles : ', bibliothèque livresDispo nouvelleLigne
    }
}

Main main
```

**Explication du code :**

* **Classe ObjetBibliothèque :** Représente la bibliothèque et gère les livres, les emprunteurs et les retours.
* **Classe ObjetLivre :** Représente un livre avec son nom, son auteur et son année de publication.
* **Classe ObjetEmprunteur :** Représente un emprunteur avec son nom, son adresse et son numéro de téléphone.
* **Classe Main :** Point d'entrée du programme.
* L'objet **bibliothèque** est créé pour gérer la collection de livres.
* Deux livres sont ajoutés à la bibliothèque.
* Deux emprunteurs sont créés et le premier emprunte le livre "Le Petit Prince".
* Le deuxième emprunteur emprunte le livre "Harry Potter et la Pierre philosophale".
* Le premier livre est retourné à la bibliothèque.
* Les livres empruntés par le premier emprunteur sont affichés sur l'écran.
* Les livres disponibles dans la bibliothèque sont également affichés sur l'écran.